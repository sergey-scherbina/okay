-- | okay's programs-as-data for Haskell (remote-foreign,
-- specs/remote-foreign.md).
--
-- A Haskell program is written in 'Prog': 'done' answers, 'perform'
-- asks okay to run a named operation (a callback the Scala side
-- offered) and continues with its answer. A worker built with 'serve'
-- speaks the okay wire on stdin/stdout, so okay-py's engine
-- (@PySubprocess.speaking@) drives it exactly as it drives Python.
--
-- The continuation of a 'Perform' is an ordinary, PURE Haskell function.
-- The worker keeps it under an id until okay forgets the run, so okay may
-- continue it more than once: a @Choice@ handler on the Scala side makes
-- every branch, exactly - multi-shot across a process boundary.
--
-- Depends on base, containers, bytestring and text only (all ship with
-- GHC). The wire is JSON lines until the host configures CBOR
-- (polyglot-one-wire stage 5a); this worker has no DEFLATE - GHC ships no
-- zlib binding - and says so in its hello, so a host whose given
-- compression is Deflate is refused by name before a request is sent.
module Okay
  ( Prog (..)
  , Value (..)
  , done
  , perform
  , serve
  , shimVersion
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, isDigit, isHexDigit, isSpace, ord)
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import qualified Data.Map.Strict as M
import Numeric (readHex, showHex)
import System.IO

-- | the wire version this worker speaks; the host refuses any other
shimVersion :: Int
shimVersion = 8

-- | a value on the okay wire: what crosses between okay and Haskell
data Value
  = VNull
  | VBool Bool
  | VInt Integer
  | VDouble Double
  | VStr String
  | VList [Value]
  | VDict [(String, Value)]
  deriving (Eq, Show)

-- | a program as data: an answer, or a named operation and the pure
-- function that continues with its answer
data Prog a = Done a | Perform String [Value] (Value -> Prog a)

instance Functor Prog where
  fmap f (Done a) = Done (f a)
  fmap f (Perform n as k) = Perform n as (fmap f . k)

instance Applicative Prog where
  pure = Done
  pf <*> pa = pf >>= \f -> fmap f pa

instance Monad Prog where
  Done a >>= f = f a
  Perform n as k >>= f = Perform n as (\x -> k x >>= f)

done :: a -> Prog a
done = Done

-- | ask okay to run the operation named @n@ with these arguments
perform :: String -> [Value] -> Prog Value
perform n as = Perform n as Done

-- ------------------------------------------------------------------ JSON

data Json = JNull | JBool Bool | JNum String | JStr String | JArr [Json] | JObj [(String, Json)]

parseJson :: String -> Maybe Json
parseJson s = case value (skip s) of
  Just (j, rest) | all isSpace rest -> Just j
  _ -> Nothing
  where
    skip = dropWhile isSpace
    value ('n' : 'u' : 'l' : 'l' : r) = Just (JNull, skip r)
    value ('t' : 'r' : 'u' : 'e' : r) = Just (JBool True, skip r)
    value ('f' : 'a' : 'l' : 's' : 'e' : r) = Just (JBool False, skip r)
    value ('"' : r) = fmap (\(str, r') -> (JStr str, skip r')) (string r "")
    value ('[' : r) = array (skip r) []
    value ('{' : r) = object (skip r) []
    value r@(c : _) | c == '-' || isDigit c =
      let (num, r') = span (\x -> isDigit x || x `elem` "+-.eE") r in Just (JNum num, skip r')
    value _ = Nothing
    string ('"' : r) acc = Just (reverse acc, r)
    string ('\\' : c : r) acc = case c of
      'n' -> string r ('\n' : acc)
      't' -> string r ('\t' : acc)
      'r' -> string r ('\r' : acc)
      'b' -> string r ('\b' : acc)
      'f' -> string r ('\f' : acc)
      'u' | (h, r') <- splitAt 4 r, length h == 4, all isHexDigit h, [(n, "")] <- readHex h ->
              string r' (chr n : acc)
      _ -> string r (c : acc)
    string (c : r) acc = string r (c : acc)
    string [] _ = Nothing
    array (']' : r) acc = Just (JArr (reverse acc), skip r)
    array r acc = do
      (j, r') <- value r
      case r' of
        ',' : r'' -> array (skip r'') (j : acc)
        ']' : r'' -> Just (JArr (reverse (j : acc)), skip r'')
        _ -> Nothing
    object ('}' : r) acc = Just (JObj (reverse acc), skip r)
    object ('"' : r) acc = do
      (k, r1) <- string r ""
      case skip r1 of
        ':' : r2 -> do
          (v, r3) <- value (skip r2)
          case r3 of
            ',' : r4 -> object (skip r4) ((k, v) : acc)
            '}' : r4 -> Just (JObj (reverse ((k, v) : acc)), skip r4)
            _ -> Nothing
        _ -> Nothing
    object _ _ = Nothing

render :: Json -> String
render JNull = "null"
render (JBool b) = if b then "true" else "false"
render (JNum n) = n
render (JStr s) = '"' : concatMap esc s ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc c | ord c < 0x20 = "\\u" ++ replicate (4 - length h) '0' ++ h where h = showHex (ord c) ""
    esc c = [c]
render (JArr xs) = "[" ++ commas (map render xs) ++ "]"
render (JObj fs) = "{" ++ commas [render (JStr k) ++ ":" ++ render v | (k, v) <- fs] ++ "}"

commas :: [String] -> String
commas [] = ""
commas [x] = x
commas (x : xs) = x ++ "," ++ commas xs

-- ------------------------------------------------------------------ CBOR
-- The wire's subset (RFC 8949): integers, floats (half, single, double),
-- text, definite arrays and maps with text keys, true/false/null/undefined.
-- The tree is the same one JSON carries, escapes included.

cborEncode :: Json -> BB.Builder
cborEncode j = case j of
  JNull -> BB.word8 0xf6
  JBool b -> BB.word8 (if b then 0xf5 else 0xf4)
  JNum n
    | not (null n), all (\c -> isDigit c || c == '-') n ->
        let i = read n :: Integer in if i >= 0 then hd 0 i else hd 1 (-1 - i)
    | otherwise -> BB.word8 0xfb <> BB.doubleBE (readDouble n)
  JStr s -> let b = TE.encodeUtf8 (T.pack s) in hd 3 (toInteger (B.length b)) <> BB.byteString b
  JArr xs -> hd 4 (toInteger (length xs)) <> mconcat (map cborEncode xs)
  JObj fs -> hd 5 (toInteger (length fs)) <> mconcat [cborEncode (JStr k) <> cborEncode v | (k, v) <- fs]
  where
    hd :: Word8 -> Integer -> BB.Builder
    hd major n
      | n < 24 = BB.word8 (m .|. fromInteger n)
      | n < 0x100 = BB.word8 (m .|. 24) <> BB.word8 (fromInteger n)
      | n < 0x10000 = BB.word8 (m .|. 25) <> BB.word16BE (fromInteger n)
      | n < 0x100000000 = BB.word8 (m .|. 26) <> BB.word32BE (fromInteger n)
      | otherwise = BB.word8 (m .|. 27) <> BB.word64BE (fromInteger n)
      where m = major `shiftL` 5

cborDecode :: B.ByteString -> Either String Json
cborDecode bs = case item bs of
  Right (j, rest) | B.null rest -> Right j
                  | otherwise -> Left (show (B.length rest) ++ " bytes after a CBOR message")
  Left e -> Left e
  where
    short = Left "a CBOR message ended early (cut short?)"
    take' n b = if B.length b < n then short else Right (B.splitAt n b)
    be :: B.ByteString -> Word64
    be = B.foldl' (\a w -> a `shiftL` 8 .|. fromIntegral w) 0
    item b = case B.uncons b of
      Nothing -> short
      Just (ib, r) -> do
        let major = ib `shiftR` 5
            info = ib .&. 0x1f
        if major == 7 then simple info r else do
          (n, r1) <- case info of
            _ | info < 24 -> Right (fromIntegral info :: Word64, r)
            24 -> fmap (\(h, t) -> (be h, t)) (take' 1 r)
            25 -> fmap (\(h, t) -> (be h, t)) (take' 2 r)
            26 -> fmap (\(h, t) -> (be h, t)) (take' 4 r)
            27 -> fmap (\(h, t) -> (be h, t)) (take' 8 r)
            _ -> Left ("an indefinite or reserved CBOR length (" ++ show info ++ ") is not in the wire's subset")
          case major of
            0 -> Right (JNum (show n), r1)
            1 -> Right (JNum (show (-1 - toInteger n)), r1)
            3 -> do
              (t, r2) <- take' (fromIntegral n) r1
              case TE.decodeUtf8' t of
                Right txt -> Right (JStr (T.unpack txt), r2)
                Left _ -> Left "CBOR text that is not UTF-8"
            4 -> many n r1 [] >>= \(xs, r2) -> Right (JArr xs, r2)
            5 -> pairs n r1 []
            _ -> Left ("CBOR major type " ++ show major ++ " (byte strings, tags) is not in the wire's subset")
    many :: Word64 -> B.ByteString -> [Json] -> Either String ([Json], B.ByteString)
    many 0 r acc = Right (reverse acc, r)
    many n r acc = item r >>= \(j, r') -> many (n - 1) r' (j : acc)
    pairs :: Word64 -> B.ByteString -> [(String, Json)] -> Either String (Json, B.ByteString)
    pairs 0 r acc = Right (JObj (reverse acc), r)
    pairs n r acc = do
      (k, r1) <- item r
      (v, r2) <- item r1
      case k of
        JStr ks -> pairs (n - 1) r2 ((ks, v) : acc)
        _ -> Left "a CBOR map key that is not text"
    simple :: Word8 -> B.ByteString -> Either String (Json, B.ByteString)
    simple info r = case info of
      20 -> Right (JBool False, r)
      21 -> Right (JBool True, r)
      22 -> Right (JNull, r)
      23 -> Right (JNull, r)
      25 -> take' 2 r >>= \(h, t) -> Right (float (half (fromIntegral (be h))), t)
      26 -> take' 4 r >>= \(h, t) -> Right (float (realToFrac (castWord32ToFloat (fromIntegral (be h)))), t)
      27 -> take' 8 r >>= \(h, t) -> Right (float (castWord64ToDouble (be h)), t)
      _ -> Left ("CBOR simple value " ++ show info ++ " is not in the wire's subset")
    -- a float read from CBOR becomes the JSON tree's number; the wire's
    -- escapes ("nan", "f") are what carry the cases JSON cannot say
    float :: Double -> Json
    float d
      | isNaN d = JObj [("t", JStr "nan")]
      | otherwise = JNum (show d)
    half :: Word16 -> Double
    half h =
      let e = fromIntegral ((h `shiftR` 10) .&. 0x1f) :: Int
          m = fromIntegral (h .&. 0x3ff) :: Double
          v | e == 0 = m * 2 ** (-24)
            | e == 31 = if m == 0 then 1 / 0 else 0 / 0
            | otherwise = (m + 1024) * 2 ^^ (e - 25)
      in if h .&. 0x8000 /= 0 then negate v else v

-- --------------------------------------------------- the okay wire's values

exact :: Integer
exact = 2 ^ (53 :: Int)

enc :: Value -> Json
enc VNull = JNull
enc (VBool b) = JBool b
enc (VInt n)
  | abs n < exact = JNum (show n)
  | otherwise = JObj [("t", JStr "int"), ("v", JStr (show n))]
enc (VDouble d)
  | isNaN d = JObj [("t", JStr "nan")]
  | d == fromInteger (round d) && abs d < 1e15 = JObj [("t", JStr "f"), ("v", JNum (show d))]
  | otherwise = JNum (show d)
enc (VStr s) = JStr s
enc (VList xs) = JArr (map enc xs)
enc (VDict kv) = JObj [("t", JStr "dict"), ("kv", JArr [JArr [JStr k, enc v] | (k, v) <- kv])]

dec :: Json -> Value
dec JNull = VNull
dec (JBool b) = VBool b
dec (JNum n)
  | all (\c -> isDigit c || c == '-') n = VInt (read n)
  | otherwise = VDouble (readDouble n)
dec (JStr s) = VStr s
dec (JArr xs) = VList (map dec xs)
dec (JObj fs) = case lookup "t" fs of
  Just (JStr "nan") -> VDouble (0 / 0)
  Just (JStr "f") | Just (JNum n) <- lookup "v" fs -> VDouble (readDouble n)
  Just (JStr "int") | Just (JStr d) <- lookup "v" fs -> VInt (read d)
  Just (JStr "dict") | Just (JArr ps) <- lookup "kv" fs -> VDict [(k, dec v) | JArr [JStr k, v] <- ps]
  _ -> VDict [(k, dec v) | (k, v) <- fs]

-- | JSON's numbers as Haskell reads them ("1e-5" wants a mantissa point)
readDouble :: String -> Double
readDouble n = read (fixup n)
  where
    fixup s = let (m, e) = break (`elem` "eE") s
                  m' = if '.' `elem` m then m else m ++ ".0"
                  m'' = if take 1 m' == "." then '0' : m' else m'
              in m'' ++ e

-- ---------------------------------------------------------------- the worker

-- | the worker's main loop: named programs, served on stdin/stdout
serve :: [(String, [Value] -> Prog Value)] -> IO ()
serve progs = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  konts <- newIORef (M.empty :: M.Map (Integer, Integer) (Value -> Prog Value))
  next <- newIORef (0 :: Integer)
  -- the format this worker is speaking: "json" (lines) until a configure
  cbor <- newIORef False
  putStrLn (render (JObj [("shim", JNum (show shimVersion)), ("python", JStr "haskell"),
                          ("speaks", JObj [("format", JArr [JStr "json", JStr "cbor"]), ("compress", JArr [])])]))
  let node run p = case p of
        Done v -> return (JObj [("done", enc v)])
        Perform n as k -> do
          i <- atomicModifyIORef' next (\x -> (x + 1, x + 1))
          modifyIORef' konts (M.insert (run, i) k)
          return (JObj [("perform", JStr n), ("args", JArr (map enc as)), ("k", JNum (show i))])
      int j = case j of
        Just (JNum n) -> Just (read (takeWhile (/= '.') n) :: Integer)
        _ -> Nothing
      condition rid kind msg =
        JObj [("id", rid), ("condition", JObj [("kind", JStr kind), ("message", JStr msg)])]
      answer rid fs = do
        ok <- case (lookup "op" fs, int (lookup "run" fs)) of
          (Just (JStr "program"), Just run) -> case lookup "fn" fs of
            Just (JStr fn) | Just f <- lookup fn progs -> do
              let args = case lookup "args" fs of { Just (JArr xs) -> map dec xs; _ -> [] }
              Right <$> node run (f args)
            Just (JStr fn) -> return (Left ("LookupError", "no program named '" ++ fn ++ "' in this worker"))
            _ -> return (Left ("ValueError", "a program request names its fn"))
          (Just (JStr "continue"), Just run) -> case int (lookup "k" fs) of
            Just k -> do
              m <- readIORef konts
              case M.lookup (run, k) m of
                Just f -> Right <$> node run (f (maybe VNull dec (lookup "answer" fs)))
                Nothing -> return (Left ("LookupError", "continuation " ++ show k ++ " of run "
                                    ++ show run ++ " is not held here (forgotten, or another process)"))
            Nothing -> return (Left ("ValueError", "a continue names its k"))
          (Just (JStr "configure"), _) -> case (lookup "format" fs, lookup "compress" fs) of
            (Just (JStr f), Just (JStr "none")) | f == "json" || f == "cbor" ->
              return (Right (JObj [("format", JStr f), ("compress", JStr "none")]))
            (Just (JStr f), Just (JStr c)) ->
              return (Left ("ValueError", "this Haskell worker speaks the formats json, cbor and no compression; not "
                                           ++ f ++ " with " ++ c))
            _ -> return (Left ("ValueError", "a configure names its format and compression"))
          (Just (JStr "forget"), Just run) -> do
            modifyIORef' konts (M.filterWithKey (\(r, _) _ -> r /= run))
            return (Right JNull)
          (Just (JStr op), _) -> return (Left ("ValueError", "this Haskell worker serves programs only, not '" ++ op ++ "'"))
          _ -> return (Left ("ValueError", "not a request"))
        return (case ok of
          Right j -> JObj [("id", rid), ("ok", j)]
          Left (kind, msg) -> condition rid kind msg)
      -- one request: a JSON line, or (after a configure) a CBOR frame
      request = do
        framed <- readIORef cbor
        eof <- isEOF
        if eof then return Nothing else if not framed then Just . maybe (Left "not a JSON request") Right . parseJson <$> getLine else do
          len <- B.hGet stdin 4
          if B.length len < 4 then return Nothing else do
            let n = B.foldl' (\a w -> a `shiftL` 8 .|. fromIntegral w) (0 :: Word32) len
            body <- B.hGet stdin (fromIntegral n)
            if B.length body < fromIntegral n then return Nothing else return (Just (cborDecode body))
      -- a reply is built in full before a byte of it is written
      write j = do
        framed <- readIORef cbor
        if not framed then evaluate (let s = render j in length s `seq` s) >>= putStrLn else do
          body <- evaluate (BL.toStrict (BB.toLazyByteString (cborEncode j)))
          BB.hPutBuilder stdout (BB.word32BE (fromIntegral (B.length body)) <> BB.byteString body)
          hFlush stdout
      loop = do
        req <- request
        case req of
          Nothing -> return ()
          Just (Right (JObj fs)) -> do
            let rid = maybe JNull id (lookup "id" fs)
            -- a Haskell error (an 'error' call, a failed pattern) in the
            -- program is a CONDITION, and the worker lives on
            r <- try (answer rid fs >>= \j -> evaluate (forceJson j))
            case r of
              Right j -> do
                write j
                -- a configure takes effect AFTER its own answer
                case (lookup "op" fs, j) of
                  (Just (JStr "configure"), JObj rs) | Just (JObj ok) <- lookup "ok" rs, Just (JStr "cbor") <- lookup "format" ok -> do
                    hSetBinaryMode stdin True
                    hSetBinaryMode stdout True
                    hSetBuffering stdout (BlockBuffering Nothing)
                    writeIORef cbor True
                  _ -> return ()
              Left e -> write (condition rid "HaskellError" (displayException (e :: SomeException)))
          Just (Right _) -> write (condition JNull "ValueError" "not a request")
          Just (Left msg) -> write (condition JNull "ValueError" msg)
        case req of
          Nothing -> return ()
          _ -> loop
  loop

-- | the whole tree, evaluated: an 'error' inside a reply surfaces here,
-- before any of it is written
forceJson :: Json -> Json
forceJson j = length (render j) `seq` j
