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
-- Depends on base and containers only (both ship with GHC).
module Okay
  ( Prog (..)
  , Value (..)
  , done
  , perform
  , serve
  , shimVersion
  ) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Data.Char (chr, isDigit, isHexDigit, isSpace, ord)
import Data.IORef
import qualified Data.Map.Strict as M
import Numeric (readHex, showHex)
import System.IO

-- | the wire version this worker speaks; the host refuses any other
shimVersion :: Int
shimVersion = 6

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
  putStrLn (render (JObj [("shim", JNum (show shimVersion)), ("python", JStr "haskell")]))
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
          (Just (JStr "forget"), Just run) -> do
            modifyIORef' konts (M.filterWithKey (\(r, _) _ -> r /= run))
            return (Right JNull)
          (Just (JStr op), _) -> return (Left ("ValueError", "this Haskell worker serves programs only, not '" ++ op ++ "'"))
          _ -> return (Left ("ValueError", "not a request"))
        return (case ok of
          Right j -> JObj [("id", rid), ("ok", j)]
          Left (kind, msg) -> condition rid kind msg)
      loop = do
        eof <- isEOF
        if eof then return () else do
          line <- getLine
          case parseJson line of
            Just (JObj fs) -> do
              let rid = maybe JNull id (lookup "id" fs)
              -- a Haskell error (an 'error' call, a failed pattern) in the
              -- program is a CONDITION, and the worker lives on: the reply
              -- is rendered in full before a byte of it is written
              r <- try (answer rid fs >>= \j -> evaluate (let s = render j in length s `seq` s))
              putStrLn (case r of
                Right s -> s
                Left e -> render (condition rid "HaskellError" (displayException (e :: SomeException))))
            _ -> putStrLn (render (condition JNull "ValueError" "not a JSON request"))
          loop
  loop
