{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies, GADTs, ConstraintKinds,
             UndecidableInstances, PolyKinds, RankNTypes #-}
-- | okay's programs-as-data for Haskell, TYPED BY THEIR EFFECTS
-- (hs-typed-effects, specs/remote-foreign.md). 'Okay.perform' names an
-- operation by a string and answers a 'Value'; here an effect is a GADT
-- of its operations, each typed by its argument and its answer, and a
-- program carries the effects it may perform in its type:
--
-- > data Shop a where
-- >   PriceOf :: String -> Shop Double
-- >
-- > priced :: String -> Integer -> Eff '[Shop] Double
-- > priced sku qty = do { p <- send (PriceOf sku); return (p * fromInteger qty) }
--
-- 'send' of an operation whose effect the program did not declare does not
-- COMPILE, and GHC says which effect is missing. The Scala side writes the
-- GADT and its 'Wire' instance from its callbacks' Schemas (@Hs.ops@), so
-- the row is written once, in Scala. Underneath it is 'Okay.Prog': the
-- wire, the worker and multi-shot continuations are unchanged.
module OkayEff
  ( Eff
  , Member
  , Wire (..)
  , FromValue (..)
  , ToValue (..)
  , send
  , runEff
  , program
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Okay (Prog, Value (..), perform)

-- | a program performing only operations of the effects @effs@
newtype Eff (effs :: [Type -> Type]) a = Eff (Prog a)

instance Functor (Eff effs) where
  fmap f (Eff p) = Eff (fmap f p)

instance Applicative (Eff effs) where
  pure = Eff . pure
  Eff f <*> Eff a = Eff (f <*> a)

instance Monad (Eff effs) where
  Eff p >>= f = Eff (p >>= \a -> let Eff q = f a in q)

-- | @op@ is one of @effs@; otherwise a type error naming the effect
type family Member (op :: Type -> Type) (effs :: [Type -> Type]) :: Constraint where
  Member op (op ': effs) = ()
  Member op (other ': effs) = Member op effs
  Member op '[] = TypeError ('Text "this program does not declare the effect " ':<>: 'ShowType op
                             ':$$: 'Text "add it to the program's Eff '[...] list, or handle it on the Scala side")

-- | how an effect's operations cross the okay wire: the name and the
-- arguments of a request, and the typed answer of a reply
class Wire (op :: Type -> Type) where
  request :: op a -> (String, [Value])
  reply :: op a -> Value -> a

-- | perform one operation of a declared effect
send :: (Member op effs, Wire op) => op a -> Eff effs a
send op = let (name, args) = request op in Eff (fmap (reply op) (perform name args))

-- | the untyped program underneath, for 'Okay.serve'
runEff :: Eff effs a -> Prog a
runEff (Eff p) = p

-- | a typed program as a served one: its answer written as a 'Value'
program :: ToValue a => Eff effs a -> Prog Value
program = fmap toValue . runEff

-- | a value read back from the wire; a mismatch is a Haskell error, which
-- the worker reports as a condition by name and lives on
class FromValue a where
  fromValue :: Value -> a

class ToValue a where
  toValue :: a -> Value

instance FromValue Value where fromValue = id
instance ToValue Value where toValue = id

instance FromValue Integer where
  fromValue (VInt n) = n
  fromValue (VDouble d) | d == fromInteger (round d) = round d
  fromValue v = error ("expected an integer, got " ++ show v)
instance ToValue Integer where toValue = VInt

instance FromValue Double where
  fromValue (VDouble d) = d
  fromValue (VInt n) = fromInteger n
  fromValue v = error ("expected a number, got " ++ show v)
instance ToValue Double where toValue = VDouble

instance FromValue Bool where
  fromValue (VBool b) = b
  fromValue v = error ("expected a boolean, got " ++ show v)
instance ToValue Bool where toValue = VBool

instance {-# OVERLAPPING #-} FromValue String where
  fromValue (VStr s) = s
  fromValue v = error ("expected a string, got " ++ show v)
instance {-# OVERLAPPING #-} ToValue String where toValue = VStr

instance FromValue a => FromValue [a] where
  fromValue (VList xs) = map fromValue xs
  fromValue v = error ("expected a list, got " ++ show v)
instance ToValue a => ToValue [a] where toValue = VList . map toValue

instance FromValue a => FromValue (Maybe a) where
  fromValue VNull = Nothing
  fromValue v = Just (fromValue v)
instance ToValue a => ToValue (Maybe a) where toValue = maybe VNull toValue
