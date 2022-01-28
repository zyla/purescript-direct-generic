module Data.Generic.Direct.Constructor where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Any :: Type

toAny :: forall a. a -> Any
toAny = unsafeCoerce

unsafeFromAny :: forall a. Any -> a
unsafeFromAny = unsafeCoerce

-- | Represents a PureScript data constructor.
foreign import data Constructor :: Type

-- | Get the constructor used to construct the given value.
-- | Unsafe: only guaranteed to work for PS sum types
foreign import getConstructor :: forall a. a -> Constructor

foreign import isInstanceOf :: forall a. Constructor -> a -> Boolean

foreign import new :: forall a. Constructor -> Array Any -> a

instance Eq Constructor where eq = unsafeRefEq

constructorName :: Constructor -> String
constructorName con = (unsafeCoerce con :: { name :: String }).name

class ToConstructor a where
  toConstructor :: a -> Constructor

instance ToConstructor b => ToConstructor (a -> b) where
  toConstructor f = toConstructor (f dummy)
    where dummy = unsafeCoerce {}
else instance ToConstructor a where
  toConstructor = getConstructor
