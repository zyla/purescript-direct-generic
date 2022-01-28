module Data.Generic.Direct where

import Prelude

import Data.Generic.Direct.Constructor (Constructor, constructorName)
import Data.Generic.Direct.Constructor as C
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))
import Type.Row as Row

class Generic a (rep :: Row List) | a -> rep where
  constructors :: Constructors a

newtype Constructors (a :: Type) = Constructors (FO.Object Constructor)

toConstructors :: forall a. Array Constructor -> Constructors a
toConstructors = Constructors <<< FO.fromFoldable <<< map (\c -> Tuple (constructorName c) c)

construct :: forall label a args rest rep. Generic a rep
  => IsSymbol label
  => Row.Cons label args rest rep
  => Proxy label
  -> Builder args a
construct labelP =
  let Constructors cs = constructors :: Constructors a
      label = reflectSymbol labelP
  in case FO.lookup label cs of
       Nothing ->
         unsafeCrashWith $ "Constructor " <> label <> " not found"
       Just con ->
         Builder con 0 identity

data List

foreign import data Nil :: List
foreign import data Cons :: Type -> List -> List

data Builder (args :: List) a = Builder Constructor Int (a -> a)

build :: forall a. Builder Nil a -> a
build (Builder con _ fn) = fn $ C.new con []

addArgument :: forall r x xs. x -> Builder xs r -> Builder (Cons x xs) r
addArgument x (Builder con offset fn) = Builder con (offset + 1) (unsafeSetArgument offset x <<< fn)

foreign import unsafeSetArgument :: forall a b. Int -> a -> b -> b


