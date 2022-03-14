module Ch5 where
    
import Prelude (Unit, (+), show, discard)

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil y = y : Nil
snoc (x : xs) y = x : snoc xs y

length :: ∀ a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go :: ∀ b. List b -> List b
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs}

test :: Effect Unit
test = do
  log "test - const"
  log $ show $ flip const 1 2
  log "test - flip"
  flip const 1 2 # show # log
  log "test - singleton"
  log $ show $ singleton "xyz"
  log "test - null"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log "test - snoc"
  log $ show $ snoc (1 : 2 : Nil) 3
  log "test - length"
  log $ show $ length $ 1 : 2 : 3 : Nil
  log "test - head"
  log $ show (head Nil :: Maybe Unit)
  log $ show $ head ("abc" : "123" : Nil)
  log "test - tail"
  log $ show $ (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abc" : "123" : Nil)
  log "test - last"
  log $ show $ (last Nil :: Maybe Unit)
  log $ show $ last ("a" : "b" : "c" : Nil)
  log "test - init"
  log $ show $ (init Nil :: Maybe (List Unit))
  log $ show $ init (1 : Nil)
  log $ show $ init (1 : 2 : Nil)
  log $ show $ init (1 : 2 : 3 : Nil)
  log "test - uncons"
  log $ show $ uncons (1 : 2 : 3 : Nil)
