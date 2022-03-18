module Ch5 where
    
import Prelude (Unit, (+), (-), (<), (>=), (/=), (==), show, discard, negate, type (~>))

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

import Data.EuclideanRing (mod)
import Data.HeytingAlgebra ((||))

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

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex f l = go 0 l where
  go _ Nil = Nothing
  go i (x : xs) = if f x
                  then Just i
                  else go (i + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex f l = go Nothing 0 l where
  go fi _ Nil = fi
  go fi i (x : xs) = go (if f x then Just i else fi) (i + 1) xs

reverse :: List ~> List
reverse l = go l Nil where
  go Nil ys = ys
  go (x : xs) ys = go xs (x : ys)

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
  log "test - index"
  log $ show $ index (1 : Nil) 4
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0
  log $ show $ index (1 : 2 : 3 : Nil) (-99)
  log "test - !!"
  log $ show $ (1 : 2 : 3 : Nil) !! 1
  log "test - findIndex"
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil)
  log $ show $ findIndex (10 /= _) (Nil :: List Int)
  log "test - findLastIndex"
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log "test - reverse"
  log $ show $ reverse (10 : 20 : 30 : Nil)
