module Ch5 where
    
import Prelude (Unit, (+), (-), (<), (>=), (/=), (==), show, discard, negate, type (~>), (>), otherwise, (<<<), max, (>>>))

import Effect (Effect)
import Effect.Console (log)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd, fst)

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

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil
concat (Nil : ls) = concat ls
concat ((x : xs) : ls) = x : concat (xs : ls)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter predicate (x : xs) = if predicate x
                            then x : filter predicate xs
                            else filter predicate xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

range :: Int -> Int -> List Int
range start target | start == target = singleton start
                   | otherwise = start : range (start + if start < target then 1 else (-1)) target

range2 :: Int -> Int -> List Int
range2 start end = go (if start < end then 1 else (-1)) start where
  go step start' | start' == end = singleton start'
                | otherwise = start' : go step (start' + step)

range3 :: Int -> Int -> List Int
range3 start end = go start where
  go start' | start' == end = singleton start'
            | otherwise = start' : go (start' + step)
  step = if start < end then 1 else (-1)

range4 :: Int -> Int -> List Int
range4 start end = go Nil start where
  go rl start' | start' == end = start' : rl
               | otherwise = go (start' : rl) (start' + step)
  step = if start < end then 1 else (-1)

range5 :: Int -> Int -> List Int
range5 start end = go Nil end start where
  go rl start' end' | start' == end' = start' : rl
                    | otherwise = go (start' : rl) (start' + step) end'
  step = if start < end then (-1) else 1

take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n) where
  go nl _ Nil = nl
  go nl 0 _ = nl
  go nl n' (x : xs) = go (x : nl) (n' - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil
drop 0 xs = xs
drop i (_ : xs) = drop (i - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile pred = reverse <<< go Nil where
  go nl Nil = nl
  go nl (x : xs) | pred x = go (x : nl) xs
                 | otherwise = nl

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l @ (x : xs) | pred x = dropWhile pred xs
                            | otherwise = l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs # \(Tuple n' l') -> Tuple (n' + 1) (if n' < n then x : l' else l')

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
  log "test - concat"
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : (Nil))
  log "test - filter"
  log $ show $ filter (4 > _) (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log "test - catMaybes"
  log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  log "test - range"
  log $ show $ range 1 10
  log $ show $ range 3 (-3)
  log "test - range2"
  log $ show $ range2 1 10
  log $ show $ range2 3 (-3)
  log "test - range3"
  log $ show $ range3 1 10
  log $ show $ range3 3 (-3)
  log "test - range4"
  log $ show $ range4 1 10
  log $ show $ range4 3 (-3)
  log "test - range5"
  log $ show $ range5 1 10
  log $ show $ range5 3 (-3)
  log "test - take"
  log $ show $ take 5 (12 : 13 : 14 : Nil)
  log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  log "test - drop"
  log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  log $ show $ drop 10 (Nil :: List Unit)
  log "test - takeWhile"
  log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "test - dropWhile"
  log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  log "test - takeEnd"
  log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)
