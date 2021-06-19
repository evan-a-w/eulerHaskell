module Probs where

import qualified Data.List.Ordered as LO
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Numbers.Primes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sort
import Data.Char
import Data.List
import Data.Ratio
import qualified Math.NumberTheory.Primes as T

splitString :: String -> Char -> [String]
splitString str d = map reverse $ reverse  $ sStr str d "" []

sStr :: String -> Char -> String -> [String] -> [String]
sStr "" d cs l = cs:l
sStr (x:xs) d cs l = if x == d && cs /= "" 
                        then sStr xs d "" (cs:l)
                        else if x == d 
                            then sStr xs d cs l
                        else sStr xs d (x:cs) l

sol1 :: Int
sol1 = sum [x | x <- takeWhile (<1000) $ [3,6..] `LO.union` [5,10..]]

fibs :: Int -> Vector Integer
fibs n = case n of
           0         -> V.fromList ([1] :: [Integer])
           1         -> V.fromList ([1,1] :: [Integer])
           otherwise -> let bs = fibs $ n-1 in
                            V.snoc bs (bs!(n-1)+bs!(n-2))

fibStream :: [Integer]
fibStream = 1 : 1 : zipWith (+) fibStream (tail fibStream)

sol2 :: Integer
sol2 = sum $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<4000000) fibStream

sol3 :: Integer
sol3 = maximum $ primeFactors 600851475143

sol3' :: Integer
sol3' = maximum $ mp primes 600851475143 1
  where mp (x:xs) n p = if p == n
                             then []
                             else if n `mod` x == 0
                               then x : mp xs n (p*x)
                               else mp xs n p

sol4 :: Int
sol4 = maximum $ filter isPali $ [x*y | x <- [100..999], y <- [100..999]]
  where isPali :: Int -> Bool
        isPali i = (reverse . show) i  == show i

sol4' :: Int
sol4' = head $ dropWhile (not . isPali) $ reverse $ sort [x*y | x <- [100..999], y <- [100..999]]
  where isPali :: Int -> Bool
        isPali i = (reverse . show) i  == show i

sol5 :: Integer
sol5 = foldr lcm 1 [1..20]

sol6 :: Integer
sol6 = (sum [1..100])^2 - sum [x^2 | x <- [1..100]]

sol7 = primes !! 10000

sol8_num :: Integer
sol8_num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

integerToList :: Integer -> [Int]
integerToList i = go i []
  where go 0 acc = acc
        go x acc = go (x`div`10) (fromIntegral (x`mod`10) : acc)

sol8 :: Integer
sol8 = go (integerToList sol8_num) [] 0
  where prod = foldr (\x y -> y * fromIntegral x) 1 
        go xs acc p = if length xs < 13
                           then p
                           else let t = take 13 xs
                                    d = drop 1 xs
                                    np = prod t in
                                    if np > p
                                      then go d t np
                                      else go d acc p

isInteger x = (fromIntegral . round) x == x
dSquare = fromIntegral . (^2)

pythTrips :: [(Int, Int, Int)]
pythTrips = [(x,y, (round . sqrt) $ dSquare x + dSquare y) 
            | x <- [1..1000], y <- [1..1000],
              isInteger (sqrt $ dSquare x + dSquare y)]

sol9 = (\(x,y,z)->x*y*z) $ head $ dropWhile pred pythTrips
  where pred (x,y,z) = x+y+z /= 1000

sol10 = sum $ takeWhile (<2000000) primes

grid11 :: Vector (Vector Integer)
grid11 = 
  V.fromList $ map V.fromList
    [ [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08]
    , [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00]
    , [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65]
    , [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91]
    , [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80]
    , [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50]
    , [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70]
    , [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21]
    , [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72]
    , [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95]
    , [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92]
    , [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57]
    , [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58]
    , [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40]
    , [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66]
    , [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69]
    , [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36]
    , [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16]
    , [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54]
    , [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]

sol11 = undefined

divisors :: Integer -> [Integer]
divisors n = 1 : go n 2 (ceiling $ sqrt $ fromIntegral n)
  where go :: Integer -> Integer -> Integer -> [Integer]
        go x c end = if c >= end
                            then [x]
                            else if x `mod` c == 0
                              then c : (x `div` c : go x (c+1) end)
                              else go x (c+1) end

numDivisors :: Integer -> Int
numDivisors n = go n 2 (ceiling $ sqrt $ fromIntegral n)
  where go :: Integer -> Integer -> Integer -> Int
        go x c end = if c >= end
                            then 0
                            else if x `mod` c == 0
                              then 2 + go x (c+1) end
                              else go x (c+1) end

triangleNumbers :: Integral a => [a]
triangleNumbers = scanl (+) 1 [2..]

sol12 = head $ dropWhile pred triangleNumbers
  where pred i = numDivisors i <= 500

collatzChain :: Int -> Int
collatzChain i = go i 1
  where go 1 c = c 
        go y c = if y `mod` 2 == 0
                    then go (y `div` 2) (c+1)
                    else go (3*y + 1) (c+1)

insertOverList :: [Int] -> Int -> Map Int Int -> Map Int Int
insertOverList xs v m = snd $ foldr (\x (v',m') -> (v'-1, Map.insert x v' m')) (v,m) xs

collatzChainMap :: Int -> Map Int Int -> (Int, Map Int Int)
collatzChainMap i map = go i map 1 []
  where go 1 m c xs = (c, insertOverList xs c m)
        go y m c xs = case Map.lookup y m of
                          Just v  -> (c+v, insertOverList xs (c+v) m)
                          Nothing -> if y `mod` 2 == 0
                                         then go (y `div` 2) m (c+1) (y:xs)
                                         else go (3*y + 1) m (c+1) (y:xs)

sol13 = undefined

sol14 :: Int
sol14 = go [1..999999] Map.empty 0
  where unWrap (Just x) = x
        unWrap Nothing = 0
        go :: [Int] -> Map Int Int -> Int -> Int
        go [] _ max = max
        go (x:xs) map max = case Map.lookup x map of
                              Nothing -> let (res, nm) = collatzChainMap x map in
                                              if res > unWrap (Map.lookup max nm)
                                                then go xs nm x
                                                else go xs nm max
                              Just v  -> if v > unWrap (Map.lookup max map)
                                            then go xs map x
                                            else go xs map max

sol14' = go 2 0
  where go 1000000 m = m
        go i m = let c = collatzChain i in
                     if c > m
                        then go (i+1) c
                        else go (i+1) m

grid :: [[Integer]]
grid = [ [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
       , [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
       ]

vGrid :: Vector (Vector Integer)
vGrid = V.fromList $ map V.fromList grid

set :: Integer -> Int -> Int -> Vector (Vector Integer) -> Vector (Vector Integer)
set val y x v = v // [(y,((v!y) // [(x,val)]))]

movements :: Int -> Int -> Vector (Vector Integer) -> (Integer, Vector (Vector Integer))
movements y x arr = if val /= 0
                       then (val, arr)
                       else let (r1, arr1) = movements (y+1) x arr
                                (r2, arr2) = movements y (x+1) arr1
                                res = r1 + r2 in
                                  (res, set res y x arr2)
  where val = (arr!y)!x

-- Memoized
sol15 = fst $ movements 0 0 vGrid

sol16 = sum $ integerToList (2^1000)

oneToNine = V.fromList
            ["one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine"]
tenToTwenty = V.fromList
              ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
               "seventeen", "eighteen", "nineteen"]
tenToNinety = V.fromList
              ["ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
               "eighty", "ninety"]

numToWord :: Integer -> String
numToWord i = listWord $ integerToList i
  where listWord l =
              case length l of
                1         -> oneToNine ! ((l!!0)-1)
                2         -> if l!!1 == 0
                                then tenToNinety ! ((l!!0)-1)
                                else if l!!0 /= 1
                                     then tenToNinety ! ((l!!0)-1) ++ " " ++ 
                                          oneToNine ! ((l!!1)-1)
                                     else tenToTwenty ! ((l!!1)-1)
                3         -> let b = oneToNine ! ((l!!0)-1) ++ " hundred" in
                              if l!!1 == 0 && l!!2 == 0
                                 then b
                                 else b ++ " and " ++ listWord (if l!!1 == 0
                                                        then [l!!2]
                                                        else [l!!1, l!!2])
                otherwise -> "one thousand"

wordToNine   = V.fromList [0,3,3,5,4,4,3,5,5,4]
wordToTwenty = V.map (fromIntegral . length) tenToTwenty
wordToNinety = V.fromList [0,3,6,6,5,5,5,7,6,6]

countLetters :: Integer -> Integer
countLetters i = listWord $ integerToList i
  where listWord l =
              case length l of
                1         -> wordToNine ! (l!!0)
                2         -> if l!!1 == 0
                                then wordToNinety ! (l!!0)
                                else if l!!0 /= 1
                                     then wordToNinety ! (l!!0) + 
                                          wordToNine ! (l!!1)
                                     else wordToTwenty ! ((l!!1)-1)
                3         -> let b = (wordToNine ! (l!!0)) + 7 in
                              if l!!1 == 0 && l!!2 == 0
                                 then b
                                 else b + 3 + listWord [l!!1, l!!2]
                otherwise -> 11

sol17 = sum $ map countLetters [1..1000]
-- compiler is amazing... sol17' is just as fast
sol17' = sum $ map (sum . (map length) . words . numToWord) [1..1000]

tri :: Vector (Vector Integer)
tri = V.fromList $ map V.fromList 
                 [ [75]
                 , [95, 64]
                 , [17, 47, 82]
                 , [18, 35, 87, 10]
                 , [20, 04, 82, 47, 65]
                 , [19, 01, 23, 75, 03, 34]
                 , [88, 02, 77, 73, 07, 63, 67]
                 , [99, 65, 04, 28, 06, 16, 70, 92]
                 , [41, 41, 26, 56, 83, 40, 80, 70, 33]
                 , [41, 48, 72, 33, 47, 32, 37, 16, 94, 29]
                 , [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14]
                 , [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57]
                 , [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48]
                 , [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31]
                 , [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23] ]

startMap :: Map (Int, Int) Integer 
startMap = Map.fromList [((i,14), (tri!14)!i) | i <- [0..14]]

maximumPathSum :: (Int, Int) -> Map (Int, Int) Integer -> Integer
maximumPathSum (x,y) m = fst $ go (x,y) m
  where go (x, 14) m = ((tri!14)!x, m)
        go (x, 13) m = let cv = (tri!13)!x
                           fs = (tri!14)!x
                           sn = (tri!14)!(x+1) in
                             if fs > sn
                                then (cv+fs, Map.insert (x,13) (cv+fs) m)
                                else (cv+sn, Map.insert (x,13) (cv+sn) m)
        go (x,y) m = case Map.lookup (x,y) m of
                       Just v  -> (v, m)
                       Nothing -> let cv = (tri!y)!x
                                      (p1, m1) = go (x,y+1) m
                                      (p2, m2) = go (x+1,y+1) m1 in
                                    if p2 > p1
                                       then (cv+p2, Map.insert (x,y) (cv+p2) m2)
                                       else (cv+p1, Map.insert (x,y) (cv+p1) m2)

sol18 = maximumPathSum (0,0) startMap

isLeapYear :: Integral a => a -> Bool
isLeapYear y
  | y `mod` 100 == 0 = if y `mod` 400 == 0
                          then True
                          else False
  | y `mod` 4 == 0   = True
  | otherwise        = False

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Eq, Show, Ord)
data WeekDay = Mon | Tue | Wed | Thur | Fri | Sat | Sun
  deriving (Eq, Ord, Show)

instance Enum Month where
  toEnum i = case i of
               1         -> Jan
               2         -> Feb
               3         -> Mar
               4         -> Apr
               5         -> May
               6         -> Jun
               7         -> Jul
               8         -> Aug
               9         -> Sep
               10        -> Oct
               11        -> Nov
               12        -> Dec
               otherwise -> toEnum ((i `mod` 12) + 1)
  fromEnum d = case d of
                 Jan -> 1
                 Feb -> 2
                 Mar -> 3
                 Apr -> 4
                 May -> 5
                 Jun -> 6
                 Jul -> 7
                 Aug -> 8
                 Sep -> 9
                 Oct -> 10
                 Nov -> 11
                 Dec -> 12

instance Enum WeekDay where
  toEnum i = case i of
               0         -> Sun
               1         -> Mon
               2         -> Tue
               3         -> Wed
               4         -> Thur
               5         -> Fri
               6         -> Sat
               7         -> Sun
               otherwise -> toEnum (i `mod` 7)
  fromEnum d = case d of
                 Mon  -> 1
                 Tue  -> 2
                 Wed  -> 3
                 Thur -> 4
                 Fri  -> 5
                 Sat  -> 6
                 Sun  -> 7

type Year = Int
type Day = Int

data Date = Date Day Month Year WeekDay
  deriving (Eq, Ord, Show)

next :: Date -> Date
next (Date d m y wd) = if d + 1 > days m y
                          then if m == Dec
                                  then Date 1 Jan (y+1) (succ wd)
                                  else Date 1 (succ m) y (succ wd)
                          else Date (d+1) m y (succ wd)

days :: Month -> Year -> Int
days m y
  | m == Feb                    = if isLeapYear y
                                     then 29
                                     else 28
  | elem m [Sep, Apr, Jun, Nov] = 30
  | otherwise                   = 31

firstJan1901 :: Date
firstJan1901 = go (Date 1 Jan 1900 Mon)
  where go r@(Date 1 Jan 1901 _) = r
        go d = go $ next d

sol19 :: Int
sol19 = go firstJan1901
  where go :: Date -> Int
        go (Date 31 Dec 2000 _) = 0
        go c@(Date 1 _ _ Sun) = 1 + (go $ next c)
        go c = go $ next c

factorial :: Integral a => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

sol20 = sum $ integerToList $ factorial 100

properDivisors :: Integral a => a -> [a]
properDivisors n = 1 : go n 2 (ceiling $ sqrt $ fromIntegral n)
  where go x c end = if c >= end
                        then []
                        else if x `mod` c == 0
                          then c : (x `div` c : go x (c+1) end)
                          else go x (c+1) end

sol21 = sum $ go 2 Map.empty
  where go 10000 m = []
        go i m = let dn = sum $ properDivisors i in
                     case Map.lookup i m of
                       Nothing -> go (i+1) (Map.insert dn i m)
                       Just b  -> if b == dn
                                     then i : b : go (i+1) m
                                     else go (i+1) (Map.insert dn i m)

readSplitSort :: IO [String]
readSplitSort = do
  s <- readFile "p022_names.txt"
  let ss = sort $ map (filter (/='\"')) (splitString s ',')
  return ss

alpVal :: String -> Integer
alpVal = sum . (map (\x -> fromIntegral $ ord x - ord 'A' + 1))

sol22 = do
  nms <- readSplitSort
  print $ go nms 1
  where go :: [String] -> Integer -> Integer
        go [] c = 0
        go (x:xs) c =  c * alpVal x + (go xs (c+1))

data FacClass = Deficient | Perfect | Abundant
  deriving (Eq, Ord, Show)

classify :: Integer -> FacClass
classify n = case compare (sum (properDivisors n)) n of
               LT -> Deficient
               EQ -> Perfect
               GT -> Abundant

isAbundant :: Integral a => a -> Bool
isAbundant n = n < (sum $ properDivisors n)

abundantTill :: [Int]
abundantTill = filter isAbundant [1..28123]

posSums :: Set Int
posSums = Set.fromList [x+y | x <- abundantTill, y <- abundantTill]

-- wrong answer for god knows what reason :((
sol23 = sum $ filter (not . ((flip Set.member) posSums)) [1..28123]

-- Too slow (still feasible tho - takes about 13 seconds to calculate all perms)
perms :: Eq a => [a] -> [[a]]
perms [] = []
perms [x] = [[x]]
perms xs = foldr f [] xs
  where f x y = (map ((:)x) (perms (xs \\ [x]))) ++ y

set24 = Set.fromList $ permutations "0123456789"

-- "Only" 10! permutations
-- Can definitely make it WAYYYY faster using combinatorics (TODO)
-- Guess that it starts with 2 (or 3)
sol24 = Set.elemAt 999999 set24

dig1000 :: Integer
dig1000 = 10^999

sol25 = go 1 1 3
  where go f s c = if f + s >= dig1000
                      then c
                      else go s (f + s) (c + 1)

repeating :: Int -> Int -> Int
repeating a b = decList a b 0 [] False
  where decList :: Int -> Int -> Int -> [Int] -> Bool -> Int
        decList a b c xs t = let i = a `div` b
                                 m = a `mod` b in
                                 case elemIndex a xs of
                                   Just x  -> abs (x - c)
                                   Nothing ->
                                     if i == 0
                                        then decList (a*10) b (if t then (c+1) else c) 
                                                              (if t
                                                                  then (xs++[0])
                                                                  else xs)
                                                     True
                                        else if m == 0
                                                then 0
                                                else decList m b (c+1) (xs++[a]) False

-- Also just a pattern that you can use that straight up gives the answer.
sol26 = go 1 (1,0)
  where go 1000 (m, mi) = mi
        go x (m, mi) = let nm = repeating 1 x in
                           go (x+1) (if nm > m then (nm, x) else (m, mi))

numConsPrimes :: Int -> Int -> Int
numConsPrimes a b = go 0
  where go n = if isPrime (n^2 + a*n + b)
                  then go (n+1)
                  else n

-- n^2 + an + b where |a| < 1000, |b| <= 1000, max num of primes for
-- n = 0,... 
sol27 = (\(x,y,_) -> x*y) $ maximumBy (\(_,_,x) (_,_,y) -> compare x y)
        [(a,b,numConsPrimes a b) | a <- [-999..999], b <- [-1000..1000]]

-- Sum of the sequences gives this formula
sol28 = (\n -> (3+2*n*(8*n*n+15*n+13))/3) ((1001-1)/2)

sol29 = foldr (\x y -> y + 1) 0 $ Set.fromList [a^b | a <- [2..100], b <- [2..100]]

digList :: Integral a => a -> [a]
digList i = go i []
  where go 0 acc = acc
        go x acc = go (x`div`10) ((x`mod`10) : acc)

listDig :: Integral a => [a] -> a
listDig = foldl' (\x y -> 10*x+y) 0

sol30 = go 1
  where go 1000000 = 0
        go n = (if (sum $ map (^5) (digList n)) == n then n else 0)
               + go (n+1)

coinTypes :: [Int]
coinTypes = [1,2,5,10,20,50,100,200]

coins :: Map Int Int
coins = Map.fromList [(1,1),(2,2),(3,2),(4,4),(5,6),(6,5)]

sol31 = undefined -- need to scribble stuff

isPandigital :: Integral a => [a] -> Bool
isPandigital xs = let n = length xs in
                    foldr (\x y -> y && elem x xs) True [1..(fromIntegral n)]

panProdT :: Integral a => a -> Bool
panProdT x = 
  let m = (ceiling $ sqrt $ fromIntegral x) + 1
      ids = digList x in
      go 2 m ids
      where go i m ids = if i == m then False else
                            let (q, r) = divMod x i in
                                if r /= 0
                                  then go (i+1) m ids
                                  else let dq = digList q
                                           dq2 = digList (x`div`q)
                                           dl = concat [dq, dq2, ids] in
                                           if length dl == 9 && isPandigital dl
                                              then True
                                              else go (i+1) m ids

-- Thought this was slow.. it's not... 0.38 seconds. I do use the exact bounds tho
sol32 :: Integer
sol32 = sum $ filter panProdT [4396..7852]

-- This was used to optimise
maxi = head $ filter pred [(x,y,x*y) | x <- (reverse [1..10000]), y <- reverse ([1..10000])]
  where pred (a,b,c) = let tl = concat [digList a, digList b, digList c] in
                       length tl == 9 && isPandigital tl

-- Quite fast - about 7 seconds compiled
sol32' :: Integer
sol32' = Set.foldr (+) 0 $ Set.fromList [x*y | x <- [1..2000], y <- [1..2000], pred x y]
  where pred a b = let tl = concat [digList a, digList b, digList (a*b)] in
                       length tl == 9 && isPandigital tl

curiousFrac :: Integral a => (a, a) -> Bool
curiousFrac (n, d) = let dn = digList n
                         dd = digList d
                         nn = listDig $ dn \\ dd
                         nd = listDig $ dd \\ dn in
                         if (n`mod`10==0 && d`mod`10 == 0) || nd == 0 || nn == 0
                            || nn == n || nd == d
                            then False
                            else n % d == nn % nd

sol33 :: Int
sol33 = denominator $ (product . map (\(a,b) -> a % b)) $ 
          filter curiousFrac [(a,b) | a <- [10..99], b <- [10..99], a < b]

curiousNum :: Integer -> Bool
curiousNum n = n == (sum $ map factorial (digList n))

sol34 = sum $ filter curiousNum [3..40585]

-- https://stackoverflow.com/a/44309145
rotate :: Int -> [a] -> [a]
rotate n xs = take lxs . drop (n `mod` lxs) . cycle $ xs where lxs = length xs

isCircularPrime :: Int -> Bool
isCircularPrime n = (all isPrime) . (map (\x -> listDig $ rotate x ns)) $ [0..lns-1]
  where ns = digList n
        lns = length ns

sol35 = length $ filter isCircularPrime $ takeWhile (<1000000) primes

intToBS :: Int -> String
intToBS i = stringify $ go i
  where stringify [] = ""
        stringify [x] = '1' : replicate x '0'
        stringify (x:xs) = '1' : (replicate (x-(head xs)-1) '0') ++ (stringify xs)
        go 0 = []
        go 1 = [0]
        go i = let p = head $ dropWhile (\x -> 2^(x+1) <= i) [1..] in
                   p : go (i - 2^p)  

doubleBase :: Int -> Bool
doubleBase i = let dl = digList i
                   bs = intToBS i in
                   dl == reverse dl && bs == reverse bs

sol36 = sum $ filter doubleBase [1..999999]

tEnd [] = []
tEnd [x] = []
tEnd (x:xs) = x : tEnd xs

trunLeft :: Integral a => [a] -> Bool
trunLeft [] = True
trunLeft xs = let n = listDig xs in
                  if isPrime n
                     then trunLeft $ tail xs
                     else False

trunRight :: Integral a => [a] -> Bool
trunRight [] = True
trunRight xs = let n = listDig xs in
                   if isPrime n
                      then trunRight $ tEnd xs
                      else False

trunPrime x = let dx = digList x in
                  trunLeft dx && trunRight dx

sol37 = go (drop 4 primes) 0
  where go _ 11  = 0
        go (x:xs) c = if trunPrime x
                         then x + go xs (c+1)
                         else go xs c

concatProd :: Int -> Maybe Int
concatProd i = go 1 []
  where go c acc = case compare (length acc) 9 of
                     EQ -> if isPandigital acc
                              then Just (listDig acc)
                              else Nothing
                     LT -> go (c+1) (concat $ acc : [digList(c * i)])
                     GT -> Nothing

sol38 = go 1 0
  where go 999999 m = m
        go c m = case concatProd c of
                   Just n -> go (c+1) (if n > m then n else m)
                   Nothing -> go (c+1) m

pythTripsSet = Set.fromList $ map (Set.fromList . il) pythTrips
  where il (x,y,z) = [x,y,z]

numSols i = (i, Set.size $ Set.filter (\x -> foldr (+) 0 x == i) pythTripsSet)

sol39 = fst $ maximumBy (\(_,x) (_,y) -> compare x y) $ map numSols [1..1000]

sol40 = product $ map go [1,10,100,1000,10000,100000,1000000] 
  where frc = concat $ map show [1..]
        go i = ord (frc !! (i-1)) - ord '0'

-- Took a decent bit of time, close to 10 mins?
sol41 :: Int
sol41 = go 999999999
  where go x = if isPrime x && isPandigital (digList x)
                  then x
                  else go $ x-1

-- Haven't timed but I assume its similar to other solution - no idea how the
-- lib works out primes tho. If its trial division it should be exactly the same
-- pretty much.
sol41' :: Int
sol41' = go $ T.precPrime (999999999 :: Int)
  where go x = if isPandigital ((digList.fromEnum) x)
                  then fromEnum x
                  else go $ pred x

isTriangleWord :: String -> Bool
isTriangleWord s = go triangleNumbers $ sum $ map (\x -> ord x - ord 'A' + 1) s
  where go (x:xs) i = case compare x i of
                        EQ -> True
                        LT -> go xs i
                        GT -> False

sol42 = do
  s <- readFile "p042_words.txt"
  let ss = map (filter (/='\"')) (splitString s ',')
  print $ length $ filter isTriangleWord ss

slice :: (Int,Int) -> [b] -> [b]
slice (s,e) = take (e-s+1) . drop s

threeSliceBack :: Integral a =>  Int -> [a] -> a
threeSliceBack p xs = listDig $ take 3 $ drop p xs

prop43 :: Integral a => [a] -> Bool
prop43 i = go 0
  where go 7 = True
        go x = if (threeSliceBack (x+1) i) `rem` (primes !! x) == 0
                  then go (x+1)
                  else False

isPandigital09 :: Integral a => [a] -> Bool
isPandigital09 xs = let n = length xs in
                      if n /= 10
                         then False
                         else foldr (\x y -> y && elem x xs) True [0..9]

sol43 :: Integer
sol43 = sum $ map listDig $ filter prop43 $ permutations [0,1,2,3,4,5,6,7,8,9]

pentNums :: Integral a => [a]
pentNums = map (\x -> x*(3*x-1)`div`2) [1..]

pentSet :: Integral a => Int -> Set a
pentSet n = Set.fromList $ take n pentNums

peen = take 1000000 pentNums

pentPairs = filter (\(x,y,z) -> elem (x+y) peen && elem z peen) $ 
              map (\(x,y) -> (x,y,abs(x-y))) $ 
              zipWith (,) peen peen

sol44 = minimumBy (\(x,y,z) (a,b,c) -> compare z c) pentPairs
