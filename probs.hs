import Data.List.Ordered
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Numbers.Primes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sort
import Data.Char
import Data.List ((\\), union, permutations, splitAt, elemIndex)
import qualified Data.Vector.Algorithms.Intro as VA

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
sol1 = sum [x | x <- takeWhile (<1000) $ [3,6..] `Data.List.Ordered.union` [5,10..]]

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

triangleNumbers :: [Integer]
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
