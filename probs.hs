import Data.List.Ordered
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Numbers.Primes
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

sol1 :: Int
sol1 = sum [x | x <- takeWhile (<1000) $ [3,6..] `union` [5,10..]]

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

sol3_2 :: Integer
sol3_2 = maximum $ mp primes 600851475143 1
  where mp (x:xs) n p = if p == n
                             then []
                             else if n `mod` x == 0
                               then x : mp xs n (p*x)
                               else mp xs n p

sol4 :: Int
sol4 = maximum $ filter isPali $ [x*y | x <- [100..999], y <- [100..999]]
  where isPali :: Int -> Bool
        isPali i = (reverse . show) i  == show i

sol4_2 :: Int
sol4_2 = head $ dropWhile (not . isPali) $ reverse $ sort [x*y | x <- [100..999], y <- [100..999]]
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

-- Dynamic programming
sol15 = fst $ movements 0 0 vGrid

sol16 = sum $ integerToList (2^1000)
