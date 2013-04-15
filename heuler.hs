-- it's mainly not about performance, but about getting the result )
import qualified Data.Set as Set
import qualified Data.Char as Char
import Debug.Trace
import Data.Time
import Data.List


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=1
sumOfMultiples3and5under :: Int -> Int
sumOfMultiples3and5under n =
    let mults_of x = Set.fromList $ takeWhile (<n) $ map (*x) $ [1..]
    in sum $ Set.toList $ Set.union (mults_of 3) (mults_of 5)

_EULER_ANSWER_1 = sumOfMultiples3and5under 1000


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=2
--             1 : 2
--         1 : 2
--     1 : 2 : 3
-- 1 : 2 : 3 : 5
sumEvenFibsUnder :: Int -> Int
sumEvenFibsUnder n =
    let fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
    in sum $ filter even $ takeWhile (<n) fibs

_EULER_ANSWER_2 = sumEvenFibsUnder 4000000


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=3
-- fromIntegral gave me a little headache but now it's ok :)
-- also had to reimplement it in python first.
isPrime :: Integer -> Bool
isPrime n = isPrime' n 2
isPrime' n x
    | x * x > n = True
    | n `rem` x == 0 = False
    | otherwise = isPrime' n $ succ x

primeDivisor n =
    let possible_prime_factors = 1 : filter ((==0) . rem n) [2..round $ sqrt $ fromIntegral n]
    in filter (isPrime) possible_prime_factors

_EULER_ANSWER_3 = maximum $ primeDivisor 600851475143


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=4
isPalindrom :: Eq a => [a] -> Bool
isPalindrom [] = True
isPalindrom [x] = True
isPalindrom (x:xs)
    | x == (last xs) = isPalindrom (init xs)
    | otherwise = False

_EULER_ANSWER_4 =
    let palindroms = [ p | a <- [100..999], b <- [100..999], let p = a*b, isPalindrom (show p) ]
    in maximum palindroms


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=5
isDivisors :: Integer -> [Integer] -> Bool
isDivisors _ [] = True
isDivisors n (x:xs)
    | n `mod` x == 0 = isDivisors n xs
    | otherwise      = False

smallestMultiple :: [Integer] -> Integer
smallestMultiple divs =
    let step = sum divs
    in (dropWhile (not . (`isDivisors` divs)) [step, step + step ..])!!0

_EULER_ANSWER_5 = smallestMultiple [20, 19..1]


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=6
sumProdDivProdsSum :: [Integer] -> Integer
sumProdDivProdsSum xs = (sum xs) ^ 2 - sum (map (^2) xs)

_EULER_ANSWER_6 = sumProdDivProdsSum [1..100]


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=7
_EULER_ANSWER_7 = (filter (isPrime) [1..]) !! 10001


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=8
problem8InitialData = concat
    [
        "73167176531330624919225119674426574742355349194934",
        "96983520312774506326239578318016984801869478851843",
        "85861560789112949495459501737958331952853208805511",
        "12540698747158523863050715693290963295227443043557",
        "66896648950445244523161731856403098711121722383113",
        "62229893423380308135336276614282806444486645238749",
        "30358907296290491560440772390713810515859307960866",
        "70172427121883998797908792274921901699720888093776",
        "65727333001053367881220235421809751254540594752243",
        "52584907711670556013604839586446706324415722155397",
        "53697817977846174064955149290862569321978468622482",
        "83972241375657056057490261407972968652414535100474",
        "82166370484403199890008895243450658541227588666881",
        "16427171479924442928230863465674813919123162824586",
        "17866458359124566529476545682848912883142607690042",
        "24219022671055626321111109370544217506941658960408",
        "07198403850962455444362981230987879927244284909188",
        "84580156166097919133875499200524063689912560717606",
        "05886116467109405077541002256983155200055935729725",
        "71636269561882670428252483600823257530420752963450"
    ]

prodList :: [Int] -> Int -> [Int]
prodList fromlst n = prodList' [] fromlst n
prodList' result fromlst n
    | length fromlst < n = result
    | otherwise          = result ++ (product $ take n fromlst) : prodList' result (tail fromlst) n

_EULER_ANSWER_8 = maximum $ prodList (map (Char.digitToInt) problem8InitialData) 5


--------------------------------------------------------------------------------
--http://projecteuler.net/problem=9
pithagoreanTripletsUnder n =
    [
        (a,  b,  c)
        | a <- [1..n], b <-[1..n],
        let c = round . sqrt . fromIntegral $ a^2 + b^2,
        (a < b && b < c),
        a^2 + b^2 == c^2
    ]

_EULER_ANSWER_9 =
    let triplet = (filter (\(a, b, c) -> a + b + c == 1000) (pithagoreanTripletsUnder 1000)) !! 0
    in (\(a, b, c) -> a * b * c) triplet


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=10
-- bruteforce soulution (It's really time to implement some nice sive solution
-- or use primes from Haskell Packages :) )

-- to long _EULER_ANSWER_10 = sum $ filter (isPrime) [2..2000000]

-- this solution finished in about 3 minutes in ghci
-- not too bad but still not good + it's ugly

sieveOfEratosthenes :: Integer -> [Integer]
sieveOfEratosthenes n = sieveOfEratosthenes' 2 [2..n] [2]
sieveOfEratosthenes' _ [] primes = primes
sieveOfEratosthenes' p sieve primes =
    let new_sieve = dropWhile (<=p) (filter (\x -> x `rem` p /= 0) sieve)
        new_prime = new_sieve!!0

    in  if length(new_sieve) > 150000
            then sieveOfEratosthenes' new_prime new_sieve (new_prime:primes)
        else primes ++ (filter (isPrime) new_sieve)


_EULER_ANSWER_10 = sum $ sieveOfEratosthenes 2000000


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=10
pr11data =
    [--  00  01  02  03  04  05  06  07  08  09  10  11  12  13  14  15  16  17  18  19
        [08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08], -- 00
        [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00], -- 01
        [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65], -- 02
        [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91], -- 03
        [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80], -- 04
        [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50], -- 05
        [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70], -- 06
        [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21], -- 07
        [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72], -- 08
        [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95], -- 09
        [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92], -- 10
        [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57], -- 11
        [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58], -- 12
        [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40], -- 13
        [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66], -- 14
        [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69], -- 15
        [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36], -- 16
        [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16], -- 17
        [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54], -- 18
        [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]  -- 19
    ]


getProducts matrix =
    map (product) matrix ++
    map (product) (transpose matrix) ++
    map (product) (getDiagonals matrix)


getDiagonal m = getDiagonal' m [] 0
getDiagonal' m result i
    | i == length m  = reverse result
    | otherwise      =
        let new_res = m !! i !! i : result
        in getDiagonal' m new_res (succ i)


getDiagonals m = getDiagonal m : getDiagonal (reverse m) : []


getSubMatrixes :: [[a]] -> Int -> [[[a]]]
getSubMatrixes [] _ = []
getSubMatrixes _ 0 = []
getSubMatrixes matrix n = getSubMatrixes' matrix n []
getSubMatrixes' matrix n result
    | n > length (matrix !! 0) = reverse result
    | otherwise = getSubMatrixes' (map (tail) matrix) n (getVertSubMatrices matrix n result)


getVertSubMatrices :: [[a]] -> Int -> [[[a]]] -> [[[a]]]
getVertSubMatrices matrix n result
    | n > length matrix = result
    | otherwise = getVertSubMatrices (tail matrix) n (take n (map (take n) matrix) : result)

_EULER_ANSWER_11 = maximum $ concat $ map (getProducts) (getSubMatrixes pr11data 4)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
main =
    putStrLn $
        "pr 01: actual: 233168:        "   ++ show _EULER_ANSWER_1     ++ "\n" ++
        "pr 02: actual: 4613732:       "   ++ show _EULER_ANSWER_2     ++ "\n" ++
        "pr 03: actual: 6857:          "   ++ show _EULER_ANSWER_3     ++ "\n" ++
        "pr 04: actual: 906609:        "   ++ show _EULER_ANSWER_4     ++ "\n" ++
        "pr 05: actual: 232792560:     "   ++ show _EULER_ANSWER_5     ++ "\n" ++
        "pr 06: actual: 25164150:      "   ++ show _EULER_ANSWER_6     ++ "\n" ++
        "pr 07: actual: 104743:        "   ++ show _EULER_ANSWER_7     ++ "\n" ++
        "pr 08: actual: 40824:         "   ++ show _EULER_ANSWER_8     ++ "\n" ++
        "pr 09: actual: 31875000:      "   ++ show _EULER_ANSWER_9     ++ "\n" ++
        "pr 10: actual: 142913828922:  "   ++ show "TAKES to long :("  ++ "\n" ++
        "pr 11: actual: 70600674:      "   ++ show _EULER_ANSWER_11    ++ "\n" ++
        "pr 12: actual: ??????,:       "   ++ show "_EULER_ANSWER_12"  ++ "\n" ++
        "pr 13: actual: 5537376230:    "   ++ show "_EULER_ANSWER_13"  ++ "\n" ++
        "pr 14: actual: 837799:        "   ++ show "_EULER_ANSWER_14"  ++ "\n" ++
        "pr 15: actual: ??????,:       "   ++ show "_EULER_ANSWER_15"  ++ "\n"


