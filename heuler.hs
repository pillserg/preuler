-- it's mainly not about performance, but about getting the result )
import qualified Data.Set as Set
import qualified Data.Char as Char
import Debug.Trace
import Data.Time


----------------------------------------
-- http://projecteuler.net/problem=1
sumOfMultiples3and5under :: Int -> Int
sumOfMultiples3and5under n =
    let mults_of x = Set.fromList $ takeWhile (<n) $ map (*x) $ [1..]
    in sum $ Set.toList $ Set.union (mults_of 3) (mults_of 5)

_EULER_ANSWER_1 = sumOfMultiples3and5under 1000

----------------------------------------
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

----------------------------------------
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

----------------------------------------
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

----------------------------------------
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

----------------------------------------
--http://projecteuler.net/problem=6
sumProdDivProdsSum :: [Integer] -> Integer
sumProdDivProdsSum xs = (sum xs) ^ 2 - sum (map (^2) xs)

_EULER_ANSWER_6 = sumProdDivProdsSum [1..100]

----------------------------------------
--http://projecteuler.net/problem=7

_EULER_ANSWER_7 = (filter (isPrime) [1..]) !! 10001

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


-- http://projecteuler.net/problem=10
-- bruteforce soulution (It's really time to implement some nice sive solution or use primes from Haskell Packages :) )
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


main =
    -- o i,
    putStrLn $
        "projecteuler pr 1: actual: 233168, calculated: "         ++ show _EULER_ANSWER_1     ++ "\n" ++
        "projecteuler pr 2: actual: 4613732, calculated: "        ++ show _EULER_ANSWER_2     ++ "\n" ++
        "projecteuler pr 3: actual: 6857, calculated: "           ++ show _EULER_ANSWER_3     ++ "\n" ++
        "projecteuler pr 4: actual: 906609, calculated: "         ++ show _EULER_ANSWER_4     ++ "\n" ++
        "projecteuler pr 5: actual: 232792560, calculated: "      ++ show _EULER_ANSWER_5     ++ "\n" ++
        "projecteuler pr 6: actual: 25164150, calculated: "       ++ show _EULER_ANSWER_6     ++ "\n" ++
        "projecteuler pr 7: actual: 104743, calculated: "         ++ show _EULER_ANSWER_7     ++ "\n" ++
        "projecteuler pr 8: actual: 40824, calculated: "          ++ show _EULER_ANSWER_8     ++ "\n" ++
        "projecteuler pr 9: actual: 31875000, calculated: "       ++ show _EULER_ANSWER_9     ++ "\n" ++
        "projecteuler pr 10: actual: 142913828922, calculated: "  ++ show "_EULER_ANSWER_10"  ++ "\n" ++
        "projecteuler pr 11: actual: 70600674, calculated: "      ++ show "_EULER_ANSWER_11"  ++ "\n" ++
        "projecteuler pr 12: actual: ??????, calculated: "        ++ show "_EULER_ANSWER_12"  ++ "\n" ++
        "projecteuler pr 13: actual: 5537376230, calculated: "    ++ show "_EULER_ANSWER_13"  ++ "\n" ++
        "projecteuler pr 14: actual: 837799, calculated: "        ++ show "_EULER_ANSWER_14"  ++ "\n" ++
        "projecteuler pr 15: actual: ??????, calculated: "        ++ show "_EULER_ANSWER_15"  ++ "\n"


