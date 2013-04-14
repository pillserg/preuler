-- it's mainly not about performance, but about getting the result )
import qualified Data.Set as Set

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
        "projecteuler pr 8: actual: 40824, calculated: "          ++ show "_EULER_ANSWER_8"   ++ "\n" ++
        "projecteuler pr 9: actual: 31875000, calculated: "       ++ show "_EULER_ANSWER_9"   ++ "\n" ++
        "projecteuler pr 10: actual: 142913828922, calculated: "  ++ show "_EULER_ANSWER_10"  ++ "\n" ++
        "projecteuler pr 11: actual: 70600674, calculated: "      ++ show "_EULER_ANSWER_11"  ++ "\n" ++
        "projecteuler pr 12: actual: ??????, calculated: "        ++ show "_EULER_ANSWER_12"  ++ "\n" ++
        "projecteuler pr 13: actual: 5537376230, calculated: "    ++ show "_EULER_ANSWER_13"  ++ "\n" ++
        "projecteuler pr 14: actual: 837799, calculated: "        ++ show "_EULER_ANSWER_14"  ++ "\n" ++
        "projecteuler pr 15: actual: ??????, calculated: "        ++ show "_EULER_ANSWER_15"  ++ "\n"


