-- some stuff. and few preuler problems
mytake :: (Num i, Ord i) => i -> [a] -> [a]
mytake n _
     | n <=0    = []
mytake _ []     = []
mytake n (x:xs) = x : mytake (n-1) xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]

myrepeat :: a -> [a]
myrepeat x = x:myrepeat x

myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y):myzip xs ys

myelem :: (Eq a) => a -> [a] -> Bool
myelem a [] = False
myelem a (x:xs)
    | a == x    = True
    | otherwise = a `myelem` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x ]

------------------------------------------------------

--http://projecteuler.net/problem=4

isPalindrom [] = True
isPalindrom [x] = True
isPalindrom (x:xs)
    | x == (last xs) = isPalindrom (init xs)
    | otherwise = False


digit2ProdPalindroms = [ p | a <- [10..99], b <- [10..99], let p = a*b, isPalindrom (show p) ]
digit3ProdPalindroms = [ p | a <- [100..999], b <- [100..999], let p = a*b, isPalindrom (show p) ]

----------------------------------------
--http://projecteuler.net/problem=5

divisors = [20, 19..1]
isDivisors _ [] = True
isDivisors n (x:xs)
        | n `mod` x == 0 = isDivisors n xs
        | otherwise      = False

findS n
    | isDivisors n divisors = n
    | otherwise             = findS (n + sum divisors)

smallestCommon = findS (sum divisors)

----------------------------------------------
--http://projecteuler.net/problem=6
diff = (sum [1..100]) ^ 2 - sum [ x^2 | x <- [1..100]]


isDivisible :: Int -> Int -> Bool
isDivisible x y
    | y == 1 || x == 1 = True
    | x `mod` y == 0 = False
    | otherwise = isDivisible x (y-1)

isPrime :: Int -> Bool
isPrime x = isDivisible x (x `div` 2)


getPrimes current_n x target_n
    | target_n == current_n             = x - 1
    | current_n < target_n && isPrime x = getPrimes (current_n+1)  (x+2) target_n
    | otherwise                         = getPrimes current_n (x+2) target_n

_getNthPrime = getPrimes 1 3
