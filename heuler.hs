-- !!!!!!!!! SPOILERS ALERT :) !!!!!!!!!!!! --

-- it's mainly not about performance, but about getting the result )
-- also some parts are really ugle.. i'll try to make them nicer/faster down the road
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
    | otherwise      = getDiagonal' m (m !! i !! i : result) (succ i)


getDiagonals m = getDiagonal m : getDiagonal (reverse m) : []


getSubMatrixes :: [[a]] -> Int -> [[[a]]]
getSubMatrixes [] _ = []
getSubMatrixes _ 0 = []
getSubMatrixes matrix n = getSubMatrixes' matrix n []
getSubMatrixes' matrix n result
    | horizontal_limit_reached = reverse result
    | otherwise                = getSubMatrixes' stripped_matrix n vertical_submatices
    where horizontal_limit_reached = n > length (matrix !! 0)
          stripped_matrix          = (map (tail) matrix)
          vertical_submatices      = getVertSubMatrices matrix n result

getVertSubMatrices :: [[a]] -> Int -> [[[a]]] -> [[[a]]]
getVertSubMatrices matrix n result
    | vertical_limit_reached = result
    | otherwise = getVertSubMatrices stripped_matrix n (take n column : result)
    where vertical_limit_reached = n > length matrix
          stripped_matrix        = tail matrix
          column                 = map (take n) matrix

_EULER_ANSWER_11 = maximum $ concat $ map (getProducts) (getSubMatrixes pr11data 4)

--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=12
triangular_numbers = [sum [1..n] | n <- [1..]]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

getFirstNumWithDivisors n pool
    | num_divisors >= n = num
    | otherwise         = getFirstNumWithDivisors n (tail pool)
    where num = pool !! 0
          num_divisors = length (divisors num)

some_primes = filter (isPrime) [1..17]
_EULER_ANSWER_12 = getFirstNumWithDivisors 500 (filter (\x-> x > product(some_primes) && all (==0) (map (rem x) some_primes)) triangular_numbers)


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=13

pr13data =
    [
        37107287533902102798797998220837590246510135740250,
        46376937677490009712648124896970078050417018260538,
        74324986199524741059474233309513058123726617309629,
        91942213363574161572522430563301811072406154908250,
        23067588207539346171171980310421047513778063246676,
        89261670696623633820136378418383684178734361726757,
        28112879812849979408065481931592621691275889832738,
        44274228917432520321923589422876796487670272189318,
        47451445736001306439091167216856844588711603153276,
        70386486105843025439939619828917593665686757934951,
        62176457141856560629502157223196586755079324193331,
        64906352462741904929101432445813822663347944758178,
        92575867718337217661963751590579239728245598838407,
        58203565325359399008402633568948830189458628227828,
        80181199384826282014278194139940567587151170094390,
        35398664372827112653829987240784473053190104293586,
        86515506006295864861532075273371959191420517255829,
        71693888707715466499115593487603532921714970056938,
        54370070576826684624621495650076471787294438377604,
        53282654108756828443191190634694037855217779295145,
        36123272525000296071075082563815656710885258350721,
        45876576172410976447339110607218265236877223636045,
        17423706905851860660448207621209813287860733969412,
        81142660418086830619328460811191061556940512689692,
        51934325451728388641918047049293215058642563049483,
        62467221648435076201727918039944693004732956340691,
        15732444386908125794514089057706229429197107928209,
        55037687525678773091862540744969844508330393682126,
        18336384825330154686196124348767681297534375946515,
        80386287592878490201521685554828717201219257766954,
        78182833757993103614740356856449095527097864797581,
        16726320100436897842553539920931837441497806860984,
        48403098129077791799088218795327364475675590848030,
        87086987551392711854517078544161852424320693150332,
        59959406895756536782107074926966537676326235447210,
        69793950679652694742597709739166693763042633987085,
        41052684708299085211399427365734116182760315001271,
        65378607361501080857009149939512557028198746004375,
        35829035317434717326932123578154982629742552737307,
        94953759765105305946966067683156574377167401875275,
        88902802571733229619176668713819931811048770190271,
        25267680276078003013678680992525463401061632866526,
        36270218540497705585629946580636237993140746255962,
        24074486908231174977792365466257246923322810917141,
        91430288197103288597806669760892938638285025333403,
        34413065578016127815921815005561868836468420090470,
        23053081172816430487623791969842487255036638784583,
        11487696932154902810424020138335124462181441773470,
        63783299490636259666498587618221225225512486764533,
        67720186971698544312419572409913959008952310058822,
        95548255300263520781532296796249481641953868218774,
        76085327132285723110424803456124867697064507995236,
        37774242535411291684276865538926205024910326572967,
        23701913275725675285653248258265463092207058596522,
        29798860272258331913126375147341994889534765745501,
        18495701454879288984856827726077713721403798879715,
        38298203783031473527721580348144513491373226651381,
        34829543829199918180278916522431027392251122869539,
        40957953066405232632538044100059654939159879593635,
        29746152185502371307642255121183693803580388584903,
        41698116222072977186158236678424689157993532961922,
        62467957194401269043877107275048102390895523597457,
        23189706772547915061505504953922979530901129967519,
        86188088225875314529584099251203829009407770775672,
        11306739708304724483816533873502340845647058077308,
        82959174767140363198008187129011875491310547126581,
        97623331044818386269515456334926366572897563400500,
        42846280183517070527831839425882145521227251250327,
        55121603546981200581762165212827652751691296897789,
        32238195734329339946437501907836945765883352399886,
        75506164965184775180738168837861091527357929701337,
        62177842752192623401942399639168044983993173312731,
        32924185707147349566916674687634660915035914677504,
        99518671430235219628894890102423325116913619626622,
        73267460800591547471830798392868535206946944540724,
        76841822524674417161514036427982273348055556214818,
        97142617910342598647204516893989422179826088076852,
        87783646182799346313767754307809363333018982642090,
        10848802521674670883215120185883543223812876952786,
        71329612474782464538636993009049310363619763878039,
        62184073572399794223406235393808339651327408011116,
        66627891981488087797941876876144230030984490851411,
        60661826293682836764744779239180335110989069790714,
        85786944089552990653640447425576083659976645795096,
        66024396409905389607120198219976047599490197230297,
        64913982680032973156037120041377903785566085089252,
        16730939319872750275468906903707539413042652315011,
        94809377245048795150954100921645863754710598436791,
        78639167021187492431995700641917969777599028300699,
        15368713711936614952811305876380278410754449733078,
        40789923115535562561142322423255033685442488917353,
        44889911501440648020369068063960672322193204149535,
        41503128880339536053299340368006977710650566631954,
        81234880673210146739058568557934581403627822703280,
        82616570773948327592232845941706525094512325230608,
        22918802058777319719839450180888072429661980811197,
        77158542502016545090413245809786882778948721859617,
        72107838435069186155435662884062257473692284509516,
        20849603980134001723930671666823555245252804609722,
        53503534226472524250874054075591789781264330331690
    ]

-- languages with native "long" numbers support rules
_EULER_ANSWER_13 = take 10 (show $ sum pr13data)


--------------------------------------------------------------------------------
-- http://projecteuler.net/problem=14
collatzSequence :: Int -> [Int]
collatzSequence n = collatzSequence' n [n]
collatzSequence' n accum
    | n == 1 = reverse accum
    | even n = let new_n = n `div` 2 in collatzSequence' new_n (new_n:accum)
    | odd n  = let new_n = 3 * n + 1 in collatzSequence' new_n (new_n:accum)

-- it took few minutes to process all numbers, thus some optimisation is needed
-- but it's ok for now.

_EULER_ANSWER_14 = snd $ maximum $ map (\x -> (length (collatzSequence x), x)) [1..1000000]

-- http://projecteuler.net/problem=15
--------------------------------------------------------------------------------
-- coming soon

-- http://projecteuler.net/problem=16
--------------------------------------------------------------------------------


-- http://projecteuler.net/problem=16
-- again: languages with native "long" numbers support rules :)
_EULER_ANSWER_16 =sum $  map (Char.digitToInt) (show (2^1000))
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
        "pr 12: actual: 76576500:      "   ++ show _EULER_ANSWER_12    ++ "\n" ++
        "pr 13: actual: 5537376230:    "   ++ show _EULER_ANSWER_13    ++ "\n" ++
        "pr 14: actual: 837799:        "   ++ show "Takes to long :("  ++ "\n" ++
        "pr 15: actual: ??????,:       "   ++ show "_EULER_ANSWER_15"  ++ "\n"


