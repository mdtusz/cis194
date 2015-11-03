--Section 1

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = toDigits (div n 10) ++ [mod n 10] 

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)


double :: Integer -> Integer
double x = x * 2

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft [x] = [x]
doubleEveryOtherFromLeft (x:y:zs) = x : double y : doubleEveryOtherFromLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherFromLeft $ reverse xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum $ concatMap (toDigits) xs

validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0


--Section 2

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 x y z = [(x, y)]