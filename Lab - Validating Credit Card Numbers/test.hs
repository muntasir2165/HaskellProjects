toDigits :: Integer -> [Integer]
--toDigits = undefined
toDigits x
    | x == (x `mod` 10) = [x]
    | otherwise = reverse((x `mod` 10) : reverse(toDigits (x `div` 10)))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse(toDigits x)

--unused helper function for doubleSecond
--index :: [Integer] -> [(Integer, Int)]
--index [] = []
--index xs = zip xs [0..n]
--			where n = length xs - 1

doubleSecond :: [Integer] -> [Integer]
doubleSecond xs = zipWith (\x y -> x * y) xs (take (length xs) (cycle([1,2])))

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concat([toDigits x | x <- xs]))

isValid :: Integer -> Bool
isValid x = sumDigits(doubleSecond((toDigitsRev x))) `mod` 10 == 0