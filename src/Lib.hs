module Lib
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- | remove *non* UPPERCASE chars.
-- >>> removeNonUppercase "qWERt"
-- "WER"
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- | remove UPPERCASE chars.
-- >>> removeUppercase "qWERt"
-- "qt"
removeUppercase :: [Char] -> [Char]
removeUppercase st = [c | c <- st, c `elem` ['a'..'z']]

-- | compute Factorial.
-- >>> fac 5
-- 120
fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n-1)

-- | calcuate circumference.
-- >>> circumference 4.0 :: Float
-- 25.132742
-- >>> circumference 4.0 :: Double
-- 25.132741228718345
circumference :: (Floating a) => a -> a
circumference r = 2 * pi * r

-- | compute fibonacci numbers.
-- >>> fib 10
-- 55
fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | return a lucky sentence when given 7
-- | otherwise out of luck.
-- >>> lucky 7
-- "LUCKY NUMBER SEVEN!"
-- >>> lucky 1
-- "Sorry, you're out of luck, pal!"
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

-- | another implementation of head
-- >>> head' [4,5,6]
-- 4
-- >>> head' "hello"
-- 'h'
head' :: [a] -> a
head' []    = error "empty list"
head' (x:_) = x

-- | another implementation of length
-- >>> length' [1,2,3]
-- 3
-- >>> length' "hello"
-- 5
length' :: (Integral b) => [a] -> b
length' []     = 0
length' (_:xs) = 1 + length' xs

-- | another implementation of sum
-- >>> sum' [1,2,3,4]
-- 10
sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- | another implementation of sum
-- >>> sum'' [1,2,3,4]
-- 10
sum'' :: (Num a) => [a] -> a
sum'' xs = foldr (+) 0 xs

-- | calculate BMI then return as string.
-- >>> bmiTell 85 1.90
-- "You're supposedly normal. Pffft, I bet you're ugly!"
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- | another implementation of maximum
-- >>> maximum' [1,2,5,4,3]
-- 5
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxtail = x
  | otherwise = maxtail
  where maxtail = maximum' xs
