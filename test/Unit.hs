module Main (main) where

import DosyearLeapCheck

-- A simple assertion function
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
    if expected == actual
        then putStrLn $ "PASS: " ++ msg
        else putStrLn $ "FAIL: " ++ msg ++ " Expected: " ++ show expected ++ ", Actual: " ++ show actual

-- Test cases for isLeapDefault
testIsLeapDefault :: IO ()
testIsLeapDefault = do
    putStrLn "\n--- Testing isLeapDefault ---"
    assertEqual "1980 is leap" True (isLeapDefault (Dosyear 1980))
    assertEqual "1981 is not leap" False (isLeapDefault (Dosyear 1981))
    assertEqual "2000 is leap" True (isLeapDefault (Dosyear 2000))
    assertEqual "2100 is not leap (special case)" False (isLeapDefault (Dosyear 2100))
    assertEqual "2107 is not leap" False (isLeapDefault (Dosyear 2107))

-- Test cases for str2int
testStr2Int :: IO ()
testStr2Int = do
    putStrLn "\n--- Testing str2int ---"
    assertEqual "str2int '1980'" (Right 1980) (str2int "1980")
    assertEqual "str2int 'foo'" (Left (InvalidYearString "foo")) (str2int "foo")
    assertEqual "str2int '1985.823'" (Left (InvalidYearString "1985.823")) (str2int "1985.823")

-- Test cases for int2yearDefault (boundary conditions and errors)
testInt2YearDefault :: IO ()
testInt2YearDefault = do
    putStrLn "\n--- Testing int2yearDefault ---"
    assertEqual "int2yearDefault 1980" (Right (Dosyear 1980)) (int2yearDefault 1980)
    assertEqual "int2yearDefault 1979 (NegativeRelativeYear)" (Left (NegativeRelativeYear (-1))) (int2yearDefault 1979)
    assertEqual "int2yearDefault 2108 (RelativeYearOutOfRange)" (Left (RelativeYearOutOfRange 128)) (int2yearDefault 2108)

-- Main test runner
main :: IO ()
main = do
    testIsLeapDefault
    testStr2Int
    testInt2YearDefault
