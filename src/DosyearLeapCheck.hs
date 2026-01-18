module DosyearLeapCheck (
    Dosyear (..),
    Offset (..),
    offsetDefault,
    RelativeYear (..),
    DosyearError (..),
    int2rel,
    rel2abs,
    abs2rel,
    abs2dos,
    int2year,
    int2yearDefault,
    str2int,
    str2yearDefault,
    str2isLeap,
    isLeapDefault,
    mod4,
    isMultipleOf4,
) where

import Control.Monad ((>=>))

newtype Dosyear = Dosyear Int deriving (Eq, Show)

newtype Offset = Offset Int

offsetDefault :: Offset
offsetDefault = Offset 1980

data DosyearError
    = NegativeRelativeYear Int
    | RelativeYearOutOfRange Int
    | InvalidYearString String
    deriving (Show, Eq)

newtype RelativeYear = RelativeYear Int

int2rel :: Int -> Either DosyearError RelativeYear
int2rel i
    | i < 0 = Left (NegativeRelativeYear i)
    | 127 < i = Left (RelativeYearOutOfRange i)
    | otherwise = Right (RelativeYear i)

rel2abs :: Offset -> RelativeYear -> Dosyear
rel2abs (Offset off) (RelativeYear rel) = Dosyear (off + rel)

abs2rel :: Offset -> Int -> Either DosyearError RelativeYear
abs2rel (Offset off) i = int2rel (i - off)

abs2dos :: Offset -> Int -> Either DosyearError Dosyear
abs2dos off i = do
    rel :: RelativeYear <- abs2rel off i
    Right (rel2abs off rel)

int2year :: Offset -> Int -> Either DosyearError Dosyear
int2year = abs2dos

int2yearDefault :: Int -> Either DosyearError Dosyear
int2yearDefault = int2year offsetDefault

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _ -> Nothing

str2int :: String -> Either DosyearError Int
str2int s =
    case readMaybe s of
        Just i -> Right i
        Nothing -> Left (InvalidYearString s)

str2yearDefault :: String -> Either DosyearError Dosyear
str2yearDefault = str2int >=> int2yearDefault

str2isLeap :: String -> Either DosyearError Bool
str2isLeap = fmap isLeapDefault . str2yearDefault

mod4 :: Int -> Int
mod4 x = x `mod` 4

isMultipleOf4 :: Int -> Bool
isMultipleOf4 x = 0 == mod4 x

isLeapDefault :: Dosyear -> Bool
isLeapDefault (Dosyear 2100) = False
isLeapDefault (Dosyear y) = isMultipleOf4 y
