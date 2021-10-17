--
-- EPITECH PROJECT, 2021
-- B-FUN-300-STG-3-1-funPoolDay2-clement.muth
-- File description:
-- DoOp
--

import Control.Monad
import System.Environment
import System.Exit

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | a == x = True
    | otherwise = myElem a xs

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myIsEqZero :: Int -> Bool
myIsEqZero n
    | n <= 0 = True
    | otherwise = False

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (a `div` b)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:x) = 1 + myLength x

safeNth :: [a] -> Int -> Maybe a
safeNth [] x = Nothing
safeNth (y:ys) x
    | myLength(ys) < x = Nothing
    | myIsNeg(x) == True = Nothing
    | myIsEqZero(x) = Just y
    | otherwise = safeNth ys (x - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc (Just a) = Just (a + 1)
safeSucc _ = Nothing

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup a [] =  Nothing
myLookup a (x:xs)
    | a == (fst x) = Just (snd x)
    | otherwise = myLookup a xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f maybeA maybeB =
    do a <- maybeA
       b <- maybeB
       Just (f a b)

readInt :: [Char] -> Maybe Int
readInt str = case reads str :: [(Int, String)] of
                 [(val, "")] -> Just val
                 _           -> Nothing

getLineLength :: IO Int
getLineLength = do
    str <- getLine
    return (length str)

printAndGetLength :: String -> IO Int
printAndGetLength a = putStrLn a >> return (length a)

concatLines :: Int -> IO String
concatLines a = concat <$> replicateM a getLine

getInt :: IO (Maybe Int)
getInt = do
    a <- getLine
    return (readInt a)

rInt :: Maybe [Char] -> Maybe Int
rInt Nothing = Nothing
rInt (Just a) = case reads a of
    [(value, "")] -> Just value
    _ -> Nothing

myDuplicate :: [a] -> Int -> [a]
myDuplicate dup n = concat $ replicate n dup

myPrintLine :: Int -> Int -> String
myPrintLine _ 1 = "+"
myPrintLine a b
    | a == b = '+' : nextPrintLine
    | otherwise = '-' : nextPrintLine
    where nextPrintLine = myPrintLine a (b - 1)

myFillBox :: Int -> Int -> String
myFillBox _ 1 = "|"
myFillBox a b
    | a == b = '|' : nextFillBox
    | otherwise = ' ' : nextFillBox
    where nextFillBox = myFillBox a (b - 1)

myPrintBox :: Int -> Int -> String
myPrintBox a 1 = myPrintLine (a * 2) (a * 2)
myPrintBox a b
    | a == b = myPrintLine (a * 2) (a * 2)
    ++ "\n" ++ nextPrintBox
    | otherwise = myFillBox (a * 2) (a * 2)
    ++ "\n" ++ nextPrintBox
    where nextPrintBox = myPrintBox a (b - 1)

printBox :: Int -> IO ()
printBox a
    | myIsNeg(a) = putStr ""
    | myIsEqZero(a) = putStr ""
printBox a = putStrLn $ (myPrintBox a a)

myParse :: Int -> [Char] -> Int -> Int
myParse number_one "-" number_two = number_one - number_two
myParse number_one "%" number_two = number_one `mod` number_two
myParse number_one "/" number_two = number_one `div` number_two
myParse number_one "*" number_two = number_one * number_two
myParse number_one "+" number_two = number_one + number_two

myProcess :: Maybe Int -> Maybe [Char] -> Maybe Int -> IO Int
myProcess _ _ Nothing = exitWith (ExitFailure 84)
myProcess _ Nothing _ = exitWith (ExitFailure 84)
myProcess Nothing _ _ = exitWith (ExitFailure 84)
myProcess (Just number_one) (Just operator) (Just number_two)
    | "/" <- operator = case safeDiv number_one number_two of
        Just n -> print n >> return (0)
        Nothing -> exitWith (ExitFailure 84)
    | otherwise = print (myParse number_one operator number_two) >>
                     return (0)

main :: IO Int
main = do
    args <- getArgs
    let number_one = rInt (safeNth args 0)
    let number_two = rInt (safeNth args 2)
    let operator = safeNth args 1
    myProcess number_one operator number_two