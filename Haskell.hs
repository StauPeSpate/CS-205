import Test.QuickCheck
import Data.Char
import Data.String

--Question 1
max1 :: (Ord a) => a -> a -> a -> a
max1 x y z
        | x > y && x > z = x
        | y > x && y > z = y
        | otherwise = z

max2 :: (Ord a) => a -> a -> a -> a
max2 x y z = max2' (x:(y:(z:[])))

max2' :: (Eq a, Ord a) => [a] -> a
max2' (x:xs)
        | xs == [] = x
        | x > max2' xs = x
        | otherwise = max2' xs

checkcorrectness x y z =
        max1 x y z == max2 x y z

--Question 2
luigi :: Float -> Float -> Float
luigi diam topp = truncate' ((diameterPrice diam + toppingsPrice topp) * 1.6)

diameterPrice :: Float -> Float
diameterPrice diam = ((pi * (diam/2) * (diam/2)) * 0.002)

toppingsPrice :: Float -> Float
toppingsPrice amount = (0.6 * amount)

truncate' :: Float -> Float
truncate' x = (fromIntegral (floor (x * t))) / t
        where t = 10^2

--Pizza Bambini costs £6.32
--Pizza Famiglia costs £4.49
--Pizza Bambini costs more

--Question 3
counta :: [Char] -> Int
counta input = length (counta' input)

counta' :: [Char] -> [Char]
counta' ys = [x | x <- ys, (isDigit x)]

countb :: (Char -> Bool) -> [Char] -> Int
countb f input = length ([x | x <- input, f (x)])

countc :: [Char] -> Int
countc input = countc' input 0

countc' :: [Char] -> Int -> Int
countc' xs y
        | xs == "" = 0
        | tail xs == [] && isDigit (head xs) = y + 1
        | tail xs == [] = y
        | isDigit (head xs) = countc' (tail xs) (y+1)
        | otherwise = countc' (tail xs) y

--Question 5
won :: [Int] -> Bool
won (p:piles)
        | not (p == 0) = False
        | piles == [] = True
        | otherwise = won piles

validMove :: [Int] -> Int -> Int -> Bool
validMove piles pile amount
                | (pile > (length (piles) - 1)) = False
                | (piles!!pile - amount) >= 0 = True
                | amount == 0 = False
                | otherwise = False

takeAway :: [Int] -> Int -> Int -> [Int]
takeAway piles pile amount = takeAway' piles pile amount []

takeAway' :: [Int] -> Int -> Int -> [Int] -> [Int]
takeAway' piles pile amount opiles
                                | pile > 0 = takeAway' (tail piles) (pile - 1) amount (opiles ++ [head piles])
                                | otherwise = opiles ++ [((head piles) - amount)] ++ (tail piles)

getMove1 :: [Int] -> IO (Int,Int)
getMove1 xs = do
                putStr "Enter your move (Int, Int): "
                x <- getChar
                getChar
                y <- getChar 
                return (read [x] :: Int,read [y] :: Int)

getMove2 :: [Int] -> IO (Int,Int)
getMove2 xs = do
                putStr "Enter your pile: "
                x <- getLine
                putStr "Enter your amount: "
                y <- getLine
                if (x == "") || (y == "") then do
                                                putStrLn "Please enter a value"
                                                getMove2 xs
                else if validMove xs (read x :: Int) (read y :: Int) then do
                                let outputString = "(" ++ x ++ "," ++ y ++ ")"
                                putStr outputString
                                return (read x :: Int,read y :: Int)
                        else do
                                putStrLn "Please enter a valid move"
                                getMove2 xs

displayGame :: [Int] -> IO()
displayGame xs = displayGameaux xs 0
                
displayGameaux :: [Int] -> Int -> IO()
displayGameaux xs y = do
                        if not (xs == []) then do
                                putStr (show y ++ ": ")
                                displayGame' (head xs)
                                putStrLn ""
                                displayGameaux (tail xs) (y+1)
                        else putStr ""

displayGame' :: Int -> IO()
displayGame' x = do
                        if x > 0 then do
                                putStr "*"
                                displayGame' (x-1)
                        else putStr ""

nim :: IO()
nim = do
        putStr "Player 1's name: "
        player1name <- getLine
        putStr "Player 2's name: "
        player2name <- getLine
        putStr "Enter the pile configuration: "
        piles <- getLine
        nimaux player1name player2name piles

nimaux :: String -> String -> String -> IO()
nimaux p1name p2name piles = do
                                let currentPile = splitPiles (piles)
                                playerTurn p1name p2name currentPile 0

playerTurn :: String -> String -> [Int] -> Int -> IO()
playerTurn p1name p2name piles currentTurn = do
                                                if (currentTurn == 0) then do
                                                        putStrLn ""
                                                        putStrLn p1name
                                                        displayGame piles
                                                        putStrLn ""
                                                        currentMove <- getMove2 piles
                                                        let newPiles = takeAway piles (fst currentMove) (snd currentMove)
                                                        putStrLn ""
                                                        if won newPiles == True then do
                                                                putStrLn ""
                                                                let winString = p1name ++ " has Won!"
                                                                putStrLn winString
                                                        else
                                                                playerTurn p1name p2name newPiles 1
                                                else do
                                                        putStrLn ""
                                                        putStrLn p2name
                                                        displayGame piles
                                                        putStrLn ""
                                                        currentMove <- getMove2 piles
                                                        let newPiles = takeAway piles (fst currentMove) (snd currentMove)
                                                        putStrLn ""
                                                        if won newPiles == True then do
                                                                putStrLn ""
                                                                let winString = p2name ++ " has Won!"
                                                                putStrLn winString
                                                        else
                                                                playerTurn p1name p2name newPiles 0

splitPiles :: String -> [Int]
splitPiles (xs)
                | xs == "" = [5,4,3,6]
                | otherwise = createPiles xs []

createPiles :: String -> [Int] -> [Int]
createPiles (input) current
                        | input == "" = current
                        | otherwise = createPiles (tail input) (current ++ [(read [head input] :: Int)])

--Main> nim
--Player 1's name: Samuel
--Player 2's name: Bogdan
--Enter the pile configuration: 1576
--
--Samuel
--0: *
--1: *****
--2: *******
--3: ******
--
--Enter your pile: 1
--Enter your amount: 5
--(1,5)
--
--Bogdan
--0: *
--1:
--2: *******
--3: ******
--
--Enter your pile: 2
--Enter your amount: 3
--(2,3)
--
--Samuel
--0: *
--1:
--2: ****
--3: ******
--
--Enter your pile: 0
--Enter your amount: 1
--(0,1)
--
--Bogdan
--0:
--1:
--2: ****
--3: ******

--Question 6
stratB :: [Int] -> (Int, Int)
stratB piles = stratB' piles 0

stratB' :: [Int] -> Int -> (Int, Int)
stratB' (p:piles) pile 
                | p == 0 = stratB' piles (pile+1)
                | otherwise = (pile,p)

stratI :: [Int] -> (Int, Int)
stratI piles = stratI' piles 0

stratI' :: [Int] -> Int -> (Int, Int)
stratI' (p:piles) pile
                | (not (p == 0)) && ((length piles == 1)) = (pile,p-1)
                | p == 0 = stratI' piles (pile+1)
                | otherwise = (pile,p)
--This strategy plays like Strat B, unless there are 2 piles remaining, where it takes all but 1
--Leaving the player in a losing position


--Main> nimAI stratI
--Player 1's name: Samuel
--Enter the pile configuration:

--Samuel
--0: *****
--1: ****
--2: ***
--3: ******

--Enter your pile: 0
--Enter your amount: 5
--(0,5)
                
--BOT
--0:
--1: ****
--2: ***
--3: ******    

--Samuel
--0:
--1:
--2: ***
--3: ******
                
--Enter your pile: 3
--Enter your amount: 5
--(3,5)

--BOT
--0:
--1:
--2: ***
--3: * 

--Samuel
--0:
--1:
--2: *
--3: *

--Enter your pile: 2
--Enter your amount: 1
--(2,1)
                
--BOT
--0:
--1:
--2:
--3: *

--BOT has Won!

nimAI :: ([Int] -> (Int, Int)) -> IO()
nimAI strat = do
        putStr "Player 1's name: "
        player1name <- getLine
        putStr "Enter the pile configuration: "
        piles <- getLine
        nimAIaux strat player1name piles

nimAIaux :: ([Int] -> (Int, Int)) -> String -> String -> IO()
nimAIaux strat p1name piles = do
                                let currentPile = splitPiles (piles)
                                playerAITurn strat p1name currentPile 0

playerAITurn :: ([Int] -> (Int, Int)) -> String -> [Int] -> Int -> IO()
playerAITurn strat p1name piles currentTurn = do
                                                if (currentTurn == 0) then do
                                                        putStrLn ""
                                                        putStrLn p1name
                                                        displayGame piles
                                                        putStrLn ""
                                                        currentMove <- getMove2 piles
                                                        let newPiles = takeAway piles (fst currentMove) (snd currentMove)
                                                        putStrLn ""
                                                        if won newPiles == True then do
                                                                putStrLn ""
                                                                let winString = p1name ++ " has Won!"
                                                                putStrLn winString
                                                        else
                                                                playerAITurn strat p1name newPiles 1
                                                else do
                                                        putStrLn ""
                                                        putStrLn "BOT"
                                                        displayGame piles
                                                        putStrLn ""
                                                        let currentMove = (strat piles)
                                                        let newPiles = takeAway piles (fst currentMove) (snd currentMove)
                                                        putStrLn ""
                                                        if won newPiles == True then do
                                                                putStrLn ""
                                                                let winString = "BOT" ++ " has Won!"
                                                                putStrLn winString
                                                        else
                                                                playerAITurn strat p1name newPiles 0