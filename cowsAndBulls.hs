module Main where


import System.Random


data GameState = GameState { numberToGuess :: [Integer], numOfTries :: Integer } deriving (Show)


numOfChances :: Integer
numOfChances = 20


numLength :: Integer
numLength = 4


listInt :: Integer -> [Integer]
listInt 0 = []
listInt num = listInt (num `div` 10) ++ [num `mod` 10]


checkList :: [Integer] -> Bool
checkList list = case list of
  [] -> True
  (x:xs) -> x `notElem` xs && checkList xs


generate :: IO GameState
generate = do
  num <- randomRIO(1023, 9678)
  if checkList (listInt num)
     then
       return $ GameState (listInt num) numOfChances
     else
       generate


getInput :: IO String
getInput = getLine


strInt :: String -> [Integer]
strInt = map (read . (:[]))


checkCow :: [Integer] -> [Integer] -> Integer
checkCow input guess =
  go input guess 0 - checkBull input guess
  where
    go :: [Integer] -> [Integer] -> Integer -> Integer
    go _ [] acc = acc
    go input (x:xs) acc = if x `elem` input
                             then
                               go input xs (acc + 1)
                             else
                               go input xs acc


checkBull :: [Integer] -> [Integer] -> Integer
checkBull [] [] = 0
checkBull (x:xs) (y:ys) = if x == y
                             then
                               1 + checkBull xs ys
                             else
                               checkBull xs ys


validateInput :: [Integer] -> Bool
validateInput input = length input > fromIntegral numLength || not (checkList input)


valCowBull :: GameState -> [Integer] -> (Integer, Integer)
valCowBull gs input = (checkCow input (numberToGuess gs), checkBull input (numberToGuess gs))


verifyGameOver :: GameState -> String -> IO GameState
verifyGameOver gs num = do
  let input = strInt num
  if validateInput input
     then do
       putStrLn "Incorrect input!"
       input <- getInput
       verifyGameOver gs input
     else do
       let (cow, bull) = valCowBull gs input
       winOrLose (cow, bull) gs


winOrLose :: (Integer, Integer) -> GameState -> IO GameState
winOrLose (cow, bull) gs = if bull == fromIntegral numLength
                             then do
                               putStrLn "You won!"
                               runGame
                             else do
                               printCowBull cow bull gs
                               gameOver gs


joiner :: [Integer] -> Integer
joiner = read . concatMap show


gameOver :: GameState -> IO GameState
gameOver gs = if numOfTries gs == 1
                 then do
                   putStrLn $ "You lose! The number is " ++ show (joiner (numberToGuess gs))
                   runGame
                 else
                   return $ decrGameState gs


decrGameState :: GameState -> GameState
decrGameState (GameState numG numT) = GameState numG (numT - 1)


printCowBull :: Integer -> Integer -> GameState -> IO ()
printCowBull cow bull gs = putStrLn $ show cow ++ " cows " ++ show bull ++ " bulls and " ++ show (numOfTries gs - 1) ++ " tries left"


runGame :: IO GameState
runGame = do
  putStrLn "Game started! The computer has thought of a 4 digit number without repetitions."
  gs <- generate
  gameLoop gs


gameLoop :: GameState -> IO GameState
gameLoop gs = do
  input <- getInput
  gs1 <- verifyGameOver gs input
  gameLoop gs1
   


main :: IO()
main = do
  runGame
  return ()
