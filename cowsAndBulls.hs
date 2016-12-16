module Main where


import System.Random


data GameState = GameState { numberToGuess :: [Integer], numOfTries :: Integer } deriving (Show)


listInt :: Integer -> [Integer]
listInt 0 = []
listInt num = listInt (num `div` 10) ++ [num `mod` 10]


checkList :: [Integer] -> Bool
checkList list = case list of
  [] -> True
  (x:xs) -> (x `notElem` xs && checkList xs)


generate :: IO GameState
generate = do
  num <- randomRIO(1001, 9999)
  if checkList (listInt num)
     then do
       return $ GameState (listInt num) 20
     else
       generate


getInput :: IO String
getInput = getLine


strInt :: String -> [Integer]
strInt input = map (read . (:[])) input


checkCow :: [Integer] -> [Integer] -> Integer
checkCow input guess =
  (go input guess 0) - (checkBull input guess)
  where
    go :: [Integer] -> [Integer] -> Integer -> Integer
    go _ [] acc = acc
    go input (x:xs) acc = if elem x input
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

verifyInput :: GameState -> String -> IO GameState
verifyInput gs num = do
  let input = strInt num
      cow = checkCow input (numberToGuess gs)
      bull = checkBull input (numberToGuess gs)
  if bull == 4
     then do
       putStrLn "You won!"
       runGame
     else do
       printCowBull cow bull
       if numOfTries gs == 1
          then do
            putStrLn $ "You lose! The number is " ++ (show $ numberToGuess gs)
            runGame
          else
            do
              return $ decrGameState gs
      
  
decrGameState :: GameState -> GameState
decrGameState (GameState numG numT) = GameState numG (numT - 1)


printCowBull :: Integer -> Integer -> IO ()
printCowBull cow bull = putStrLn $ (show cow) ++ " cows " ++ (show bull) ++ " bulls"


runGame :: IO GameState
runGame = do
  putStrLn "Game started! The computer has thought of a 4 digit number without repetitions."
  gs <- generate
  gameLoop gs


gameLoop :: GameState -> IO GameState
gameLoop gs = do
  input <- getInput
  gs1 <- verifyInput gs input
  gameLoop gs1
   


main :: IO()
main = do
  runGame
  return ()