-- Testing fpa functionality

-- imports
import System.Environment
import System.IO 
import Data.List
import Data.Maybe
import Control.Monad

--for parsing forced partial assignment--
parseFpa :: [String] -> [String]
parseFpa [] = ["Error"]
parseFpa ("forced partial assignment:":xs) = getFpa xs
parseFpa ("":xs) = parseFpa xs
parseFpa (x:xs) = ["Error"]

getFpa :: [String] -> [String]
getFpa ("":xs) = getFpa xs
getFpa ("forbidden machine:":xs) = ["forbidden machine:"]
getFpa (x:xs) = if (fpaOkay x) == 0 && fpaOkay2 (x !! 1) xs == True && fpaOkay2 (x !! 3) xs == True then getFpa xs else if (fpaOkay x) == 1 && fpaOkay2 (x !! 1) xs == True && fpaOkay2 (x !! 3) xs == True then ["invalid machine/task"] else if fpaOkay2 (x !! 1) xs == False then ["partial assignment error"] else if fpaOkay2 (x !! 3) xs == False then ["partial assignment error"] else ["Error while parsing input file"]
{-
((((x !! 0 == '(') && (length $ filter (==',') x) == 1 && (last x == ')')) && ((length x) - 3) /= 5 && ((fromJust $ findIndex (==',') x) /= 2) && ((last $ init x) /= ',')) || ((length x) == 5 && ((validMach (x !! 1) == False) || (validTask (x !! 3) == False))))
-}
fpaOkay :: String -> Int
fpaOkay x
  | (length x == 5) && ( (x !! 0 ) == '(' ) && (validMach (x !! 1) == True) && ( (x !! 2) == ',') && (validTask (x !! 3) == True) && ( (x !! 4) == ')' )  = 0
  | (x !! 0 == '(') && (last x == ')')  && ((countOccurrences x ',') == 1) && ( (length ((split x) !! 0) /= 1) || (length ((split x) !! 1) /= 1) || (validMach (x !! 2) == False) || (validTask (x !! 4) == False) ) = 1
  | otherwise = 2

fpaOkay2 :: Char -> [String] -> Bool
fpaOkay2 x [] = True
fpaOkay2 x (y:ys) = if y == "forbidden machine:" then True else if fpaOkay3 x y && fpaOkay2 x ys then True else False

fpaOkay3 :: Char -> String -> Bool
fpaOkay3 x "" = True
fpaOkay3 x y = if elem x y then False else True

countOccurrences :: String -> Char -> Int
countOccurrences str c = length $ filter (== c) str

-- @ https://stackoverflow.com/questions/46580924/haskell-splitting-a-string-by-delimiter
split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

--end parsing forced partial assignment--

validMach :: Char -> Bool
validMach '1' = True
validMach '2' = True
validMach '3' = True
validMach '4' = True
validMach '5' = True
validMach '6' = True
validMach '7' = True
validMach '8' = True
validMach _ = False

validTask :: Char -> Bool
validTask 'A' = True
validTask 'B' = True
validTask 'C' = True
validTask 'D' = True
validTask 'E' = True
validTask 'F' = True
validTask 'G' = True
validTask 'H' = True
validTask _ = False

parseName :: [String] -> [String]
parseName [] = ["Error"]
parseName ("Name:":xs) = getName xs
parseName ("":xs) = parseName xs
parseName (x:xs) = ["Error"]

getName :: [String] -> [String]
getName ("":xs) = getName xs
getName (x:xs) = if (nameOkay x) == True then x:xs else ["Error"]

nameOkay :: String -> Bool
nameOkay [] = True
nameOkay (x:xs) = if x == ' ' then False else nameOkay xs

readIC :: String -> (Int, Char)
readIC x = (y,z) 
  where 
    w = [x !! 1]
    y = read w :: Int
    z = x !! 3

--read functions read from file contents and return a list with the proper types inside
readTupleIC :: [String] -> [(Int, Char)]
readTupleIC [] = []
readTupleIC (x:xs) = (readIC x) : readTupleIC xs


main = do
    -- Test cases
  let normal = ["Name:", "test", "forced partial assignment:", "(1,A)", "(2,B)", "(3,C)", "forbidden machine:"]
  let invalid = ["Name:", "test", "forced partial assignment:", "(1000,A)", "(2,B)", "(3,C)", "forbidden machine:"]
  let pae1 = ["Name:", "test", "forced partial assignment:", "(1,A)", "(1,B)", "(3,C)", "forbidden machine:"]
  let pae2 = ["Name:", "test", "forced partial assignment:", "(1,A)", "(2,A)", "(3,C)", "forbidden machine:"]
  let err1 = ["Name:", "test", "forced partial assignment:", "(1,A", "(2,B)", "(3,C)", "forbidden machine:"]
  let err2 = ["Name:", "test", "forced partial assignment:", "(1,A)", "(2,B)", "(3,C)", "4,Y", "forbidden machine:"]

  let nameTaken = parseName pae1
  let curTest = "err2"
  let name = nameTaken !! 0
 -- print nameTaken

  -- Init
  let fpaTaken = parseFpa $ tail nameTaken
  print fpaTaken
 -- print fpaTaken
  let tempFpa1 = takeWhile(/="forbidden machine:") nameTaken
  let tempFpa2 = filter(/="") $ takeWhile(/="forced partial assignment:") $ reverse tempFpa1
  let fpa = if fpaTaken == ["forbidden machine:"] then reverse $ readTupleIC tempFpa2 else [(-1,'R')]

  putStrLn ("Testing " ++ curTest)
  print fpa

