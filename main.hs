import System.Environment
import System.IO
--import Data.List
--import Data.Maybe
import Control.Monad

import Brute

---------read functions----------------
-- helpers for read function do actual reading
readIC :: String -> (Int, Char)
readIC x = (y,z)
  where
    w = [x !! 1]
    y = read w :: Int
    z = x !! 3

readCC :: String -> (Char, Char)
readCC x = (y,z)
  where
    y = x !! 1
    z = x !! 3

readIntList :: [String] -> [Int]
readIntList [] = []
readIntList (" ":xs) = readIntList xs
readIntList (x:xs) = (read x :: Int) : readIntList xs

--read functions read from file contents and return a list with the proper types inside
readTupleIC :: [String] -> [(Int, Char)]
readTupleIC [] = []
readTupleIC (x:xs) = (readIC x) : readTupleIC xs

readTupleCC :: [String] -> [(Char, Char)]
readTupleCC [] = []
readTupleCC (x:xs) = (readCC x) : readTupleCC xs

readIntListList :: [String] -> [[Int]]
readIntListList [] = []
readIntListList (x:xs) = (readIntList $ words x) : readIntListList xs
-------end read functions----------------

------for parsing name-----------
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
------end parsing name-----------

--for parsing forced partial assignment--
parseFpa :: [String] -> [String]
parseFpa [] = ["Error while parsing input file"]
parseFpa ("forced partial assignment:":xs) = getFpa xs
parseFpa ("":xs) = parseFpa xs
parseFpa (x:xs) = ["Error while parsing input file"]

getFpa :: [String] -> [String]
getFpa ("":xs) = getFpa xs
getFpa ("forbidden machine:":xs) = ["forbidden machine:"]
getFpa (x:xs) = if (fpaOkay x) == 0 && fpaOkay2 (x !! 1) xs == True && fpaOkay2 (x !! 3) xs == True then getFpa xs else if (fpaOkay x) == 1 then ["invalid machine/task"] else if fpaOkay2 (x !! 1) xs == False then ["partial assignment error"] else if fpaOkay2 (x !! 3) xs == False then ["partial assignment error"] else ["Error while parsing input file"]

fpaOkay :: String -> Int
fpaOkay x = if (length x == 5) && (x !! 0 == '(') && (validMach (x !! 1) == True) && (x !! 2 == ',') && (validTask (x !! 3) == True) && (x !! 4 == ')') then 0 else if (((length x == 5) && (x !! 0 == '(') && (x !! 2 == ',') && (x !! 4 == ')')) && ((validMach (x !! 1) == False) || (validTask (x !! 3) == False))) || ( (length x /= 5) && (x !! 0 == '(') && (last x == ')') && ((length $ filter(==',') x) == 1)) then 1 else 2

fpaOkay2 :: Char -> [String] -> Bool
fpaOkay2 x [] = True
fpaOkay2 x [y] = True
fpaOkay2 x (y:ys) = if y == "forbidden machine:" then True else if fpaOkay3 x y && fpaOkay2 x ys then True else False

fpaOkay3 :: Char -> String -> Bool
fpaOkay3 x "" = True
fpaOkay3 x y = if elem x y && length y == 5 then False else True
--end parsing forced partial assignment--


--for parsing forbidden machine--
findFm :: [String] -> [String]
findFm [] = ["Error"]
findFm ("forbidden machine:":xs) = "forbidden machine:":xs
findFm (x:xs) = findFm xs

parseFm :: [String] -> [String]
parseFm [] = ["Error"]
parseFm ("forbidden machine:":xs) = getFm xs
parseFm ("":xs) = parseFm xs
parseFm (x:xs) = ["Error while parsing input file"]

getFm :: [String] -> [String]
getFm ("":xs) = getFm xs
getFm ("too-near tasks:":xs) = ["too-near tasks:"]
getFm (x:xs) = if (fmOkay x) == 0 then getFm xs else if fmOkay x == 1 then ["invalid machine/task"] else ["Error while parsing input file"]

fmOkay :: String -> Int
fmOkay x = if ((length x == 5) && (x !! 0 == '(') && (validMach (x !! 1) == True)
          && (x !! 2 == ',') && (validTask (x !! 3) == True) && (x !! 4 == ')')) then 0 else if
          (((length x == 5) && (x !! 0 == '(') && (x !! 2 == ',') && (x !! 4 == ')')) && ((validMach (x !! 1) == False) || (validTask (x !! 3) == False))) then 1 else
          2
--end parsing forbidden machine--

-----for too-near tasks-----
findTnt :: [String] -> [String]
findTnt [] = ["Error"]
findTnt ("too-near tasks:":xs) = "too-near tasks:":xs
findTnt (x:xs) = findTnt xs

parseTnt :: [String] -> [String]
parseTnt [] = ["Error while parsing input file"]
parseTnt ("too-near tasks:":xs) = getTnt xs
parseTnt ("":xs) = parseTnt xs
parseTnt (x:xs) = ["Error while parsing input file"]

getTnt :: [String] -> [String]
getTnt ("":xs) = getTnt xs
getTnt ("machine penalties:":xs) = ["machine penalties:"]
getTnt (x:xs) = if (tntOkay x) == 0 then getTnt xs else if tntOkay x == 1 then ["invalid machine/task"] else ["Error while parsing input file"]

tntOkay :: String -> Int
tntOkay x = if ((length x == 5) && (x !! 0 == '(') && (validTask (x !! 1) == True)
          && (x !! 2 == ',') && (validTask (x !! 3) == True) && (x !! 4 == ')')) then 0 else if
          (((length x == 5) && (x !! 0 == '(') && (x !! 2 == ',') && (x !! 4 == ')')) && ((validTask (x !! 1) == False) || (validTask (x !! 3) == False))) then 1 else
          2
--------end too-near tasks-------

-----for machine penalties-----
findMp :: [String] -> [String]
findMp [] = ["Error"]
findMp ("machine penalties:":xs) = "machine penalties:":xs
findMp (x:xs) = findMp xs

parseMp :: [String] -> [String]
parseMp [] = ["Error while parsing input"]
parseMp ("machine penalties:":xs) = getMp xs
parseMp ("":xs) = parseMp xs
parseMp (x:xs) = ["Error while parsing input file"]

getMp :: [String] -> [String]
getMp ("":xs) = getMp xs
getMp ("too-near penalities":xs) = ["too-near penalities"]
getMp (x:xs) = if (mpOkay (words x)) == False then ["invalid penalty"] else if (length $ words x) /= 8 then ["machine penalty error"] else if (mpOkay (words x)) == True then getMp xs else ["Error while parsing input file"]

mpOkay :: [String] -> Bool
mpOkay [] = True
mpOkay (x:xs) = if (isInt x == True) && (mpOkay xs == True) then True
                else False


isInt :: [Char] -> Bool
isInt (x:xs) = if (x == '0' && length xs == 0) || (elem x ['1'..'9'] && isNumeric xs) then True else False

isNumeric :: [Char] -> Bool
isNumeric [] = True
isNumeric (x:xs) = if elem x ['0'..'9'] && isNumeric xs then True else False
--------end machine penalties-------


-----for too-near penalities-----
findTnp :: [String] -> [String]
findTnp [] = ["Error while parsing input file"]
findTnp ("too-near penalities":xs) = "too-near penalities":xs
findTnp (x:xs) = findTnp xs

parseTnp :: [String] -> [String]
parseTnp [] = ["Error while parsing input file"]
parseTnp ("too-near penalities":xs) = getTnp xs
parseTnp ("":xs) = parseTnp xs
parseTnp (x:xs) = ["Error while parsing input file"]

getTnp :: [String] -> [String]
getTnp ("":xs) = getTnp xs
getTnp [] = ["cool"]
getTnp (x:xs) = if tnpOkay x then if tnpOkay2 x == False then ["invalid penalty"] else if tnpOkay3 x == False then ["invalid task"] else getTnp xs else ["Error while parsing input file"]


-- check format --
tnpOkay :: String -> Bool
tnpOkay x = if ((x !! 0 == '(') && (x !! 2 == ',') && (x !! 4 == ',') && (last x ==')')) then True else False

-- check penalty --
tnpOkay2 :: String -> Bool
tnpOkay2 s = if isInt $ ((wordsWhen (==',') $ removeParenth s) !! 2) then True else False

-- check valid --
tnpOkay3 :: String -> Bool
tnpOkay3 s = if validTask (s !! 3) && validTask (s !! 1) then True else False


--------end too-near penalities-------

----for valid machine and task---------
validMach :: Char -> Bool
validMach c = if elem c ['1'..'8'] then True else False

validMachI :: Int-> Bool
validMachI d = if elem d [0..8] then True else False


validTask :: Char -> Bool
validTask c = if elem c ['A'..'H'] then True else False
----end valid machine and task---------

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

removeParenth :: [Char] -> [Char]
removeParenth [] = []
removeParenth [x] = []
removeParenth xs = tail $ init xs

readTNP :: [String] -> [(Char, Char, Int)]
readTNP [] = []
readTNP (x:xs) = readTNPs x : readTNP xs

readTNPs :: String -> (Char, Char, Int)
readTNPs x = (v,w,z)
  where
  a = wordsWhen (==',') $ removeParenth x
  v = (a !! 0) !! 0
  w = (a !! 1) !! 0
  b = (a !! 2)
  z = read b :: Int

getOutput ::  [(Int, Char)] -> [(Int, Char)] -> [(Char, Char)] -> [[Int]] -> [(Char, Char, Int)] -> [String] -> String
getOutput fpa fm tnt mp tnp err = if length err /= 0 then (last err) else outputSolution fpa fm tnt mp tnp

appendErr :: String -> [String] -> [String]
appendErr s ls = s : ls

main = do
  -- get inputs and create a list of file's lines
  args <- getArgs
  let argsIn = args !! 0
  let argsOut = args !! 1
  contents <- readFile argsIn
  let lineyGuys = lines contents
  let trimsr = reverse . dropWhile (=='\r') . reverse
  let trimws = reverse . dropWhile (==' ') . reverse
  let trimmedLines = map trimsr lineyGuys
  let fullTrimmed = map trimws trimmedLines

  -- find file name
  let nameTaken = parseName fullTrimmed
  let name = nameTaken !! 0

  --find forced partial assignments
  let fpaTaken = parseFpa $ tail nameTaken
  let tempFpa1 = takeWhile(/="forbidden machine:") nameTaken
  let tempFpa2 = filter(/="") $ takeWhile(/="forced partial assignment:") $ reverse tempFpa1
  let fpa = if fpaTaken == ["forbidden machine:"] then reverse $ readTupleIC tempFpa2 else [(-1,'R')]

  --find forbidden machine
  let findingFm = findFm nameTaken
  let fmTaken = parseFm findingFm
  let tempFm1 = takeWhile(/="too-near tasks:") findingFm
  let tempFm2 = filter(/="") $ takeWhile(/="forbidden machine:") $ reverse tempFm1
  let fm = if fmTaken == ["too-near tasks:"] then reverse $ readTupleIC tempFm2 else [(-1,'R')]


  --find too-near tasks
  let findingTnt = findTnt nameTaken
  let tntTaken = parseTnt findingTnt
  let tempTnt1 = takeWhile(/="machine penalties:") findingTnt
  let tempTnt2 = filter(/="") $ takeWhile(/="too-near tasks:") $ reverse tempTnt1
  let tnt = if tntTaken == ["machine penalties:"] then reverse $ readTupleCC tempTnt2 else [('R','R')]

  --find machine penalties

  let findingMp = findMp nameTaken
  let mpTaken = parseMp findingMp
  let tempMp1 = takeWhile(/="too-near penalities") findingMp
  let tempMp2 = filter(/="") $ takeWhile(/="machine penalties:") $ reverse tempMp1
  let tempMp3 = if mpTaken == ["too-near penalities"] then reverse $ readIntListList tempMp2 else [[-1]]
  let mp = if length tempMp3 == 8 then tempMp3 else [[-1]]
  let mp2 = if (mpTaken == ["invalid penalty"]) then ["invalid penalty"] else if ((mp !! 0) !! 0 ) == -1 then ["machine penalty error"] else ["too-near penalities"]

    --find too-near penalities
  let findingTnp = findTnp nameTaken
  let tnpTaken = parseTnp findingTnp
  let tempTnp1 = takeWhile(/=[]) findingTnp
  let tempTnp2 = filter(/="") $ takeWhile(/="too-near penalities") $ reverse tempTnp1
  let tnp = if tnpTaken == ["cool"] then reverse $ readTNP tempTnp2 else [('R','R',-1)]


  let errList1 = tnpTaken ++ mp2 ++ tntTaken ++ fmTaken ++ fpaTaken ++ (nameTaken!!0):[]

  let it = filter(=="Name:") fullTrimmed
  let cant = filter(=="forced partial assignment:") fullTrimmed
  let have = filter(=="forbidden machine:") fullTrimmed
  let been = filter(=="too-near tasks:") fullTrimmed
  let this = filter(=="machine penalties:") fullTrimmed
  let easy = filter(=="too-near penalities") fullTrimmed
  let areYouKiddingMe = it ++ cant ++ have ++ been ++ this ++ easy
  
  let out = outputSolution fpa fm tnt mp tnp

  theFile <- openFile argsOut WriteMode


  if length areYouKiddingMe /= 6 then hPutStrLn theFile "Error while parsing input file" else if
     last errList1 == "Error while parsing input file" then hPutStrLn theFile $ last errList1 else if
     ((last $ init errList1) /= "forbidden machine:") then hPutStrLn theFile $ last $ init errList1 else if
     ((last $ init $ init errList1) /= "too-near tasks:") then hPutStrLn theFile $ last $ init $ init errList1 else if
     ((last $ init $ init $ init errList1) /= "machine penalties:") then hPutStrLn theFile $ last $ init $ init $ init errList1 else if
     ((last $ init $ init $ init $ init errList1) /= "too-near penalities") then hPutStrLn theFile $last $ init $ init $ init $ init errList1 else if
     ((last $ init $ init $ init $ init $ init errList1) /= "cool") then hPutStrLn theFile $last $ init $ init $ init $ init $ init errList1 else hPutStrLn theFile out
  hClose theFile

