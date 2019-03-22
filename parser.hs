import System.Environment
import System.IO 
import Data.List
import Control.Monad

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

readCCI :: String -> (Char, Char, Int)
readCCI x = (v,w,z) 
  where 
    v = x !! 1
    w = x !! 3
    y = [x !! 5]
    z = read y :: Int

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

readTupleCCI :: [String] -> [(Char, Char, Int)]
readTupleCCI [] = []
readTupleCCI (x:xs) = (readCCI x) : readTupleCCI xs

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
parseFpa [] = ["Error"]
parseFpa ("forced partial assignment:":xs) = getFpa xs
parseFpa ("":xs) = parseFpa xs
parseFpa (x:xs) = ["Error"]

getFpa :: [String] -> [String]
getFpa [] = []
getFpa ("":xs) = getFpa xs
getFpa ("forbidden machine:":xs) = ["forbidden machine:"]
getFpa (x:xs) = if (fpaOkay x) == 0 && (fpaOkay2 (x !! 1) xs == True) && (fpaOkay2 (x !! 3) xs == True) then getFpa xs else if 
                (fpaOkay x) == 1 && (fpaOkay2 (x !! 1) xs == True) && (fpaOkay2 (x !! 3) xs == True) then ["invalid machine/task"] else if
                (fpaOkay x) == 2 && (fpaOkay2 (x !! 1) xs == True) && (fpaOkay2 (x !! 3) xs == True) then ["Error while parsing input file"] else if
                (fpaOkay2 (x !! 1) xs == False) || (fpaOkay2 (x !! 3) xs == False) then ["partial assignment error"] else ["Error while parsing input file"]

fpaOkay :: String -> Int
fpaOkay "" = 0
fpaOkay x = if ((length x == 5) && (x !! 0 == '(') && (validMach (x !! 1) == True) 
          && (x !! 2 == ',') && (validTask (x !! 3) == True) && (x !! 4 == ')')) then 0 else if 
          (((length x == 5) && (x !! 0 == '(') && (x !! 2 == ',') && (x !! 4 == ')')) && ((validMach (x !! 1) == False) || (validTask (x !! 3) == False))) then 1 else
          2

fpaOkay2 :: Char -> [String] -> Bool
fpaOkay2 x [] = True
fpaOkay2 x (y:ys) = if (elem x y == True) then False else fpaOkay2 x ys          
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
parseFm (x:xs) = ["Error"]

getFm :: [String] -> [String]
getFm ("":xs) = getFm xs
getFm ("too-near tasks:":xs) = ["too-near tasks:"]
getFm (x:xs) = if (fmOkay x) == 0 then getFm xs else if fmOkay x == 1 then ["invalid machine/task"] else ["Error"]

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
parseTnt [] = ["Error"]
parseTnt ("too-near tasks:":xs) = getTnt xs
parseTnt ("":xs) = parseTnt xs
parseTnt (x:xs) = ["Error"]

getTnt :: [String] -> [String]
getTnt ("":xs) = getTnt xs
getTnt ("machine penalties:":xs) = ["machine penalties:"]
getTnt (x:xs) = if (tntOkay x) == 0 then getTnt xs else if tntOkay x == 1 then ["invalid machine/task"] else ["Error"]

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
parseMp [] = ["Error"]
parseMp ("machine penalties:":xs) = getMp xs
parseMp ("":xs) = parseMp xs
parseMp (x:xs) = ["Error"]

getMp :: [String] -> [String]
getMp ("":xs) = getMp xs
getMp ("too-near penalities":xs) = ["too-near penalities"]
getMp (x:xs) = if (mpOkay (words x)) == True then getMp xs else ["Error"]

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
findTnp [] = ["Error"]
findTnp ("too-near penalities":xs) = "too-near penalities":xs
findTnp (x:xs) = findTnp xs

parseTnp :: [String] -> [String]
parseTnp [] = ["Error"]
parseTnp ("too-near penalities":xs) = getTnp xs
parseTnp ("":xs) = parseTnp xs
parseTnp (x:xs) = ["Error"]

getTnp :: [String] -> [String]
getTnp ("":xs) = getTnp xs
getTnp [] = []
getTnp (x:xs) = if (tnpOkay x) == True then getTnp xs else ["Error"]

tnpOkay :: String -> Bool
tnpOkay x = if ((length x == 7) && (x !! 0 == '(') && (validTask (x !! 1) == True) 
          && (x !! 2 == ',') && (validTask (x !! 3) == True) && (x !! 4 == ',')) 
          && (validMach (x !! 5) == True) && (x !! 6 ==')') then True else
          False     
--------end too-near penalities-------

----for valid machine and task---------
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

validMachI :: Int-> Bool
validMachI 1 = True
validMachI 2 = True
validMachI 3 = True
validMachI 4 = True
validMachI 5 = True
validMachI 6 = True
validMachI 7 = True
validMachI 8 = True
validMachI _ = False

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
----end valid machine and task---------

main = do
  -- get inputs and create a list of file's lines
  args <- getArgs
  let argsIn = args !! 0
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
  let mp = if mpTaken == ["too-near penalities"] then reverse $ readIntListList tempMp2 else [[-1]]
  
    --find too-near penalities
  let findingTnp = findTnp nameTaken
  let tnpTaken = parseTnp findingTnp
  let tempTnp1 = takeWhile(/=[]) findingTnp
  let tempTnp2 = filter(/="") $ takeWhile(/="too-near penalities") $ reverse tempTnp1
  let tnp = if tnpTaken == [] then reverse $ readTupleCCI tempTnp2 else [('R','R',-1)]
  
  print fpa
  print fpaTaken
  print $ getFpa ["(1,A)", "(2,B)", "(3,C)", "(3,D)", "forbidden machine:"]


  print $ "(1,A)" !! 3

  print name
  print fpa
  print fm
  print tnt
  print mp
  print tnp
  
  
  if (fpaTaken == ["invalid machine/task"]) then putStrLn "invalid machine/task" else putStrLn "lol"

  when (fpaTaken == ["invalid machine/task"]) $ putStrLn "invalid machine/task"
  when (fmTaken == ["invalid machine/task"]) $ putStrLn "invalid machine/task"
  when (tntTaken == ["invalid machine/task"]) $ putStrLn "invalid machine/task"
  when (tnpTaken == ["invalid penalty"]) $ putStrLn "invalid penalty"
  let yep = if name == "Error" || fpa == [(-1,'R')] || fm == [(-1,'R')] || tnt == [('R', 'R')] || mp == [[-1]] || tnp == [('R','R',-1)] then 
            "Error while parsing input file" else "Nothing"

  when (yep == "Error while parsing input file") $ putStrLn "Error while parsing input file"