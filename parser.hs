-- Parser.hs
-- CPSC 449 will parse input file for machine-task assignment problem

-- Modules
import Data.List
import System.IO



-- Helper functions
checkLinesHelper :: String -> String -> Bool
checkLinesHelper prev str
   | str == "Name" = readData str 
   | str == "forced partial assignment:" = readData str
   | str == "forbidden machine:" = readData str 
   | str == "too-near tasks:" = readData str
   | str == "machine penalties:" = readData str
   | str == "too-near penalities" = readData str
   | otherwise = checkDataLine prev str

checkDataLine :: String -> String -> Bool
checkDataLine prev str
   | prev == "Name" && str /= [] = 

-- Functions
checkLines :: [String] -> [Bool]		-- Check each line
checkLines (xs) = [checkLinesHelper x | x <- xs]

-- Driver
main = do
	args <- getArgs						 -- Get the command line arguments
    content <- readFile (args !! 0)		 -- Open the given txt file
    if null content then do				 -- Check if the file exists
    	putStrLn "The given txt file was empty"
    	return ()
    let fileLines = lines content		 -- Read in each line of the file, which will be the type [String]
    let (fileCheck) = checkLines fileLines -- Check each file line for error
    if (fileCheck == True) then do
    	putStrLn "Successfully parsed"
    	return ()
    else
    	putStrLn "Error found in file"
    	return ()