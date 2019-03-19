{-
File:		    brute.hs
Authors:		Elzanne Venter, Isha Afzaal
Course:		  CPSC 449 - Assignment #2
Date:		    March 11, 2019
Info:	    	Brute force implementation of machine-task assignment problem. Will find and return, by process
			      of elimination, the best machine-task assignment (assuming it exists).
-}

-- Modules
import Data.List
import System.IO

-- All possible assignments
allPossible = permutations "ABCDEFGH"

-- Hard Constraint 1: Ensure the given (mach, task) is in the solution
forcedPairCompare :: (Int,Char) -> String -> Bool
forcedPairCompare (pos, constraintmach) mach =
  if ( (mach !! (pos-1)) == constraintmach) --checks the string at the task position if it matches constraint machine
    then True
    else False

-- Hard Constraint 2: Ensure the given (mach, task) is not in the solution
forbiddenPairCompare :: (Int,Char) -> String -> Bool
forbiddenPairCompare (pos, constraintmach) mach =
  if ( (mach !! (pos-1)) == constraintmach) --checks the string at the task position if it matches constraint machine
    then False
    else True

-- Hard Constraint 3: Ensure that the given (task1, task2) are not adjacent
tooNearCompare :: (Char, Char) -> String -> Bool
tooNearCompare (t1, t2) sol = 
  if (tooNearCheck (t1, t2) sol 0 == True)    -- Cycle through and check for the too-near tasks at each position
    then True                                 -- True return means that the current solution does not have any too-near tasks
    else False                                -- False return means that the current solution contains a too-near pair

-- tooNearCheck: Helper function to tooNearCompare. Given (task1, task2), solution (in string) and a position, check if the machine at
--               at the current position = task1. If it is, check if it is adjacent to task2 and return false if so. If task1 and 2 are not 
--               adjacent, return true.
tooNearCheck :: (Char, Char) -> String -> Int -> Bool
tooNearCheck (t1, t2) sol pos
  | (pos == 8) = True                                               -- Return true if we have cycled through the whole string/solution
  | ((sol !! pos) == t1) && ( (sol !! ((pos + 1) `mod` 8 )) == t2) = False  -- If the current position in str = task1 and the next mach is task2, return false
  | ((sol !! pos) == t1) && ( (sol !! ((pos + 1) `mod` 8 )) /= t2) = True   -- If the current position in str = task1 and the next mach != task2, return true
  | (sol !! pos /= t1) = True && (tooNearCheck (t1, t2) sol (pos+1))  -- Else cycle forward and check the next machine for task1
  | otherwise = True


-- Helper functions
forbiddenHelper :: [(Int,Char)] -> String -> [Bool]
forbiddenHelper (ab) str = [forbiddenPairCompare a str | a <- ab]

forcedHelper :: [(Int,Char)] -> String -> [Bool]
forcedHelper (ab) str = [forcedPairCompare a str | a <- ab]

tooNearHelper :: [(Char, Char)] -> String -> [Bool]
tooNearHelper (ab) str = [tooNearCompare a str | a <- ab]

-- Constraint drivers
hardConstraint1 :: [(Int, Char)] ->[String] -> [String]
hardConstraint1 (xs) (ys) = [ y | y <- ys, and (forcedHelper xs y) ]

hardConstraint2 :: [(Int, Char)] ->[String] -> [String]
hardConstraint2 (xs) (ys) = [ y | y <- ys, and (forbiddenHelper xs y) ]

hardConstraint3 :: [(Char, Char)] -> [String] -> [String]
hardConstraint3 (xs) (ys) = [ y | y <- ys, and (tooNearHelper xs y) ]

-- Main: Used for testing purposes alongside ensuring modularity of our functions
main = do
   putStrLn "Testing for brute force - Hard Constraint 3."
   let x = [('A', 'B')]
   let y = ["ABCDEFGH", "ACBDEFGH", "BDEFGHCA"]
   let z = hardConstraint3 x y
   putStrLn ("Contents: " ++ show z)
