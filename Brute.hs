{-
File:		Brute.hs
Author:		Isha Afzaal, Elzanne Venter
UCID: 		30016250, 30016292
Course:		CPSC 449 - Assignment #2
Date:		March 11, 2019
Info:		Brute force implementation of machine-task assignment problem. Will find and return, by process
			of elimination, the best machine-task assignment (assuming it exists).
-}
module Brute where

-- Modules
import Data.List
import Data.Maybe


{-
Function for Forced Assignments
Input: a tuple (position, machine) and an assignment
Output: True if the machine is in the correct position in the assignment
        False otherwise
-}
forcedPairCompare :: (Int,Char) -> String -> Bool
forcedPairCompare (pos, constraintmach) mach =
  if ( (mach !! (pos-1)) == constraintmach) --checks the string at the task position if it matches constraint machine
    then True
    else False

{-
Function for Forbidden Assignments
Input: a tuple (position, machine) and an assignment
Output: False if the machine is in the correct position in the assignment
        True otherwise
-}
forbiddenPairCompare :: (Int,Char) -> String -> Bool
forbiddenPairCompare (pos, constraintmach) mach =
  if ( (mach !! (pos-1)) == constraintmach) --checks the string at the task position if it matches constraint machine
        then False
        else True


{-
Function for Too Near Assignments not allowed
Input: a tuple (machine,machine) and an assignment
Output: True if the machines in the assignment are not too near
        False if the machines in the assignment are too near
-}
tooNearCompare :: (Char, Char) -> String -> Bool
tooNearCompare (t1, t2) sol =
  if (tooNearCheck (t1, t2) sol 0 == True)
    then True
    else False

{-
Function for Too Near Assignments penalties
Input: a tuple (machine,machine) and an assignment
Output: False if the machines in the assignment are not too near
        True if the machines in the assignment are too near
-}
tooNearComparePen :: (Char, Char) -> String -> Bool
tooNearComparePen (t1, t2) sol =
    if (tooNearCheck (t1, t2) sol 0 == True)    -- Cycle through and check for the too-near tasks at each position
      then False                                 -- True return means that the current solution does not have any too-near tasks
      else True

{-
Helper function to tooNearCompare and tooNearCompare Pen
Given (task1, task2), solution (in string) and a position, check if the machine
at the current position = task1. If it is, check if it is adjacent to task2 and return false if so. If task1 and 2 are not
adjacent, return true.
-}
tooNearCheck :: (Char, Char) -> String -> Int -> Bool
tooNearCheck (t1, t2) sol pos
        | (pos == 8) = True                                               -- Return true if we have cycled through the whole string/solution
        | ((sol !! pos) == t1) && ((sol !! ((pos + 1) `mod` 8)) == t2) = False  -- If the current position in str = task1 and the next mach is task2, return false
        | ((sol !! pos) == t1) && ((sol !! ((pos + 1) `mod` 8)) /= t2) = True   -- If the current position in str = task1 and the next mach != task2, return true
        | (sol !! pos /= t1) = True && (tooNearCheck (t1, t2) sol (pos+1))  -- Else cycle forward and check the next machine for task1
        | otherwise = True

{-
Function for calculating a machines equivalent position in the penalty matrix
Helper for getting the total cost
-}
costMachNumber :: Char -> Int
costMachNumber mach
    | mach == 'A' = 0
    | mach == 'B' = 1
    | mach == 'C' = 2
    | mach == 'D' = 3
    | mach == 'E' = 4
    | mach == 'F' = 5
    | mach == 'G' = 6
    | mach == 'H' = 7

{-
Function for finding the machines equivalent position in the penalty matrix, in a list
-}
assignmentToNumber :: String -> [Int]
assignmentToNumber assignment = [costMachNumber mach | mach <- assignment]

{-
Function to convert list of toonearpen tuples to list of tuples with just the too near machines
-}
tupleSizeAdjust :: [(Char,Char,Int)] -> [(Char,Char)]
tupleSizeAdjust tupleList = [ tupleGetTask x | x <- tupleList ]

{-
Function to convert toonearpen tuples to tuple with just the too near machines
-}
tupleGetTask :: (Char,Char,Int) -> (Char,Char)
tupleGetTask (t1,t2,pen) = (t1,t2)

{-
Function to convert toonearpen tuples to just the penalty
-}
tupleGet3rd :: (Char,Char,Int) -> Int
tupleGet3rd (t1,t2,pen) = pen

{-
Function that helps hardConstraint2
-}
forbiddenHelper :: [(Int,Char)] -> String -> [Bool]
forbiddenHelper (ab) str = [forbiddenPairCompare a str | a <- ab]

{-
Function that helps hardConstraint1
-}
forcedHelper :: [(Int,Char)] -> String -> [Bool]
forcedHelper (ab) str = [forcedPairCompare a str | a <- ab]

{-
Function that helps hardConstraint3
-}
tooNearHelper :: [(Char, Char)] -> String -> [Bool]
tooNearHelper (ab) str = [tooNearCompare a str | a <- ab]

{-
Function that helps toonearpen soft constraint
-}
tooNearPenHelper:: [(Char,Char)] -> String -> [Bool]
tooNearPenHelper (ab) str = [tooNearComparePen a str | a <- ab]

{-
Function gives total machine penalties of an assignment
-}
getTotalMachPen :: [[Int]] -> String -> Int
getTotalMachPen machPen assignment = sum [machPen !! (fromJust $ findIndex (==x) assignment) !! (costMachNumber x) | x <- assignment]
--getTotalMachPen machPen assignment = sum [machPen !! (costMachNumber x) !! (fromJust $ findIndex (==x) assignment) | x <- assignment]


--filterAccordingTo :: [Bool] -> [a] -> [a]
--filterAccordingTo bs xs = map snd . filter fst $ zip bs xs

getTotalNearPen :: [(Char,Char,Int)] -> String -> Int
getTotalNearPen nearPen assignment = sum [ tupleGet3rd x | x <- nearPen, checkTooNearPresence x assignment ]

{-
Check if the given assignment contains the given too-near pair
-}
checkTooNearPresence :: (Char, Char, Int) -> String -> Bool
checkTooNearPresence (a, b, c) str =
   if (tooNearCheck (a, b) str 0 == True)
       then False
       else True

{-
getAllNearPen :: [Bool] -> [(Char,Char,Int)] -> [(Char,Char,Int)]
getAllNearPen bs xs = filterAccordingTo bs xs

Function that gives total nearpenalties of assignment
-}
{-getTotalNearPen :: [(Char,Char,Int)] -> String -> Int
--getTotalNearPen nearPen assignment
getTotalNearPen nearPen assignment = sum [tupleGet3rd x | x <- getAllNearPen( tooNearPenHelper ( (tupleSizeAdjust nearPen) assignment)  nearPen ) ]
--getTotalNearPen nearPen assignment =  sum [ tupleGet3rd x | x <- nearPen, and ((tooNearPenHelper( tupleSizeAdjust nearPen) assignment ))]
-}

--------------- Constraints and total cost

hardConstraint1 :: [(Int, Char)] ->[String] -> [String]
hardConstraint1 (xs) (ys) = [ y | y <- ys, and (forcedHelper xs y) ]

hardConstraint2 :: [(Int, Char)] ->[String] -> [String]
hardConstraint2 (xs) (ys) = [ y | y <- ys, and (forbiddenHelper xs y) ]

hardConstraint3 :: [(Char, Char)] -> [String] -> [String]
hardConstraint3 (xs) (ys) = [ y | y <- ys, and (tooNearHelper xs y) ]

getTotalCost :: [[Int]] -> [(Char,Char,Int)] -> String -> Int
getTotalCost machPen tooNearPen assignment = (getTotalMachPen machPen assignment) + (getTotalNearPen tooNearPen assignment)

---------------


--helper for lowest cost stuff
getAllCosts :: [[Int]] -> [(Char,Char,Int)] -> [String] -> [Int]
getAllCosts machPen tooNearPen assignments = [getTotalCost machPen tooNearPen x | x<-assignments]

calcLowestCost :: [[Int]] -> [(Char,Char,Int)] ->[String] -> Int
calcLowestCost machPen tooNearPen assignments =  minimum (getAllCosts machPen tooNearPen assignments)

calcLowestCostAssignment :: [[Int]] -> [(Char,Char,Int)] -> [String] -> String
calcLowestCostAssignment machPen tooNearPen assignments = assignments !! (fromJust $ findIndex (== calcLowestCost machPen tooNearPen assignments) (getAllCosts machPen tooNearPen assignments))

allPossible = permutations "ABCDEFGH"

finalHardConstraintHelper :: [(Int,Char)] -> [(Int,Char)] -> [(Char,Char)] -> [String]
finalHardConstraintHelper a b c = (hardConstraint3 c (hardConstraint2 b (hardConstraint1 a allPossible)))

hardConstraintHelper2 :: [String] -> Bool
hardConstraintHelper2 [] = False
hardConstraintHelper2 _ = True


outputSolution :: [(Int, Char)] -> [(Int, Char)] -> [(Char, Char)] -> [[Int]] -> [(Char,Char,Int)] -> String
outputSolution a b c d e = if (hardConstraintHelper2 (finalHardConstraintHelper a b c) == True)
  then "Solution "  ++ intersperse ' ' (calcLowestCostAssignment d e (finalHardConstraintHelper a b c)) ++ "; Quality: " ++ show (calcLowestCost d e  (finalHardConstraintHelper a b c))
  else "No valid solution possible!"




--solutionsAfterHC = hardConstraint3( hardConstraint2 (hardConstraint3 allPossible))

-- Main: Used for testing purposes alongside ensuring modularity of our functions
--main = do
  --putStrLn "Testing for brute force."
