{-
File:		brute.hs
Author:		Isha Afzaal
UCID: 		30016250
Course:		CPSC 449 - Assignment #2
Date:		March 11, 2019
Info:		Brute force implementation of machine-task assignment problem. Will find and return, by process
			of elimination, the best machine-task assignment (assuming it exists).
-}

-- Modules
import Data.List
import System.IO

-- Function: Generate all 40320 possible solutions. Will contain all solutions in a list containing each solution, where
--			 each solution if a list of tuples (i.e. [(A, 1), (B, 2), (C, 3), (D, 4), (E, 5), (F, 6), (G, 7), (H, 8)] is element 0 of the list)
genMasterList :: [ [(Char, Int)..] ..]

-- Function: Hard Constraint 1 - For each solution, check if it contains each forced partial assignment. If it does not, modify and return a copy
-- 			 of the master list that does not contain the solution (i.e. remove it)
hardConstraint1 :: [ [(Char, Int)..] ..] -> [ [(Char, Int)..] ..]

-- Function: Hard Constraint 2 - For each solution, if it contains a pair that is in the forbidden machine assignment, remove the solution
--		     from the master list
hardConstraint2 :: [ [(Char, Int)..] ..] -> [ [(Char, Int)..] ..]

-- Function: Hard Constraint 3 - For each solution, if two pairs in the given too-near task assignment are on adjacent machines, remove the solution
--			 from the master list
hardConstraint3 :: [ [(Char, Char)..] ..] -> [ [(Char, Int)..] ..]

-- Function: Soft Constraint 1 - For each solution, calculate the overall machine penalty for each machine-task pair
softConstraint1 :: [ [Int..] ..] -> [ ( [(Char, Int)..], Int) ..]

-- Function: Soft Constraing 2 - For each solution, calculate the too-near penalty per adjacent pairs
softConstraint2 :: [ (Char, Char, Int)..] -> [ ( [(Char, Int)..], Int) ..]

-- Function: Driver will be called by the application's main method and provides modularity. Will determine and return the best machine task assignment
driver :: [ [(Char, Int)..] ..] -> [ [(Char, Int)..] ..] -> [ [(Char, Char)..] ..] -> [ [Int..] ..] -> [ (Char, Char, Int)..] -> [(Char, Int)..]

   -- Generate all possible solutions

   -- Check hard constraint 1

   -- Check hard constraint 2

   -- Check hard constraint 3

   -- Check soft constraint 1

   -- Check soft constraint 2

   -- Return the solution

-- Main: Used for testing purposes alongside ensuring modularity of our functions
main = do 
   putStrLn "Testing for brute force."
