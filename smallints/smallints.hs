module Main where

import System.Environment (getArgs, getProgName)

import BRC.Solver

import ConstraintParser
import SmallIntConstraints

main :: IO ()
main = do args <- getArgs
          if null args then usage else mapM_ parseAndSolve args
          
usage :: IO ()
usage = do pn <- getProgName
           putStrLn $ "Usage: " ++ pn ++ " <file> ..."
           mapM_ putStrLn [ "",
                            "Parses files of inequality contraints on small integers, ",
                            "and either prints a satisfying assignment for the variables, ",
                            "or an error message.", 
                            "",
                            "The integers must be between zero (0) and twenty (20), inclusive." ]

parseAndSolve :: String -> IO ()
parseAndSolve fileName =
  do putStrLn $ "For file: " ++ fileName
     either putStrLn solveAndPrint =<< parseConstraintFile fileName
  where solveAndPrint constraints =
          let constraints' = concatMap toBRCConstraints constraints
              errOrResult = solve lteRel constraints'
              printAssignments as =
                if null as then putStrLn "No variables to bind." else mapM_ printAssignment as
              printAssignment a =
                do putStrLn "Assignment:"
                   mapM_ printBinding a
              printBinding b =
                do putStr (variable b)
                   putStr " -> "
                   print (value b)
          in either print printAssignments errOrResult

