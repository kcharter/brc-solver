module Main where

import System.Environment (getArgs, getProgName)

import Data.List (intercalate)

import BRC.Constraint
import BRC.SetOf (elements)
import BRC.Solver
import BRC.Solver.Options

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
              errOrResult = solve options lteRel constraints'
              options = defaultOptions {
                formatVariable = id,
                formatValue = show,
                formatSet = \s -> "{" ++ intercalate "," (map show (elements s)) ++ "}", 
                formatConstraint =
                  (\(Related a b) -> formatArg a ++ " < " ++ formatArg b)
                }
              formatArg (Variable v) = v
              formatArg (Constant n) = show n
              printAssignments as =
                if null as then putStrLn "No variables to bind." else mapM_ printAssignment as
              printAssignment = putStrLn . formatAssignment
              formatAssignment bs = "{ " ++ intercalate ", " (map formatBinding bs) ++ " }"
              formatBinding (v,x) = v ++ "=" ++ show x
          in either print printAssignments errOrResult
