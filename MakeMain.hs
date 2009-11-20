module Main where

import Directory
import Time

main = do
          files <- getDirectoryContents "."
          clockTime <- getClockTime
          calendarTime <- toCalendarTime $ clockTime
          let modules = map baseName $ filter isEulerModule files
          putStrLn   "module Main where\n"
          putStrLn   "-- DO NOT EDIT THIS FILE"
          putStrLn $ "-- This file was generated by MakeMain on " ++ calendarTimeToString calendarTime ++ "\n"
          -- imports
          putStrLn . unlines $ map ("import " ++) modules
          putStrLn   "import System\n"
          putStrLn   "main = do"
          putStrLn   "          args' <- getArgs"
          putStrLn   "          let args = map read args'"
          -- call problem implementations
          putStrLn . unlines . map (\(x,y) -> "          if null args || " ++ show x ++ " `elem` args\n" ++
                                              "            then putStrLn $ \"Problem " ++ y ++ ": \" ++ show e" ++ (tail y) ++ "\n" ++
                                              "            else putStr \"\"\n") $ zip [1..(length modules)] modules
          where isEulerModule x = "sh" == (((take 2) . reverse) x) && "Euler" == (take 5 x)
                baseName = takeWhile (/= '.')