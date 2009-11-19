#! /bin/zsh

exec 3>&1
exec 1>Main.hs

echo module Main where
echo
echo "-- Created by $0 on $(date)"
echo "-- DO NOT MODIFIY"
echo
for i in Euler*hs
  echo import ${i:r}
echo
echo import System
echo
echo main = do
echo "       args <- getArgs"
for i in Euler*hs
{
  echo "       if null args || `printf %d ${${i:r}#Euler}` \`elem\` (map read args)"
  echo "         then putStrLn \$ \"Problem ${i:r}: \" ++ show e${${i:r}#E}"
  echo "         else putStrLn \"Problem ${i:r} is not selected by command line\""
}

exec 1>&3 3>&-
