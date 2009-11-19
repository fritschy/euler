#! /bin/zsh

exec 3>&1
exec 1>Main.hs

echo module Main where
echo
echo "-- Created by $0 on $(date)"
echo "-- DO NOT MODIFIY"
echo
for i in Euler00*hs
  echo import ${i:r}
echo
echo main = do
for i in Euler00*hs
  echo "       putStrLn \$ \"Problem ${i:r}: \" ++ show e${${i:r}#E}"

exec 1>&3 3>&-
