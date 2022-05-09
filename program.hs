import Lib
import System.IO
import Data.Typeable

main :: IO ()
main = do
  file <- openFile "input.txt" ReadMode
  input <- hGetContents file
  let line = lines input
  print "Input data:"
  print line --line of whole input file
  let firstline = line !! 3 -- take second part
  print firstline 
  let twoNumbers =  map read $ words firstline :: [Int] -- string -> int 
  let first =  twoNumbers !! 0 -- assign first number
  let second = twoNumbers !! 1 -- assign second number
  print "N:" 
  print first
  print "C:"
  print second
  let x = [1..first]
  let primelist = filterPrime x
  let lengthoflist = length primelist
  let middle = middleIndex lengthoflist
  let middlefirst = middle !! 0 - 1
  let offsetlr = calco second
  let leftof = calcleft middlefirst offsetlr lengthoflist
  let rightof = calcright middlefirst offsetlr lengthoflist
  let range = dispRange primelist leftof rightof
  print range

  hClose file



is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False | otherwise = True


filterPrime :: [Int] -> [Int]
filterPrime xs = 1 : filter (is_prime) xs

middleIndex :: Int -> [Int]
middleIndex index = if index `rem` 2 == 0 then [index `div` 2] else [(index-1) `div` 2,(index-1) `div` 2 + 1]

calco :: Int -> Int
calco x = x * 2

calcleft :: Int -> Int -> Int -> Int
calcleft middle offset length = if length `rem` 2 == 0 then middle-offset `div` 2+1 else middle-offset `div` 2+2

calcright :: Int -> Int -> Int -> Int
calcright middle offset length = if length `rem` 2 == 0 then middle+offset `div` 2+1 else middle+offset-1

dispRange :: [Int] -> Int -> Int -> [Int]
dispRange l i j = take (j-i) (drop i l)




