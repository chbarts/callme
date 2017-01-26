import Data.Char
import System.IO
import System.Exit
import System.Environment

ltr :: [[Char]]
ltr = [['0','0','0'], ['1','1','1'],
       ['a','b','c'], ['d','e','f'],
       ['g','h','i'], ['j','k','l'],
       ['m','n','o'], ['p','r','s'],
       ['t','u','v'], ['w','x','y']]

mki :: [Char] -> [[Char]] -> [[Char]]
mki []      acc = acc
mki (s:str) acc = if (isDigit s) then mki str (acc ++ [ltr !! (digitToInt s)]) else mki str acc

mkarrs :: [Char] -> [[Char]]
mkarrs str = mki str []

maxval :: Int -> Int
maxval len = 3^len

tti :: Int -> [Int] -> [Int]
tti 0 acc = acc
tti n acc = tti (div n 3) ([mod n 3] ++ acc)

toternary :: Int -> [Int]
toternary n = if n == 0 then [0] else tti n []

padarr :: Int -> [Int] -> [Int]
padarr len arr = (take (len - (length arr)) (repeat 0)) ++ arr

-- let inds is as = inner is as [] where { inner [] _ acc = acc; inner _ [] acc = acc; inner (i:is) (a:as) acc = inner is as (acc ++ [a !! i]) }

indinn :: [Int] -> [[Char]] -> [Char] -> [Char]
indinn [] _          acc = acc
indinn _ []          acc = acc
indinn (i:is) (a:as) acc = indinn is as (acc ++ [a !! i])

inds :: [Int] -> [[Char]] -> [Char]
inds is as = indinn is as []

innprint :: [[Char]] -> Int -> IO ()
innprint _     (-1) = return ()
innprint str   num  = do
  putStrLn (inds (padarr (length str) (toternary num)) str)
  innprint str (num - 1)

printall :: [Char] -> IO ()
printall num = innprint (mkarrs num) ((maxval (length num)) - 1)

help :: IO ()
help = do
  putStrLn "callme numbers..."
  putStrLn "Print all possible words which can be formed from the phone numbers on the command line"
  putStrLn "From a program by Kamran Husain, MPS Inc. Sugarland, Texas <khx@se44.wg2.waii.com>"
  putStrLn "TELECOM Digest Volume 12, Issue 520"

version :: IO ()
version = putStrLn "callme version 1.0"

parse :: [String] -> IO ()
parse ["-h"]        = help
parse ["--help"]    = help
parse ["-v"]        = version
parse ["--version"] = version
parse []            = help >> (exitWith ExitSuccess)
parse nums          = mapM_ printall nums

main :: IO ()
main = getArgs >>= parse >> (exitWith ExitSuccess)
