--  计算第n个质数

module Main where

import Data.List (nub)

primes :: [Int] -- 定义一个函数，返回所有质数
primes = sieve [2..]     -- 调用sieve函数，返回所有质数

-- sieve函数，返回所有质数
-- 输入：一个整数列表
-- 输出：一个质数列表

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]  -- 递归调用，返回所有质数

-- nthPrime函数，返回第n个质数
-- 输入：一个整数n
-- 输出：第n个质数

nthPrime :: Int -> Int  -- 定义一个函数，返回第n个质数
nthPrime n = primes!! (n-1) -- 调用primes函数，返回第n个质数

main :: IO ()
main = do
    putStrLn "Enter a number to find the nth prime:"
    n <- readLn
    putStrLn $ "The " ++ show n ++ "th prime is " ++ show (nthPrime n)
