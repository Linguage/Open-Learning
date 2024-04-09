-- 实现功能：计算fibonacci数列的第n项

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = do
    putStrLn "Enter a number to calculate the fibonacci number:"
    n <- readLn :: IO Int
    putStrLn $ "The " ++ show n ++ "th fibonacci number is: " ++ show (fib n)

-- 运行结果：
-- Enter a number to calculate the fibonacci number:
-- 10
-- The 10th fibonacci number is: 55
