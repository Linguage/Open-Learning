-- 计算平方数列的和

sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares (x:xs) = x^2 + sumSquares xs

main :: IO ()
main = do
    let xs = [1..10]
    let sum = sumSquares xs
    putStrLn $ "The sum of squares of " ++ show xs ++ " is " ++ show sum
