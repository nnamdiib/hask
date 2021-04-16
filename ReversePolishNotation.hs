import System.Environment
import System.IO

main = do
  (expr:_) <- getArgs
  putStrLn $ show (solveRPN expr)

solveRPN :: String -> Int
solveRPN = head . foldl processItem [] . words

processItem :: [Int] -> String -> [Int]
processItem (x:y:rest) "+" = (x+y):rest
processItem (x:y:rest) "-" = (x-y):rest
processItem (x:y:rest) "*" = (x*y):rest
processItem (x:y:rest) "/" = (x/y):rest
processItem xs v = (read v :: Int):xs
