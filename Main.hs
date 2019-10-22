module Main where

main :: IO ()
main = do putStrLn "The awesome word counting machine"
          [filePath] <- getArgs
          file <- readFile filePath
          putStrLn file
