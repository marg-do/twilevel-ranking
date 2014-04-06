import Data.Text

splitStr :: Char -> String -> [String]
splitStr c = \str -> Prelude.map unpack (split (==c) (pack str))

putListStrLn :: [String] -> IO ()
putListStrLn [] = return ()
putListStrLn (s:ss) = do putStrLn s
                         putListStrLn ss
             
main = do file <- readFile "./test.sdat"
          putListStrLn (splitStr '\n' file)
          return ()
          
