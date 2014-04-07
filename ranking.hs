import Data.Text
import Network.CGI


rankStart :: Int
rankStart = 1

rankLimit :: Int
rankLimit = 100000
    
rankFileName :: String
rankFileName = "./ranking.sdat"

splitStr :: Char -> String -> [String]
splitStr c = \str -> Prelude.map unpack (split (==c) (pack str))

splitStrByLf :: String -> [String]
splitStrByLf str = splitStr '\n' str

putListStrLn :: [String] -> IO ()
putListStrLn [] = return ()
putListStrLn (s:ss) = do putStrLn s
                         putListStrLn ss

joinStrList :: [String] -> String
joinStrList [] = ""
joinStrList (s:ss) = s ++ joinStrList ss

getRank :: String -> String
getRank s = (splitStr ':' s) !! 0

getName :: String -> String
getName s = (splitStr ':' s) !! 1

getTweets :: String -> String
getTweets s = (splitStr ':' s) !! 2

toFloat :: String -> Float
toFloat x = (read x :: Float)

getLevel :: String -> String
getLevel s = (show (fromEnum ((toFloat (getTweets s)) ** 0.65)))
              
getRankAndName :: String -> String
getRankAndName s = "\"" ++ getName s ++ "\",\n"

getJHash :: String -> String
getJHash s = "{\"name\":\"" ++ getName s ++ "\",\"level\":" ++ getLevel s ++ "},\n"

twice :: (a -> a) -> a -> a
twice f x = f (f x)

cutLastChar :: String -> String
cutLastChar str = Prelude.init str
            
rankingAll :: IO String
rankingAll = do file <- readFile rankFileName
                return file

getListRange :: (Int, Int) -> [a] -> [a]
getListRange (s, e) xs = Prelude.drop (s-1) (Prelude.take e xs)

header :: String
header = "Content-type: text/javascript; charset=utf-8\n\n"

main :: IO ()
main = do file <- rankingAll
          putStrLn header
          putStrLn "["
          putStrLn (twice cutLastChar (joinStrList (Prelude.map getJHash (getListRange (rankStart, rankLimit) (splitStrByLf file)))))
          putStrLn "]"
          return ()
