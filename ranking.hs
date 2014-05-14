import Data.List.Split

data User = User { rank :: String
                 , username :: String
                 , tweets :: String } deriving (Show)

rankFile :: FilePath
rankFile = "./ranking.sdat"

tweets2level :: Float -> Int
tweets2level n
  | n >= 0 = floor $ n ** 0.65
  | otherwise = 0

parseLine :: String -> User
parseLine str = User rank username level
  where [rank, username, tweets] = splitOn ":" str
        level = show $ tweets2level $ read tweets

user2json :: User -> String
user2json (User _ username level) =
  "{\"name\":\"" ++ username ++ "\", \"level\":" ++ level ++ "}"

main = do
  contents <- readFile rankFile
  let users = map (user2json . parseLine) $ lines contents
      json = foldl1 (\acc str -> acc ++ "," ++ str) users
  putStrLn "Content-type: application/json; charset=utf-8\n\n"
  putStrLn $ "[" ++ json ++ "]"
