{-# LANGUAGE OverloadedStrings #-}
import Database.SQLite.Simple

data CrudField = TestField Int String deriving (Show)

instance FromRow CrudField where
  fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "crud.db"
  execute conn "INSERT INTO crud (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from crud" :: IO [CrudField]
  mapM_ print r
  close conn