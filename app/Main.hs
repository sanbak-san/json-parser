module Main where

import Text.ParserCombinators.Parsec

thisParser :: Parser value -> Parser value
thisParser pars = do result <- pars
                     spaces
                     return result

parseJSON :: String -> String
parseJSON text = case parse fileContent "" text of
                Right v -> v

fileContent :: Parser String
fileContent = do jsonVal <- objectValue
                 eof
                 return jsonVal

jsonValue :: Parser String
jsonValue = do objectType <- thisParser (nameValue <|> objectValue)
               return objectType

objectValue :: Parser String
objectValue = do tupleXmlValues <- between firstChar lastChar (sepBy tuplesValue sepChar)
                 return (concat tupleXmlValues)
                 where
                 firstChar = thisParser (char '{')
                 lastChar = thisParser (char '}')
                 sepChar = thisParser (char ',')

tuplesValue :: Parser String
tuplesValue = do property <- thisParser nameValue
                 thisParser (char ':')
                 value <- thisParser jsonValue
                 return ("<" ++ property ++ ">" ++ value ++ "</" ++ property ++ ">")

nameValue :: Parser String
nameValue = between (char '"' ) (char '"' ) (many letter)

main :: IO ()
main = do
         jsonFile <- readFile "resources/basic.json"
         putStrLn jsonFile
         putStrLn $ parseJSON jsonFile
