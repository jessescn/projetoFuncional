module JsonParser
(
    getJSON,
    getTransactions,
    getTestTransactions
) where

import qualified Data.ByteString.Lazy as B  
import Data.Aeson
import Data.Maybe
import Types

jsonFile :: FilePath
jsonFile = "data/transacoes.json"

testFile :: FilePath
testFile = "data/testTransacoes.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getTestJSON :: IO B.ByteString
getTestJSON = B.readFile testFile

getTransactions :: IO [Transacao]
getTransactions = do
    transactions <- (decode <$> getJSON) :: IO (Maybe [Transacao])
    return (fromJust transactions)

getTestTransactions :: IO [Transacao]
getTestTransactions = do
    transactions <- (decode <$>  getTestJSON) :: IO (Maybe [Transacao])
    return (fromJust transactions)