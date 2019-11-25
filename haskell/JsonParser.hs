module JsonParser
(
    getJSON,
    getTransactions
) where

import qualified Data.ByteString.Lazy as B  
import Data.Aeson
import Data.Maybe
import Tipos

jsonFile :: FilePath
jsonFile = "data/transacoes.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getTransactions :: IO [Transacao]
getTransactions = do
    transactions <- (decode <$> getJSON) :: IO (Maybe [Transacao])
    return (fromJust transactions)
