module JsonParser
(
    getJSON,
    getTransations
) where

import qualified Data.ByteString.Lazy as B  
import Data.Aeson
import Tipos

jsonFile :: FilePath
jsonFile = "data/transacao.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

getTransations :: IO (Either String [Transacao])
getTransations = (eitherDecode <$> getJSON) :: IO (Either String [Transacao])

