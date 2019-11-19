{-# LANGUAGE DeriveGeneric #-}
module Tipos 
(
    Transacao (..),
    GregorianCalendar (..)
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B  

-- data TipoTransacao = SALDO_CORRENTE | VALOR_APLICACAO | RECEITA_OPERACIONAL | TAXA_CONDOMINIO | TAXA_EXTRA | TAXA_SALAO_FESTA | MULTAS_JUROS | TAXA_AGUA | RECUPERACAO_ATIVOS | MULTA_JURO_CORRECAO_COBRANCA | OUTRAS_RECEITAS | DESPESAS_PESSOAL | TERCEIRAZAO_FUNCIONARIOS | VIGILANCIA | SALARIO_FUNCIONARIOS_ORGANICOS | ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS | FERIAS | INSS | FGTS | PIS | ISS | BENEFICIO_SOCIAL | OUTRAS_DESPESAS_PESSOAL | DESPESAS_ADMINISTRATIVAS | ENERGISA | CAGEPA |  COMPRA | ADMINISTRACAO_CONDOMINIO | MANUTENCAO |  ABASTECIMENTO | SERVICOS_TERCEIROS | IRPF | TARIFAS_BANCARIAS | OUTRAS_DESPESAS_ADMINISTRATIVAS | APLICACAO |  OUTROS deriving (Show, Enum, Eq, Generic) 

data GregorianCalendar = 
    GregorianCalendar { 
        year :: Int,
        month :: Int,
        dayOfMonth :: Int
    } deriving (Show, Generic)

data Transacao = 
    Transacao {
        -- data :: GregorianCalendar, "data" é uma keyword do haskell
        datas :: GregorianCalendar,
        textoIdentificador :: String,
        valor :: Double,
        descricao :: String,
        numeroDOC :: String,
        tipos :: [String]
    } deriving (Show, Generic)

instance FromJSON GregorianCalendar
instance ToJSON GregorianCalendar

instance FromJSON Transacao
instance ToJSON Transacao

-- instance FromJSON TipoTransacao
-- instance ToJSON TipoTransacao

instance Eq Transacao where
    Transacao _ id1 _ _ _ _ == Transacao _ id2 _ _ _ _  = id1 == id2

instance Ord Transacao where
    compare (Transacao _ _ v1 _ _ _)  (Transacao _ _ v2 _ _ _) = compare v1 v2
    (Transacao _ _ v1 _ _ _) > (Transacao _ _ v2 _ _ _ ) = v1 > v2
    (Transacao _ _ v1 _ _ _) < (Transacao _ _ v2 _ _ _) = v1 < v2
    (Transacao _ _ v1 _ _ _) <= (Transacao _ _ v2 _ _ _) = v1 <= v2