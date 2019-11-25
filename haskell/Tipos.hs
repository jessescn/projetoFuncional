{-# LANGUAGE DeriveGeneric #-}
module Tipos 
(
    Transacao (..),
    GregorianCalendar (..),
    TipoTransacao (..)
) where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B  

data TipoTransacao = SALDO_CORRENTE | VALOR_APLICACAO | RECEITA_OPERACIONAL | TAXA_CONDOMINIO | TAXA_EXTRA | TAXA_SALAO_FESTA | MULTAS_JUROS | TAXA_AGUA | RECUPERACAO_ATIVOS | MULTA_JURO_CORRECAO_COBRANCA | OUTRAS_RECEITAS | DESPESAS_PESSOAL | TERCEIRIZACAO_FUNCIONARIOS | VIGILANCIA | SALARIO_FUNCIONARIOS_ORGANICOS | ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS | FERIAS | INSS | FGTS | PIS | ISS | BENEFICIO_SOCIAL | OUTRAS_DESPESAS_PESSOAL | DESPESAS_ADMINISTRATIVAS | ENERGISA | CAGEPA |  COMPRA | ADMINISTRACAO_CONDOMINIO | MANUTENCAO |  ABASTECIMENTO | SERVICOS_TERCEIROS | IRPF | TARIFAS_BANCARIAS | OUTRAS_DESPESAS_ADMINISTRATIVAS | APLICACAO |  OUTROS deriving (Enum, Eq, Generic) 

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
        tipos :: [TipoTransacao]
    } deriving (Show, Generic)

instance FromJSON GregorianCalendar
instance ToJSON GregorianCalendar

instance FromJSON Transacao
instance ToJSON Transacao

instance Eq Transacao where
    Transacao _ _ _ _ id1 _ == Transacao _ _ _ _ id2 _  = id1 == id2

instance FromJSON TipoTransacao
instance ToJSON TipoTransacao
 
instance Show TipoTransacao where
    show  SALDO_CORRENTE = "Saldo Corrente" 
    show VALOR_APLICACAO = "Valor aplicado"
    show  RECEITA_OPERACIONAL = "Receita Operacional"
    show TAXA_CONDOMINIO = "Taxas de Condominio"
    show TAXA_EXTRA = "Taxas Extras"
    show TAXA_SALAO_FESTA = "Taxas Salao de Festas"
    show MULTAS_JUROS = "Multas e Juros"
    show TAXA_AGUA = "Taxa de Agua"
    show RECUPERACAO_ATIVOS  = "Recuperacao de Ativos"
    show MULTA_JURO_CORRECAO_COBRANCA = "Multa, Juros e Correcao de Cobranca"
    show OUTRAS_RECEITAS = "Outras receitas"
    show DESPESAS_PESSOAL = "Despesas com pessoal"
    show TERCEIRIZACAO_FUNCIONARIOS = "Terceirizacao de Funcionarios"
    show VIGILANCIA = "Vigilancia"
    show SALARIO_FUNCIONARIOS_ORGANICOS = "Salario dos funcionarios organicos"
    show ADIANTAMENTO_SALARIAL_FUNCIONARIOS_ORGANICOS = "Adiantamento salarial dos funcionarios organicos"
    show FERIAS = "Ferias"
    show INSS = "INSS funcionarios e vigilancia"
    show FGTS = "FGTS" 
    show PIS = "PIS"
    show ISS = "ISS"
    show BENEFICIO_SOCIAL = "Beneficio social dos funcionarios organicos"
    show OUTRAS_DESPESAS_PESSOAL = "Outras despesas com pessoal"
    show DESPESAS_ADMINISTRATIVAS = "Despesas Administrativas"
    show ENERGISA = "Energisa"
    show CAGEPA = "CAGEPA"
    show COMPRA = "Compra"
    show ADMINISTRACAO_CONDOMINIO = "Administracao do condominio"
    show MANUTENCAO = "Manutencao realizada"
    show ABASTECIMENTO = "Abastecimentos"
    show SERVICOS_TERCEIROS = "Servicos realizados por terceiros"
    show IRPF = "Imposto de renda recolhido na fonte"
    show TARIFAS_BANCARIAS = "Tarifas e taxas bancarias"
    show OUTRAS_DESPESAS_ADMINISTRATIVAS = "Outras despesas administrativas"
    show APLICACAO = "Aplicacao"
    show OUTROS = "Outros"  