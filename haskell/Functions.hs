module Functions (
    sumReceipts,
    sumDebts,
    _leftover,
    _balance,
    _maxBalance,
    _minBalance,
    _receiptMeanByYear,
    _debtMeanByYear,
    _leftoverMeanByYear,
    _cashFlow
) where


import Types
import Data.List (groupBy)
import Utils

-- OPERACOES  --

-- Verificando se a 'Transacao' é uma receita ou despesa
-- não pode ser do tipo 'APLICACAO' nem 'VALOR_APLICACAO'
isReceiptOrDebt :: Transacao -> Bool
isReceiptOrDebt t = (intersect (tipos t) [APLICACAO, SALDO_CORRENTE, VALOR_APLICACAO]) == []

getReceiptsAndDebts:: [Transacao] -> [Transacao]
getReceiptsAndDebts transactions = (filter isReceiptOrDebt transactions)

-- Verificando se a 'Transacao' é uma receita
isReceipt:: Transacao -> Bool
isReceipt t = (valor t) > 0

-- Verificando se a 'Transacao' é uma despesa
isDebt:: Transacao -> Bool
isDebt t = (valor t) < 0

-- Retorna a lista de débitos
getDebts :: [Transacao] -> [Transacao]
getDebts  transactions = (filter isDebt (getReceiptsAndDebts transactions))

-- Retorna a lista de créditos
getReceipts :: [Transacao] -> [Transacao]
getReceipts transactions =  (filter isReceipt (getReceiptsAndDebts transactions))

-- Calcular o valor das receitas (créditos) em um determinado mês e ano.
sumReceipts ::  [Transacao] -> Double
sumReceipts transactions = ((sum . (map valor)) (getReceipts transactions))

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.
sumDebts ::  [Transacao] -> Double
sumDebts transactions =  ((sum . (map valor)) (getDebts transactions))

-- Calcular a sobra (receitas - despesas) de determinado mês e ano
_leftover :: [Transacao] -> Double
_leftover transactions =  ((sum . (map valor))  (getReceiptsAndDebts transactions))

-- Calcular o saldo final em um determinado ano e mês
_balance :: [Transacao] -> Double
_balance transactions =  (initialBalance transactions) + (_leftover transactions)

-- Calcular o saldo máximo atingido em determinado ano e mês
_maxBalance :: [Transacao]-> Double
_maxBalance transactions = (_minMaxBalance  (getReceiptsAndDebts transactions) (initialBalance transactions) maximum)

-- Calcular o saldo mínimo atingido em determinado ano e mês
_minBalance :: [Transacao]-> Double
_minBalance transactions = (_minMaxBalance  (getReceiptsAndDebts transactions) (initialBalance transactions) minimum)

_minMaxBalance :: [Transacao] -> Double -> ([Double] -> Double) -> Double
_minMaxBalance [] _ _= 0
_minMaxBalance transactions monthBalance f = ( f (createBalances (reverse (monthBalance:(map valor transactions)))))

-- Cria uma lista com os balanços
createBalances :: [Double] -> [Double]
createBalances [x] = [x]
createBalances (x:xs) = (x + (sum xs):(createBalances xs))

-- -- Calcular a média das receitas em determinado ano
_receiptMeanByYear :: [Transacao] -> Double
_receiptMeanByYear [] = 0.0
_receiptMeanByYear transactions = ((sum (map valor annualReceipts)) / fromIntegral ( length annualReceipts))
    where annualReceipts =  (getReceipts transactions)

-- Calcular a média das despesas em determinado ano
_debtMeanByYear :: [Transacao] -> Double
_debtMeanByYear [] = 0.0
_debtMeanByYear transactions = ((sum (map valor annualDebts)) / fromIntegral (length annualDebts))
    where annualDebts =  (getDebts transactions)

-- Calcular a média das sobras em determinado ano
_leftoverMeanByYear:: [Transacao] -> Double
_leftoverMeanByYear [] = 0.0
_leftoverMeanByYear transactions = (_meanByYear  (getReceiptsAndDebts transactions) _leftover)

_meanByYear :: [Transacao] -> ([Transacao] -> Double) -> Double
_meanByYear transactions f = ((sum (map f listOfMonths)) / (fromIntegral (length listOfMonths)))
    where listOfMonths = (groupBy sameMonth transactions)

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).
_cashFlow :: [Transacao] -> [(Int, Double)]
_cashFlow transactions = reverse (sumDayFlow (reverse (groupBy sameDay validTransactions)))
    where validTransactions = (firstTransaction transactions)++(getReceiptsAndDebts transactions)

sumDayFlow :: [[Transacao]] -> [(Int, Double)]
sumDayFlow  [] = []
sumDayFlow (x:xs) = [(((dayOfMonth . datas) (x !! 0)), (sum (map _sumDayFlow (x:xs))))] ++ (sumDayFlow xs)

_sumDayFlow :: [Transacao] -> Double
_sumDayFlow transactions = (sum (map valor transactions))
