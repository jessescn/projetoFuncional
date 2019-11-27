module Controller (
    filterByYear,
    filterByYearAndMonth,
    receiptValue,
    debtValue,
    leftover,
    balance,
    maxBalance,
    minBalance,
    receiptMeanByYear,
    debtMeanByYear,
    leftoverMeanByYear,
    cashFlow
 ) where

import Utils
import JsonParser
import Functions
import Types

-- Calcular o valor das receitas (créditos) em um determinado mês e ano.
receiptValue :: Int -> Int -> [Transacao] -> Double
receiptValue y m transactions = (sumReceipts filteredTransactions)
    where filteredTransactions = (filterByYearAndMonth y m transactions)

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.
debtValue :: Int -> Int -> [Transacao] -> Double
debtValue y m transactions = (sumDebts filteredTransactions)
    where filteredTransactions =  (filterByYearAndMonth y m transactions)

-- Calcular a sobra (receitas - despesas) de determinado mês e ano
leftover :: Int -> Int -> [Transacao] -> Double
leftover y m transactions =  (_leftover filteredTransactions)
    where filteredTransactions = (filterByYearAndMonth y m transactions)

-- Calcular o saldo final em um determinado ano e mês
balance :: Int -> Int -> [Transacao] -> Double
balance y m transactions = (_balance filteredTransactions)
    where filteredTransactions = (filterByYearAndMonth y m transactions)

-- Calcular o saldo máximo atingido em determinado ano e mês
maxBalance :: Int -> Int -> [Transacao] ->  Double
maxBalance y m transactions =(_maxBalance filteredTransactions)
    where filteredTransactions =  (filterByYearAndMonth y m transactions)

-- Calcular o saldo mínimo atingido em determinado ano e mês
minBalance :: Int -> Int -> [Transacao] -> Double
minBalance y m transactions = (_minBalance filteredTransactions)
    where filteredTransactions =  (filterByYearAndMonth y m  transactions)

-- -- Calcular a média das receitas em determinado ano
receiptMeanByYear :: Int -> [Transacao] -> Double
receiptMeanByYear y transactions =  (_receiptMeanByYear filteredTransactions)
    where filteredTransactions =  (filterByYear y transactions)

-- -- Calcular a média das despesas em determinado ano
debtMeanByYear :: Int -> [Transacao] -> Double
debtMeanByYear y transactions =  (_debtMeanByYear filteredTransactions)
    where filteredTransactions =  (filterByYear y transactions)

-- -- Calcular a média das sobras em determinado ano
leftoverMeanByYear :: Int -> [Transacao] -> Double
leftoverMeanByYear y transactions = (_leftoverMeanByYear filteredTransactions)
    where filteredTransactions = (filterByYear y transactions) 

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).
cashFlow :: Int -> Int -> [Transacao] -> [(Int, Double)]
cashFlow y m transactions = (_cashFlow filteredTransactions)
    where filteredTransactions =  (filterByYearAndMonth y m transactions)

