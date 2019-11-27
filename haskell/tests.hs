import Utils
import Types
import JsonParser
import Controller

import Test.HUnit

testFilterByYear = TestCase( do
    transactions <- getTestTransactions
    assertEqual "filtro por ano - primeiro valor"  43980.15 (valor (head (filterByYear 2018 transactions)))
    assertEqual "filtro por ano - último valor" (-100.00) (valor (last (filterByYear 2018 transactions))))

testFilterByMonth = TestCase( do
    transactions <- getTestTransactions
    assertEqual "filtro por mês - primeiro valor"  57143.13  (valor (head (filterByYearAndMonth 2018 4 transactions)))
    assertEqual "filtro por mês - último valor"  (-100.00) (valor (last (filterByYearAndMonth 2018 4 transactions))))

testReceiptValue = TestCase (do 
    transactions <- getTestTransactions
    assertEqual "Cálculo do valor das receitas - apenas uma receita"  92.45 (receiptValue 2018 0 transactions)
    assertEqual "Cálculo do valor das receitas - mais de uma receita"  1679.44 (receiptValue 2018 4 transactions))

testDebtValue = TestCase (do 
    transactions <- getTestTransactions
    assertEqual "Cálculo do valor das despesas - apenas uma receita"   (-2260.0) (debtValue 2018 0 transactions)
    assertEqual "Cálculo do valor das despesas - mais de uma receita"  (-350) (debtValue 2018 4 transactions))

testLeftover =  TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo da sobra - apenas uma receita e uma despesa"   (-2167.55) (leftover 2018 0 transactions)
    assertEqual "Cálculo da sobra - mais de uma receita e uma despesa"  (1329.44) (leftover 2018 4 transactions))

testBalance = TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo do saldo final  - mês vazio" 0.0 (balance 2018 3 transactions)
    assertEqual "Cálculo do saldo final" 41812.6 (balance 2018 0 transactions))

testMaxBalance = TestCase  (do
    transactions <- getTestTransactions
    assertEqual "Cálculo saldo maximo - mês vazio" 0.0 (maxBalance 2018 3 transactions)
    assertEqual "Cálculo saldo maximo - máximo saldo = inicial" 43980.15 (maxBalance 2018 0 transactions)
    assertEqual "Cálculo saldo máximo - máximo saldo = valor corrente"  58822.57 (maxBalance 2018 4 transactions))
    
testMinBalance = TestCase  (do
    transactions <- getTestTransactions
    assertEqual "Cálculo saldo mínimo - mês vazio" 0.0 (minBalance 2018 3 transactions)
    assertEqual "Cálculo saldo mínimo - apenas uma despesa" (41720.15) (minBalance 2018 0 transactions)
    assertEqual "Cálculo saldo mínimo - mais de uma despesa" (57143.13) (minBalance 2018 4 transactions))

testReceiptMeanByYear = TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo média das receitas - ano vazio" 0.0 (receiptMeanByYear 2010 transactions)
    assertEqual "Cálculo média das receitas" 590.63 (receiptMeanByYear 2018 transactions)
    assertEqual "Cálculo média das receitas"  2443.4(receiptMeanByYear 2019 transactions))

testDebtMeanByYear = TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo média das despesas -  ano vazio" 0.0 (debtMeanByYear 2010 transactions)
    assertEqual "Cálculo médi das despesas" (-870.0) (debtMeanByYear 2018 transactions))

testLeftoverMeanByYear = TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo da média das sobras mensais em um ano - ano vazio" 0.0 (leftoverMeanByYear 2010 transactions)
    assertEqual "Cálculo da média das sobras mensais em um ano" (-419.05500000000006) (leftoverMeanByYear 2018 transactions))

testCashFlow = TestCase (do
    transactions <- getTestTransactions
    assertEqual "Cálculo do fluxo de caixa - ano vazio" [] (cashFlow 2010 2 transactions)
    assertEqual "Cálculo do fluxo de caixa" [(1, 43980.15), (2, 41812.6)] (cashFlow 2018 0 transactions)
    assertEqual "Cálculo de caixa com mais creditos/debitos" [(1, 131301.66), (2, 130801.66), (3, 135209.42)] (cashFlow 2019 0 transactions))

utilTests = TestList [
    testFilterByYear, 
    testFilterByMonth, 
    testReceiptValue, 
    testDebtValue, 
    testLeftover, 
    testBalance, 
    testMaxBalance, 
    testMinBalance, 
    testReceiptMeanByYear, 
    testDebtMeanByYear, 
    testLeftoverMeanByYear, 
    testCashFlow]

main = do
    runTestTT utilTests