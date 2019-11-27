import Utils
import Types
import JsonParser
import Controller

import Test.HUnit

testFilterByYear = TestCase( do
    transactions <- getTestTransactions
    assertEqual "filtro por ano - primeiro valor"  43980.15 (valor (head (filterByYear 2018 transactions)))
    assertEqual "filtro por ano - último valor" 929.44 (valor (last (filterByYear 2018 transactions))))

testFilterByMonth = TestCase( do
    transactions <- getTestTransactions
    assertEqual "filtro por mês - primeiro valor"  57143.13  (valor (head (filterByYearAndMonth 2018 4 transactions)))
    assertEqual "filtro por mês - último valor"  929.44 (valor (last (filterByYearAndMonth 2018 4 transactions))))

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

utilTests = TestList [testFilterByYear, testFilterByMonth, testReceiptValue, testDebtValue, testLeftover, testBalance]

main = do
    runTestTT utilTests