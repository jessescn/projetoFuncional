import Utils
import JsonParser
import Functions
import Types

-- Filtrar transações por ano.
filterByYear :: Int -> IO [Transacao]
filterByYear year = do
    transactions <- getTransactions
    return (_filterByYear year transactions)

-- Filtrar transações por ano e mês.
filterByYearAndMonth :: Int -> Int  -> IO [Transacao]
filterByYearAndMonth year month = do
    filteredByYear <- (filterByYear year)
    return (_filterByYearAndMonth month filteredByYear)

-- Calcular o valor das receitas (créditos) em um determinado mês e ano.
receiptValue :: Int -> Int -> IO Double
receiptValue y m = do
    transactions <- (filterByYearAndMonth y m)
    return (sumReceipts transactions)

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.
debtValue :: Int -> Int -> IO Double
debtValue y m = do
    transactions <- (filterByYearAndMonth y m)
    return (sumDebts transactions)

-- Calcular a sobra (receitas - despesas) de determinado mês e ano
leftover :: Int -> Int -> IO Double
leftover y m = do
    transactions <- (filterByYearAndMonth y m)
    return (_leftover transactions)

-- Calcular o saldo final em um determinado ano e mês
balance :: Int -> Int -> IO Double
balance y m = do
    transactions <- (filterByYearAndMonth y m)
    return (_balance transactions)

-- Calcular o saldo máximo atingido em determinado ano e mês
maxBalance :: Int -> Int -> IO Double
maxBalance y m= do
    transactions <- (filterByYearAndMonth y m)
    return (_maxBalance transactions)

-- Calcular o saldo mínimo atingido em determinado ano e mês
minBalance :: Int -> Int -> IO Double
minBalance y m= do
    transactions <- (filterByYearAndMonth y m)
    return (_minBalance transactions)

-- -- Calcular a média das receitas em determinado ano
receiptMeanByYear :: Int -> IO Double
receiptMeanByYear y = do 
    transactions <- (filterByYear y)
    return (_receiptMeanByYear transactions)

-- -- Calcular a média das despesas em determinado ano
debtMeanByYear :: Int -> IO Double
debtMeanByYear y = do 
    transactions <- (filterByYear y)
    return (_debtMeanByYear transactions)

-- -- Calcular a média das sobras em determinado ano
leftoverMeanByYear :: Int -> IO Double
leftoverMeanByYear y = do 
    transactions <- (filterByYear y)
    return (_leftoverMeanByYear transactions)

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).
cashFlow :: Int -> Int -> IO [(Int, Double)]
cashFlow year month = do
    transactions <- (filterByYearAndMonth year month)
    return (_cashFlow transactions)

