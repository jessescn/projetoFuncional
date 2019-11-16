import JsonParser
import Tipos

checkYear :: Int -> Transacao -> Bool
checkYear y t = y == ((year . datas) t)

checkMonth :: Int -> Transacao -> Bool
checkMonth m  t = m == ((month . datas) t)

isIncomeOrExpense :: Transacao -> Bool
isIncomeOrExpense t = not (elem "APLICACAO" (tipos t)  && elem "VALOR_APLICACAO" (tipos t))

isIncome:: Transacao -> Bool
isIncome t = (isIncomeOrExpense t) && (valor t) >= 0

isExpense:: Transacao -> Bool
isExpense t = (isIncomeOrExpense t) && (valor t) < 0

filterByYear :: Int -> IO [Transacao]
filterByYear year = do
    transations <- getTransations
    return (filter (checkYear year) transations)

getExpenses :: Int -> Int -> IO [Transacao]
getExpenses y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByMonthAndYear y m)
    return (drop 1 (filter isExpense transations))

getIncomes :: Int -> Int -> IO [Transacao]
getIncomes y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByMonthAndYear y m)
    return (drop 1 (filter isIncome transations))

filterByMonthAndYear :: Int -> Int  -> IO [Transacao]
filterByMonthAndYear year month = do
    filteredByYear <- filterByYear year
    return (filter (checkMonth month) filteredByYear)

calculateCredit :: Int -> Int -> IO Double
calculateCredit year month = do
    transations <- getExpenses year month
    return ((sum . (map valor)) transations)

calculateDebit :: Int -> Int -> IO Double
calculateDebit year month = do
    transations <- filterByMonthAndYear year month
    return ((sum . (map valor)) transations)