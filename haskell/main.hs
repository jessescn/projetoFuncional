import JsonParser
import Tipos

-- Checando o ano 
checkYear :: Int -> Transacao -> Bool
checkYear y t = y == ((year . datas) t)

-- Checando o mês
checkMonth :: Int -> Transacao -> Bool
checkMonth m  t = m == ((month . datas) t)

checkDay :: Int -> Transacao -> Bool
checkDay d t = d == ((dayOfMonth . datas) t)

-- Verificando se a 'Transacao' é uma receita ou despesa
-- não pode ser do tipo 'APLICACAO' nem 'VALOR_APLICACAO'
isIncomeOrExpense :: Transacao -> Bool
isIncomeOrExpense t = not (elem "APLICACAO" (tipos t)  || elem "VALOR_APLICACAO" (tipos t))

-- Verificando se a 'Transacao' é uma receita
isIncome:: Transacao -> Bool
isIncome t = (isIncomeOrExpense t) && (valor t) >= 0

-- Verificando se a 'Transacao' é uma despesa
isExpense:: Transacao -> Bool
isExpense t = (isIncomeOrExpense t) && (valor t) < 0

-- Filtrar transações por ano.
filterByYear :: Int -> IO [Transacao]
filterByYear year = do
    transations <- getTransations
    return (filter (checkYear year) transations)

getExpenses :: Int -> Int -> IO [Transacao]
getExpenses y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByYearAndMonth y m)
    return (drop 1 (filter isExpense transations))

getIncomes :: Int -> Int -> IO [Transacao]
getIncomes y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByYearAndMonth y m)
    return (drop 1 (filter isIncome transations))

-- Filtrar transações por ano e mês.
filterByYearAndMonth :: Int -> Int  -> IO [Transacao]
filterByYearAndMonth year month = do
    filteredByYear <- filterByYear year
    return (filter (checkMonth month) filteredByYear)

filterByYearMonthAndDay :: Int -> Int -> Int -> IO [Transacao]
filterByYearMonthAndDay year month day = do
    filteredByMonth <- (filterByYearAndMonth year month)
    return (filter (checkDay day) filteredByMonth)

-- Calcular o valor das receitas (créditos) em um determinado mês e ano.
calculateCredit :: Int -> Int -> IO Double
calculateCredit year month = do
    transations <- getIncomes year month
    return ((sum . (map valor)) transations)

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.
calculateDebit :: Int -> Int -> IO Double
calculateDebit year month = do
    transations <- getExpenses year month
    return ((sum . (map valor)) transations)

-- Calcular a sobra (receitas - despesas) de determinado mês e ano
calculateRemainder :: Int -> Int -> IO Double
calculateRemainder year month = do
    transations <- filterByYearAndMonth year month
    credit <- (calculateCredit year month)
    debit <- (calculateDebit year month)
    return (credit - debit)

-- Calcular o saldo final em um determinado ano e mês
calculateMonthBalance :: Int -> Int -> IO Double
calculateMonthBalance year month = do
    transations <- (filterByYearAndMonth year month)
    remainer <- (calculateRemainder year month)
    return (_calculateMonthBalance transations remainer)


_calculateMonthBalance :: [Transacao] -> Double -> Double
_calculateMonthBalance [] _ = 0
_calculateMonthBalance transations remainer = (valor (transations !! 0)) + remainer

createBalances :: [Transacao] -> [Double]
createBalances [x] = []
createBalances (x:y:xs) = [(valor x) + (valor y)] ++ (createBalances (y:xs))

-- Calcular o saldo máximo atingido em determinado ano e mês
getMaxBalance :: Int -> Int -> IO Double
getMaxBalance y m= do
    transations <- filterByYearAndMonth y m
    return (_getMinMaxBalance transations maximum)

-- Calcular o saldo mínimo atingido em determinado ano e mês
getMinBalance :: Int -> Int -> IO Double
getMinBalance y m= do
    transations <- filterByYearAndMonth y m
    return (_getMinMaxBalance transations minimum)

_getMinMaxBalance :: [Transacao] -> ([Double] -> Double) -> Double
_getMinMaxBalance [] _ = 0
_getMinMaxBalance transations f = ((valor (transations !! 0)) + ( f (createBalances transations)))

-- -- Calcular a média das receitas em determinado ano
getAnnualCreditMean :: Int -> IO Double
getAnnualCreditMean year = (_getAnnualMean year calculateCredit)

_getAnnualMean year f = do
    total <-  sequence (map (f year) [1..12])
    return ((sum total) / 12)

-- Calcular a média das despesas em determinado ano
getAnnualDebitMean :: Int -> IO Double
getAnnualDebitMean year = (_getAnnualMean year calculateDebit)

-- Calcular a média das sobras em determinado ano
getAnnualBalanceMean:: Int -> IO Double
getAnnualBalanceMean year = (_getAnnualMean year calculateMonthBalance)

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia). 
-- getCashFlow :: Int -> Int -> IO [(Int, Double)]
-- getCashFlow year month = do
--     transations <- (filterByYearAndMonth year month)
--     return (map (_getCashFlow transations) [1..31])

-- _getCashFlow :: [Transacao] -> Int -> (Int, Double)
-- _getCashFlow transations day = (day, ( sum (map valor (filter (checkDay day) transations))))