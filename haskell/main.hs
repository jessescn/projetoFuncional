import Tipos
import Data.List (groupBy)
import Utils

-- OPERATIONS  --

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

-- Retorna a lista de débitos
getExpenses :: Int -> Int -> IO [Transacao]
getExpenses y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByYearAndMonth y m)
    return (drop 1 (filter isExpense transations))

-- Retorna a lista de créditos
getIncomes :: Int -> Int -> IO [Transacao]
getIncomes y m =  do
    -- Removendo a transação SALDO_CORRENTE
    transations <- (filterByYearAndMonth y m)
    return (drop 1 (filter isIncome transations))


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
_calculateMonthBalance [] remainer = remainer
_calculateMonthBalance transations remainer = (valor (transations !! 0)) + remainer

-- Cria uma lista com os balanços
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

-- Calcular a média das despesas em determinado ano
getAnnualDebitMean :: Int -> IO Double
getAnnualDebitMean year = (_getAnnualMean year calculateDebit)

-- Calcular a média das sobras em determinado ano
getAnnualBalanceMean:: Int -> IO Double
getAnnualBalanceMean year = (_getAnnualMean year calculateMonthBalance)

_getAnnualMean :: Int -> (Int -> Int -> IO Double) -> IO Double
_getAnnualMean year f = do
    total <-  sequence (((map . f) year) [0..11])
    return ((sum total) / 12)

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).
getCashFlow :: Int -> Int -> IO [(Int, Double)]
getCashFlow year month = do
    expenses <- (getExpenses year month)
    incomes <- (getIncomes year month)
    return (_getCashFlow (quicksort (expenses ++ incomes)))

_getCashFlow :: [Transacao] -> [(Int, Double)]
_getCashFlow transations = map sumDayFlow (groupBy sameDay transations)

sumDayFlow :: [Transacao] -> (Int, Double)
sumDayFlow transations = (((dayOfMonth . datas) (transations !! 0)) , (sum (map valor transations)))

sameDay :: Transacao -> Transacao -> Bool
sameDay t1 t2 = ((dayOfMonth . datas) t1) == ((dayOfMonth . datas) t2)