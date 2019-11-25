import Tipos
import Data.List (groupBy)
import Utils

-- OPERACOES  --

-- Verificando se a 'Transacao' é uma receita ou despesa
-- não pode ser do tipo 'APLICACAO' nem 'VALOR_APLICACAO'
isReceiptOrDebt :: Transacao -> Bool
isReceiptOrDebt t = (intersect (tipos t) [APLICACAO, SALDO_CORRENTE, VALOR_APLICACAO]) == []

getReceiptsAndDebts:: Int -> Int -> IO [Transacao]
getReceiptsAndDebts y m = do
    transactions <- (filterByYearAndMonth y m)
    return (filter isReceiptOrDebt transactions)

-- Verificando se a 'Transacao' é uma receita
isReceipt:: Transacao -> Bool
isReceipt t = (valor t) > 0

-- Verificando se a 'Transacao' é uma despesa
isDebt:: Transacao -> Bool
isDebt t = (valor t) < 0

-- Retorna a lista de débitos
getDebts :: Int -> Int -> IO [Transacao]
getDebts y m =  do
    transactions <- (getReceiptsAndDebts y m)
    return (filter isDebt transactions)

-- Retorna a lista de créditos
getReceipts :: Int -> Int -> IO [Transacao]
getReceipts y m =  do
    transactions <- (getReceiptsAndDebts y m)
    return (filter isReceipt transactions)

-- Calcular o valor das receitas (créditos) em um determinado mês e ano.
receiptValue :: Int -> Int -> IO Double
receiptValue y m = do
    transactions <- getReceipts y m
    return ((sum . (map valor)) transactions)

-- Calcular o valor das despesas (débitos) em um determinado mês e ano.
debtValue :: Int -> Int -> IO Double
debtValue y m = do
    transactions <- getDebts y m
    return ((sum . (map valor)) transactions)

-- Calcular a sobra (receitas - despesas) de determinado mês e ano
leftover :: Int -> Int -> IO Double
leftover y m = do
    credit <- (receiptValue y m)
    debit <- (debtValue y m)
    return (credit + debit)

-- Calcular o saldo final em um determinado ano e mês
balance :: Int -> Int -> IO Double
balance y m = do
    baseBalance <- (initialBalance y m)
    remainer <- (leftover y m)
    return (baseBalance + remainer)

-- Calcular o saldo máximo atingido em determinado ano e mês
maxBalance :: Int -> Int -> IO Double
maxBalance y m= do
    monthBalance <- (initialBalance y m)
    transactions <- (getReceiptsAndDebts y m)
    return (_minMaxBalance transactions monthBalance maximum)

-- Calcular o saldo mínimo atingido em determinado ano e mês
minBalance :: Int -> Int -> IO Double
minBalance y m= do
    monthBalance <- (initialBalance y m)
    transactions <- (getReceiptsAndDebts y m)
    return (_minMaxBalance transactions monthBalance minimum)

_minMaxBalance :: [Transacao] -> Double -> ([Double] -> Double) -> Double
_minMaxBalance [] _ _= 0
_minMaxBalance transactions monthBalance f = ( f (createBalances (reverse (monthBalance:(map valor transactions)))))

-- Cria uma lista com os balanços
createBalances :: [Double] -> [Double]
createBalances [x] = [x]
createBalances (x:xs) = (x + (sum xs):(createBalances xs))

-- -- Calcular a média das receitas em determinado ano
receiptMeanByYear :: Int -> IO Double
receiptMeanByYear y = (_meanByYear y receiptValue)

-- Calcular a média das despesas em determinado ano
debtMeanByYear :: Int -> IO Double
debtMeanByYear y = (_meanByYear y debtValue)

-- Calcular a média das sobras em determinado ano
leftoverMeanByYear:: Int -> IO Double
leftoverMeanByYear y = (_meanByYear y balance)

_meanByYear :: Int -> (Int -> Int -> IO Double) -> IO Double
_meanByYear y f = do
    total <-  sequence (((map . f) y) [0..11])
    return ((sum total) / 12)

-- Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia).
cashFlow :: Int -> Int -> IO [(Int, Double)]
cashFlow year month = do
    transactions <- (getReceiptsAndDebts year month)
    firstMonthTransaction <- (firstTransaction year month)
    return (reverse (_cashFlow (firstMonthTransaction ++ transactions)))

_cashFlow :: [Transacao] -> [(Int, Double)]
_cashFlow transactions = sumDayFlow (reverse (groupBy sameDay transactions))

sumDayFlow :: [[Transacao]] -> [(Int, Double)]
sumDayFlow  [] = []
sumDayFlow (x:xs) = [(((dayOfMonth . datas) (x !! 0)), (sum (map _sumDayFlow (x:xs))))] ++ (sumDayFlow xs)

_sumDayFlow :: [Transacao] -> Double
_sumDayFlow transactions = (sum (map valor transactions))

sameDay :: Transacao -> Transacao -> Bool
sameDay t1 t2 = ((dayOfMonth . datas) t1) == ((dayOfMonth . datas) t2)