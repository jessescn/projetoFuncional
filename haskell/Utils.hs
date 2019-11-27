module Utils (
    intersect,
    sameDay,
    sameMonth,
    checkDay,
    checkMonth,
    checkYear,
    initialBalance,
    firstTransaction,
    filterByYear,
    filterByYearAndMonth
) where

import Types

intersect []  _ = []
intersect (x:xs) ys
    | x `elem` ys = [x]++(intersect xs ys)
    | otherwise = (intersect xs ys)

-- Filtrar transações por ano.
filterByYear :: Int -> [Transacao] -> [Transacao]
filterByYear year transactions = (((filter . checkYear) year) transactions)

-- Filtrar transações por ano e mês.
filterByYearAndMonth :: Int -> Int  -> [Transacao] -> [Transacao]
filterByYearAndMonth y m transactions = (((filter . checkMonth) m) (filterByYear y transactions))

-- Checa se duas transacoes tem o mesmo valor dia
sameDay :: Transacao -> Transacao -> Bool
sameDay t1 t2 = ((dayOfMonth . datas) t1) == ((dayOfMonth . datas) t2)

-- Checa se duas transacoes tem o mesmo valor mes
sameMonth :: Transacao -> Transacao -> Bool
sameMonth t1 t2 = ((month . datas) t1) == ((month . datas) t2)

-- Checando o ano 
checkYear :: Int -> Transacao -> Bool
checkYear y t = y == ((year . datas) t)

-- Checando o mês
checkMonth :: Int -> Transacao -> Bool
checkMonth m  t = m == ((month . datas) t)

-- Checando o dia
checkDay :: Int -> Transacao -> Bool
checkDay d t = d == ((dayOfMonth . datas) t)

-- Recupera o saldo corrente (primeira transacao) de um mês, caso exista
firstTransaction :: [Transacao] -> [Transacao]
firstTransaction [] = []
firstTransaction xs = [(xs !! 0)]

initialBalance :: [Transacao] -> Double
initialBalance transaction = (_initialBalance transaction)

_initialBalance :: [Transacao] -> Double
_initialBalance [] = 0
_initialBalance xs = valor (xs !! 0)