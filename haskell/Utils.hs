module Utils (
    intersect,
    checkDay,
    checkMonth,
    checkYear,
    filterByYear,
    filterByYearAndMonth,
    filterByYearMonthAndDay,
    initialBalance,
    firstTransaction
) where

import Tipos
import JsonParser

intersect []  _ = []
intersect (x:xs) ys
    | x `elem` ys = [x]++(intersect xs ys)
    | otherwise = (intersect xs ys)

-- Checando o ano 
checkYear :: Int -> Transacao -> Bool
checkYear y t = y == ((year . datas) t)

-- Checando o mês
checkMonth :: Int -> Transacao -> Bool
checkMonth m  t = m == ((month . datas) t)

-- Checando o dia
checkDay :: Int -> Transacao -> Bool
checkDay d t = d == ((dayOfMonth . datas) t)

-- Filtrar transações por ano.
filterByYear :: Int -> IO [Transacao]
filterByYear year = do
    transactions <- getTransactions
    return (((filter . checkYear) year) transactions)
    
-- Filtrar transações por ano e mês.
filterByYearAndMonth :: Int -> Int  -> IO [Transacao]
filterByYearAndMonth year month = do
    filteredByYear <- filterByYear year
    return (((filter . checkMonth) month) filteredByYear)
    
    -- Filtrar transações por ano, mês e dia.
filterByYearMonthAndDay :: Int -> Int -> Int -> IO [Transacao]
filterByYearMonthAndDay year month day = do
    filteredByMonth <- (filterByYearAndMonth year month)
    return (((filter . checkDay) day) filteredByMonth)

firstTransaction :: Int -> Int -> IO [Transacao]
firstTransaction y m = do
    transactions <- (filterByYearAndMonth y m)
    return (_firstTransaction transactions)

_firstTransaction :: [Transacao] -> [Transacao]
_firstTransaction [] = []
_firstTransaction xs = [(xs !! 0)]

initialBalance :: Int -> Int -> IO Double
initialBalance y m = do
    transaction <- (firstTransaction y m)
    return (_initialBalance transaction)

_initialBalance :: [Transacao] -> Double
_initialBalance [] = 0
_initialBalance xs = valor (xs !! 0)