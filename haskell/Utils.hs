module Utils (
    quicksort,
    checkDay,
    checkMonth,
    checkYear,
    filterByYear,
    filterByYearAndMonth,
    filterByYearMonthAndDay
) where

import Tipos
import JsonParser

-- Ordena uma lista de Transacao por dia
quicksort :: [Transacao] -> [Transacao]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where 
        lesser = filter (lessThan p) xs
        greater =  filter (greaterThan p) xs

lessThan :: Transacao -> Transacao -> Bool        
lessThan p t2 = ((dayOfMonth . datas) p) > ((dayOfMonth . datas) t2)

greaterThan:: Transacao -> Transacao -> Bool
greaterThan p t2 = not (lessThan p t2)

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
    transations <- getTransations
    return (filter (checkYear year) transations)
    
-- Filtrar transações por ano e mês.
filterByYearAndMonth :: Int -> Int  -> IO [Transacao]
filterByYearAndMonth year month = do
    filteredByYear <- filterByYear year
    return (filter (checkMonth month) filteredByYear)
    
    -- Filtrar transações por ano, mês e dia.
filterByYearMonthAndDay :: Int -> Int -> Int -> IO [Transacao]
filterByYearMonthAndDay year month day = do
    filteredByMonth <- (filterByYearAndMonth year month)
    return (filter (checkDay day) filteredByMonth)