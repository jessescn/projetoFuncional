import Utils
import Types
import JsonParser
import Functions

import Test.HUnit

-- Testando a função 'intersect' 
test1 = TestCase(assertEqual "interseção vazia"   [] (intersect [2, 1] [3]))
test2 = TestCase(assertEqual "interseção não vazia" [3,4] (intersect [1,2,3,4] [3,4,5]))

-- t1 =( Transacao (GregorianCalendar 2017 2 5) "tran-1" -350 "no-description" "000505" [RECEITA_OPERACIONAL])
-- t3 = (Transacao (GregorianCalendar 2017 2 5) "tran-2" 50 "no-description" "000506" [TAXA_CONDOMINIO])
-- t2 = (Transacao (GregorianCalendar 2018 4 5) "tran-3"  200 "no-description" "000507" [RECEITA_OPERACIONAL])

-- Testando a função 'sameDay'
test3  = TestCase (do 
    transactions <- getTransactions
    assertEqual "dias iguais" True (sameDay (transactions !! 0) (transactions !! 1)))

utilTests = TestList [test1, test2, test3]
