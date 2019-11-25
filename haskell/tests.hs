import Utils
import Tipos
import JsonParser

import Test.HUnit

test1 = TestCase(assertEqual "intersection empty"   [] (intersect [2, 1] [3]))
test2 = TestCase(assertEqual "intersection not empty" [3,4] (intersect [1,2,3,4] [3,4,5]))

intersectionTests = TestList [test1, test2]

test3 = TestCase(assertEqual "filterByYear"  (IO [(Transacao (GregorianCalendar 2021 1 1) "Saldo Corrente" 10000 "Saldo inicial do mes" "00000000" [ "OUTRAS_RECEITAS", "SALDO_CORRENTE"])]) (filterByYear 2021) )
