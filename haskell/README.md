## Proj Funcional Haskell

Repositório referente ao projeto final da disciplina de **Programação Funcional** 2019.2

### Setup

O projeto usa o module _aeson_ para o parser do JSON. Para instalá-lo é necessário rodar o comando:

```
$ cabal update
$ cabal install aeson
```

Para executar o projeto, execute o comando abaixo na raiz do projeto:

```
$ ghci Tipos.hs
```

### Consultas

Lista de Consultas/funções/operações a implementar:

* [x] Filtrar transações por ano.
* [x] Filtrar transações por ano e mês.
* [x] Calcular o valor das receitas (créditos) em um determinado mês e ano.
* [x] Calcular o valor das despesas (débitos) em um determinado mês e ano.
* [x] Calcular a sobra (receitas - despesas) de determinado mês e ano
* [x] Calcular o saldo final em um determinado ano e mês
* [x] Calcular o saldo máximo atingido em determinado ano e mês
* [x] Calcular o saldo mínimo atingido em determinado ano e mês
* [X] Calcular a média das receitas em determinado ano
* [X] Calcular a média das despesas em determinado ano
* [x] Calcular a média das sobras em determinado ano
* [x] Retornar o fluxo de caixa de determinado mês/ano. O fluxo de caixa nada mais é do que uma lista contendo pares (dia,saldoFinalDoDia). 
