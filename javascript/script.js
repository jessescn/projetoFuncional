let transactions;

const input = document.getElementById("input")
const sumReducer = (accumulator, current) => accumulator + current.valor
const simpleSumReducer = (accumulator, current) => accumulator + current

function getData(){
  fetch('http://150.165.15.10:8080/todasTransacoes', {method: 'POST'})
    .then(r => r.json())
    .then(json => transactions = json)
}

getData();

function handleInput(f){
  const { value } = input;
  const inputValues = value.split(" ")
                      .filter(item => item !=="").map(item => parseInt(item)); 
  console.log(f(...inputValues));

}

function filterByYear(year){
  return transactions.filter(({data}) => data.year === year)
}

function filterByYearMonth(year, month){
  return transactions.filter(
    ({data}) => data.year === year && data.month === month
  )
}

const notReceiptOrDebt = ["SALDO_CORRENTE", "APLICACAO", "VALOR_APLICACAO"]

function filterValidTransactions(data){
  return data.filter(({tipos}) => (tipos.filter(item => notReceiptOrDebt.includes(item))).length == 0)
}

function filterReceipts(data){
  const valid = filterValidTransactions(data);
  return valid.filter(({valor}) => valor > 0);
}

function filterDebts(data){
  const valid = filterValidTransactions(data);
  return valid.filter(({valor}) => valor < 0);
}

function receiptValue(year, month){
  const dateFiltered = filterByYearMonth(year, month);
  const receipts = filterReceipts(dateFiltered);
  return receipts.reduce(sumReducer, 0);
}

function debtValue(year, month){
  const dateFiltered = filterByYearMonth(year, month);
  const debts = filterDebts(dateFiltered);
  return debts.reduce(sumReducer, 0);
}

function leftover(year, month){
  return receiptValue(year, month) + debtValue(year, month)
}

const years = [2017, 2018, 2019]
const months = [0,1,2,3,4,5,6,7,8,9,10,11]

const validDates = years
  .map(year => months.map( month => {return {year, month}}))
  .reduce((acc, cur) => acc.concat(cur), [])
  .filter(({year, month})=> (year != 2017 || month >= 2) && (year != 2019 || month <= 10))

function balance(year, month = 11){
  const initialBalance = filterByYearMonth(year, month)[0].valor;
  return initialBalance + leftover(year, month)
}

function compareMonthBalances(year, month, f){
  const monthTransactions = filterByYearMonth(year, month)
  const initialBalance = monthTransactions[0];
  const transactions = [initialBalance].concat(filterValidTransactions(monthTransactions));
  const balances = transactions.map((item, index) => (transactions.slice(0, index + 1)).reduce(sumReducer, 0))
  return f(...balances)
}

function maxBalance(year, month){
  return compareMonthBalances(year, month, Math.max)
}

function minBalance(year, month){
  return compareMonthBalances(year, month, Math.min)
}

function receiptMeanByYear(year){
  const dateFiltered = filterByYear(year);
  const receipts = filterReceipts(dateFiltered);
  return receipts.reduce(sumReducer, 0) / receipts.length;
}

function debtMeanByYear(year){
  const dateFiltered = filterByYear(year);
  const debts = filterDebts(dateFiltered);
  return debts.reduce(sumReducer, 0) / debts.length;
}

function leftOverMeanByYear(year){
  const filteredDates = validDates.filter(({year: y}) => y == year);
  const leftOvers = filteredDates.map(({year: y, month: m}) => leftover(y, m))
  const sum = leftOvers.reduce(simpleSumReducer)
  return sum / filteredDates.length;
}

function leftOverOfDay(year, month, day){
  validDate = transactions.filter(
    ({data}) => 
        data.year == year 
        && data.month == month 
        && data.dayOfMonth == day)
  return filterValidTransactions(validDate).reduce(sumReducer, 0)
}

function dayBalance(year, month, day){
  if(day == 1) return filterByYearMonth(year, month)[0].valor + leftOverOfDay(year, month, day);
  return leftOverOfDay(year, month, day) + dayBalance(year, month, day - 1);
}

function cashFlowMonth(year, month){
  const monthTransactions = filterByYearMonth(year, month)
  const days = monthTransactions.map(item => item.data.dayOfMonth) 
  const validDays = days.reduce((unique, item) => unique.includes(item) ? unique : [...unique, item], [])
  return (validDays.map((day) => {return {month, day: day ,balance: dayBalance(year, month, day)}}))
}

function cashFlow(year, month = -1){
  const months = month == -1 ? validDates.filter( ({year: y}) => y == year) : [{year, month}]
  const aux = months.map( ({month: m}) => cashFlowMonth(year, m))
  return aux.reduce((acc, cur) => [...acc, ...cur])
}

