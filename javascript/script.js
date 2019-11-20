let transactions;
const input = document.getElementById("input")
const firstBalance = 89731.57
const sumReducer = (accumulator, current) => accumulator + current.valor

function getData(){
  fetch('http://150.165.15.10:8080/todasTransacoes', {method: 'POST'})
    .then(r => r.json())
    .then(json => transactions = json);
}

function handleInput(f){
  const { value } = input;
  const inputValues = value.split(" ")
                      .filter(item => item !=="").map(item => parseInt(item)); 
  console.log(f(...inputValues));
}

getData();

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
  return valid.filter( ({valor}) => valor > 0);
}

function filterDebts(data){
  const valid = filterValidTransactions(data);
  return valid.filter( ({valor}) => valor < 0);
}

function receiptValue(year, month){
  const dateFiltered = filterByYearMonth(year, month);
  const receiptsOnly = filterReceipts(dateFiltered);
  return ( receiptsOnly.reduce(
    (accumulator, current) => accumulator + current.valor,
    0));
}

function debtValue(year, month){
  const dateFiltered = filterByYearMonth(year, month);
  const receiptsOnly = filterDebts(dateFiltered);
  return ( receiptsOnly.reduce(
    (accumulator, current) => accumulator + current.valor,
    0));
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
  // const datesBefore = validDates.filter(({year: y, month: m}) => (y == year && m <= month) || (y < year))
  // const firstBalance = 89731.57
  // const alterations = datesBefore
  //   .map(({year: y, month: m}) => leftover(y, m))
  //   .reduce((acc, cur) => acc + cur)
  // return firstBalance + alterations
  if(year == 2017 && month == 1) return 89731.57
  const newYear = month == 0 ? year - 1 : year
  const newMonth = month == 0 ? 11 : month - 1
  return balance(newYear, newMonth) + leftover(year, month)
}

function maxBalance(year, month){
  const datesBefore = validDates.filter(({year: y, month: m}) => (y == year && m <= month) || (y < year))
  const alterations = datesBefore
    .map(({year: y, month: m}) => balance(y, m))
    .reduce((acc, cur) => Math.max(acc, cur))
  return alterations
}

function minBalance(year, month){
  const datesBefore = validDates.filter(({year: y, month: m}) => (y == year && m <= month) || (y < year))
  const alterations = datesBefore
    .map(({year: y, month: m}) => balance(y, m))
    .reduce((acc, cur) => Math.min(acc, cur))
  return alterations
}

function receiptMeanByYear(year){
  const dateFiltered = filterByYear(year);
  const receiptsOnly = filterReceipts(dateFiltered);
  return ( receiptsOnly.reduce(
    (accumulator, current) => accumulator + current.valor,
    0)) / receiptsOnly.length;
}

function debtMeanByYear(year){
  const dateFiltered = filterByYear(year);
  const debtsOnly = filterDebts(dateFiltered);
  return ( debtsOnly.reduce(
    (accumulator, current) => accumulator + current.valor,
    0)) / debtsOnly.length;
}

function leftOverMeanByYear(year){
  const dateFiltered = validDates.filter(({year: y}) => y == year);
  return dateFiltered.map(({year: y, month: m}) => leftover(y, m))
    .reduce((acc, cur) => acc + cur) / dateFiltered.length
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
  if(day == 1) return filterByYearMonth(year, month)[0].valor;
  return leftOverOfDay(year, month, day) + dayBalance(year, month, day - 1);
}

function cashFlowMonth(year, month){
  const monthTransactions = filterByYearMonth(year, month)
  const days = monthTransactions.map(item => item.data.dayOfMonth) 
  const validDays = days.reduce((unique, item) => unique.includes(item) ? unique : [...unique, item], [])
  return (validDays.map((day) => {return {month, day: day ,balance: "R$ " + dayBalance(year, month, day).toFixed(2)}}))
}

function cashFlow(year, month = -1){
  const months = month == -1 ? validDates.filter( ({year: y}) => y == year) : [{year, month}]
  const aux = months.map( ({month: m}) => cashFlowMonth(year, m))
  return aux.reduce((acc, cur) => [...acc, ...cur])
}

