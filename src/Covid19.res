
let monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

let byStateName = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let result = if (states[indexA].name > states[indexB].name) {
    1
  } else if (states[indexA].name < states[indexB].name) {
    -1
  } else {
    0
  }
  (Page.pageState.dir == Ascending) ? result : -result;
}

let byTotal = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let totalA = (Page.pageState.period == All) ? states[indexA].totalCases : states[indexA].pastWeekCases;
  let totalB = (Page.pageState.period == All) ? states[indexB].totalCases : states[indexB].pastWeekCases;
  let valueA = totalA /. ((Page.pageState.col == Total) ? 1.0 : states[indexA].population *. 100000.0);
  let valueB = totalB /. ((Page.pageState.col == Total) ? 1.0 : states[indexB].population *. 100000.0);
  let result = if (valueA > valueB) {
    1
  } else if (valueA < valueB) {
    0
  } else {
    1
  }
  (Page.pageState.dir == Ascending) ? result : -result;
}


Js.log("About to fetch data");
let getPopulation = (cases: string) => {
  open Js.Promise;
  Fetch.fetch("covid_county_population_usafacts.csv")
  |> then_(Fetch.Response.text)
  |> then_(population => Data.analyzeData(cases, population) |> resolve);
}

let fetchData = () => {
  // fetch('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')
  open Js.Promise;
    Fetch.fetch("covid_confirmed_usafacts.csv")
    |> then_(Fetch.Response.text)
    |> then_(cases => getPopulation(cases) |> resolve);
}

/*

let fetchData = () => {
  // fetch('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')
  open Js.Promise;
    Fetch.fetch("covid_confirmed_usafacts.csv")
    |> then_(Fetch.Response.text)
    |> then_(cases => getPopulation(cases) |> resolve);
}
*/



