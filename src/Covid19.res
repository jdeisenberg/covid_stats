
let monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];


let getPopulation = (cases: string) => {
  open Js.Promise;
  Fetch.fetch("covid_county_population_usafacts.csv")
  |> then_(Fetch.Response.text)
  |> then_(population => {
    Data.analyzeData(cases, population);
    Page.setEventHandlers();
    Page.setTimePeriods();
    Page.drawTable();
    } |> resolve);
}

let fetchData = () => {
  // fetch('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')
  open Js.Promise;
    Fetch.fetch("covid_confirmed_usafacts.csv")
    |> then_(Fetch.Response.text)
    |> then_(cases => getPopulation(cases) |> resolve);
}

fetchData();




