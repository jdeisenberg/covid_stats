type popInfo = (string, float)
type caseInfo = (string, array<float>)

type csvInfo = {
  states: array<popInfo>,
  dates: array<string>,
  totalCases: array<caseInfo>
}

type rowInfo = {
  state: string,
  totalCases: float,
  perCapitaCases: float
}


let monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

let csv: ref<csvInfo> = ref({
  states: [],
  dates: [],
  totalCases: []});
  

let analyzeData = (cases, population) => {
  open Js.Array2;

  let rec processPopulation = (lines: array<string>,
    acc: array<popInfo>, total: float, state: string, index:int): array<popInfo> => {
    if (index == length(lines)) {
      concat(acc, [(state, total)])
    } else {
      let items = Js.String2.split(lines[index], ",");
      if (length(items) == 4) {
        let countyState = items[2];
        let countyPop = float_of_string(items[3]);

        if (countyState == state) {
          processPopulation(lines, acc, total +. countyPop, state, index + 1);
        } else {
          if (state != "") {
            processPopulation(lines, concat(acc, [(state, total)]), 0.0, countyState, index + 1);
          } else {
            processPopulation(lines, acc, total, countyState, index + 1);
          }
        }
      } else {
        processPopulation(lines, acc, total, state, index + 1);
      }
    }
  }
  
  let adder = (. f: float, s: string) => {
    f +. float_of_string(s);
  }
  
  let addCases = (total: array<float>, items: array<string>): array<float> => {
    Belt.Array.zipByU(total, items, adder);
  }

  let rec processCases = (lines: array<string>,
    acc: array<caseInfo>, total: array<float>, state: string, index:int): array<caseInfo> => {
    if (index == length(lines)) {
      concat(acc, [(state, total)])
    } else {
      let items = Js.String2.split(lines[index], ",");
      if (length(items) >= length(total)) {
        let countyState = items[2];
        if (countyState == state) {
          processCases(lines, acc, addCases(total, sliceFrom(items, 4)), state, index + 1);
        } else {
          if (state != "") {
            processCases(lines, concat(acc, [(state, total)]),
              Belt.Array.make(length(total), 0.0), countyState, index + 1);
          } else {
            processCases(lines, acc, total, countyState, index + 1);
          }
        }
      } else {
        processCases(lines, acc, total, state, index + 1);
      }
    }
  }

  /* Get the total population for states */
  let popLines = Js.String2.split(population, "\n");

  
  let statePopulations = processPopulation(popLines, [], 0.0, "", 1);
  
  /* Get case data for states */
  let caseLines = Js.String2.split(cases,"\n");
  let caseHeader = caseLines[0];
  let dates = Js.String2.split(caseHeader, ",") -> 
    sliceFrom(4);
  let emptyTotals = Belt.Array.make(length(dates), 0.0);
  let stateCases = processCases(caseLines, [], emptyTotals, "", 1)
  
  csv := {
    states: statePopulations,
    dates: dates,
    totalCases: stateCases
  }
}

let getPopulation = (cases: string) => {
  open Js.Promise;
  Fetch.fetch("covid_county_population_usafacts.csv")
  |> then_(Fetch.Response.text)
  |> then_(population => analyzeData(cases, population) |> resolve);
}

let fetchData = () => {
  // fetch('https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv')
  open Js.Promise;
    Fetch.fetch("covid_confirmed_usafacts.csv")
    |> then_(Fetch.Response.text)
    |> then_(cases => getPopulation(cases) |> resolve);
}

Js.log("About to fetch data");
fetchData();
