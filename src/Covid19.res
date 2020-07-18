type stateInfo = {
  name: string,
  population: float,
  cumulativeCasesPerDay: array<float>,
  totalCases: float,
  pastWeekCases: float
}

type csvInfo = {
  mutable dates: array<string>,
  mutable states: array<stateInfo>
};

let csv: csvInfo = {
  dates: [],
  states: []
};

let monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

type popInfo = (string, float);

let analyzeData = (cases: string, population: string) => {
  open Js.Array2;

  /* Get an array of state names and populations from population.csv */
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
  
  /* Add cumulative daily cases for county to total for a state */
  let addCases = (total: array<float>, items: array<string>): array<float> => {
    Belt.Array.zipByU(total, items,
      (. f: float, s: string) => {
        f +. float_of_string(s);
      });
  }

  let rec processCases = (lines: array<string>,
    acc: array<array<float>>, total: array<float>, state: string, index:int): array<array<float>> => {
    if (index == length(lines)) {
      concat(acc, [total])
    } else {
      let items = Js.String2.split(lines[index], ",");
      if (length(items) >= length(total)) {
        let countyState = items[2];
        if (countyState == state) {
          processCases(lines, acc, addCases(total, sliceFrom(items, 4)), state, index + 1);
        } else {
          if (state != "") {
            processCases(lines, concat(acc, [total]),
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
  
  /* Combine the data from the population and cases into an array of stateInfo*/
  let makeStateArray = (statePopulations: array<popInfo>, stateCases: array<array<float>>): array<stateInfo> => {
    let rec helper = (acc, index) => {
      if (index == length(stateCases)) {
        acc;
      } else {
        let len = length(stateCases[index]);
        let total = stateCases[index][len - 1];
        helper(concat(acc, [{
          name: fst(statePopulations[index]),
          population: snd(statePopulations[index]),
          cumulativeCasesPerDay: stateCases[index],
          totalCases: total,
          pastWeekCases: total -. stateCases[index][len - 8]
        }]), index + 1)
      }
    }
    helper([], 0);
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
  
  csv.dates = dates;
  csv.states = makeStateArray(statePopulations, stateCases);

  Js.log(csv.states[0]);
  
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
