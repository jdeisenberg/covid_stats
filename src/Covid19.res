type column =
  | State
  | Total
  | Per100K;
  
type sortDirection =
  | Ascending
  | Descending;

type timePeriod =
  | All
  | PastWeek;

type pageStateType = {
  mutable col: column,
  mutable dir: sortDirection,
  mutable period: timePeriod,
  mutable indices: array<int>
}

let pageState: pageStateType = {
  col: State,
  dir: Ascending,
  period: PastWeek,
  indices: []
};

let byStateName = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let result = if (states[indexA].name > states[indexB].name) {
    1
  } else if (states[indexA].name < states[indexB].name) {
    -1
  } else {
    0
  }
  (pageState.dir == Ascending) ? result : -result;
}

let byTotal = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let totalA = (pageState.period == All) ? states[indexA].totalCases : states[indexA].pastWeekCases;
  let totalB = (pageState.period == All) ? states[indexB].totalCases : states[indexB].pastWeekCases;
  let valueA = totalA /. ((pageState.col == Total) ? 1.0 : states[indexA].population *. 100000.0);
  let valueB = totalB /. ((pageState.col == Total) ? 1.0 : states[indexB].population *. 100000.0);
  let result = if (valueA > valueB) {
    1
  } else if (valueA < valueB) {
    0
  } else {
    1
  }
  (pageState.dir == Ascending) ? result : -result;
}

let to100K = (x: float, index: int): float => {
  x /. Data.csv.states[index].population *. 100000.0;
}
  
let makeRow = (index) => {
  open Webapi.Dom;
  Js.log(string_of_int(index));
  let tr = document |> Document.createElement("tr");
  let td1 = document |> Document.createElement("td");
  Element.setInnerHTML(td1, Data.csv.states[index].name);
  let td2 = document |> Document.createElement("td");
  Element.setInnerHTML(td2, Js.Float.toFixedWithPrecision(Data.csv.states[index].totalCases, ~digits=0));
  let td3 = document |> Document.createElement("td");
  Element.setInnerHTML(td3, Js.Float.toFixedWithPrecision(to100K(Data.csv.states[index].totalCases, index), ~digits=2));
  Node.appendChild(Element.asNode(td1), Element.asNode(tr));
  Node.appendChild(Element.asNode(td2), Element.asNode(tr));
  Node.appendChild(Element.asNode(td3), Element.asNode(tr))
  let table = document |> Document.getElementById("tableBody");
  switch (table) {
    | Some(el) => Node.appendChild(Element.asNode(tr), Element.asNode(el))
    | None => ()
  }
}

let rec renderFirstData = () => {
  open Belt.Array;
  Js.log2("Checking states length: ", length(Data.csv.states));
  if (length(Data.csv.states) > 0) {
    pageState.indices = makeBy(length(Data.csv.states), (i) => i);
    let _ = map(pageState.indices, makeRow);
    ()
  } else {
    let _ = Js.Global.setTimeout(renderFirstData, 1000);
    ()
  }
}

Js.log("About to fetch data");
Data.fetchData();
renderFirstData();




