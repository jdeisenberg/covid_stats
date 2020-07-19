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

let rec drawTable = () => {
  open Belt.Array;
  Js.log2("Checking states length: ", length(Data.csv.states));
  pageState.indices = makeBy(length(Data.csv.states), (i) => i);
  let _ = map(pageState.indices, makeRow);
  /*
  if (length(Data.csv.states) > 0) {
    pageState.indices = makeBy(length(Data.csv.states), (i) => i);
    let _ = map(pageState.indices, makeRow);
    ()
  } else {
    let _ = Js.Global.setTimeout(drawTable, 1000);
    ()
  }
  */
}
