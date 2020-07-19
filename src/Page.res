type column =
  | State
  | Total
  | Per100K;

let columnToString = (col) => {
  switch (col) {
    | State => "State"
    | Total => "Total"
    | Per100K => "Per100K"
  }
}

let columnFromString = (s) => {
  switch (s) {
    | "Total" => Total
    | "Per100K" => Per100K
    | _ => State
  }
}

type sortDirection =
  | Ascending
  | Descending;
  
let directionToString = (dir) => {
  switch (dir) {
    | Ascending => "Ascending"
    | Descending => "Descending"
  }
}

let directionFromString = (s) => {
  switch (s) {
    | "Ascending" => Ascending
    | _ => Descending
  }
}

type timePeriod =
  | All
  | PastWeek;
  
let periodToString = (period) => {
  switch (period) {
    | All => "All"
    | PastWeek => "PastWeek"
  }
}

let periodFromString = (s) => {
  switch (s) {
    | "PastWeek" => PastWeek
    | _ => All
  }
}


type pageStateType = {
  mutable column: column,
  mutable direction: sortDirection,
  mutable period: timePeriod,
  mutable indices: array<int>
}

let pageState: pageStateType = {
  column: Per100K,
  direction: Descending,
  period: PastWeek,
  indices: []
};

let to100K = (x: float, index: int): float => {
  x /. Data.csv.states[index].population *. 100000.0;
}

let byStateName = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let result = if (states[indexA].name > states[indexB].name) {
    1
  } else if (states[indexA].name < states[indexB].name) {
    -1
  } else {
    0
  }
  (pageState.direction == Ascending) ? result : -result;
}

let byTotal = (indexA: int, indexB: int): int => {
  let states = Data.csv.states;
  let totalA = (pageState.period == All) ? states[indexA].totalCases : states[indexA].pastWeekCases;
  let totalB = (pageState.period == All) ? states[indexB].totalCases : states[indexB].pastWeekCases;
  let valueA = totalA /. ((pageState.column == Total) ? 1.0 : states[indexA].population /. 100000.0);
  let valueB = totalB /. ((pageState.column == Total) ? 1.0 : states[indexB].population /. 100000.0);
  let result = if (valueA > valueB) {
    1
  } else if (valueA < valueB) {
    -1
  } else {
    0
  }

  (pageState.direction == Ascending) ? result : -result;
}


let setHeaders = () => {
  open Webapi.Dom

  let setChosenTitle = (id: string, chosen: bool): unit => {
    let el = Document.getElementById(id, document);
    switch (el) {
      | Some(element) => {
          if (chosen) {
            Element.setAttribute("class", "chosen", element);
          } else {
            Element.removeAttribute("class", element);
          }
        }
      | None => ()
    }
  }
  
  let setArrow = (str) => {
    let parts = Js.String2.split(str, "_");
    if (Belt.Array.length(parts) == 1) {
      let col = columnToString(parts[0]);
      setChosen(col, true);
    } else {
      ()
    }
  }
      
  let arrowIds = ["State_Ascending", "State_Descending", "Total_Ascending", "Total_Descending",
    "Per100K_Ascending", "Per100K_Descending"];
  let wordIds = ["State", "Total", "Per100K"];
  Belt.Array.forEach(wordIds, setArrow)
}

let sortIndices = (indices: array<int>): array<int> => {
  Js.log(columnToString(pageState.column))
  let result = switch (pageState.column) {
    | State => Belt.SortArray.stableSortBy(indices, byStateName)
    | _ => Belt.SortArray.stableSortBy(indices, byTotal)
  }
  Js.log(result);
  result;
}

let makeRow = (index) => {
  open Webapi.Dom;

  let tr = document |> Document.createElement("tr");
  let td1 = document |> Document.createElement("td");
  Element.setInnerHTML(td1, Data.csv.states[index].name);
  
  let td2 = document |> Document.createElement("td");
  Element.setAttribute("class", "rightAlign", td2);
  let total = (pageState.column == Total) ? Data.csv.states[index].totalCases :
    Data.csv.states[index].pastWeekCases;
  Element.setInnerHTML(td2, Js.Float.toFixedWithPrecision(total, ~digits=0));
  
  let td3 = document |> Document.createElement("td");
  Element.setAttribute("class", "rightAlign", td3);
  let perCapita = (pageState.column == Total) ? Data.csv.states[index].totalCases :
    Data.csv.states[index].pastWeekCases;
  Element.setInnerHTML(td3, Js.Float.toFixedWithPrecision(to100K(perCapita, index), ~digits=2));
  Node.appendChild(Element.asNode(td1), Element.asNode(tr));
  Node.appendChild(Element.asNode(td2), Element.asNode(tr));
  Node.appendChild(Element.asNode(td3), Element.asNode(tr))
  let table = document |> Document.getElementById("tableBody");
  switch (table) {
    | Some(el) => Node.appendChild(Element.asNode(tr), Element.asNode(el))
    | None => ()
  }
}

let rec removeChildren = (parent): unit => {
  open Webapi.Dom;
  let lastChild = Node.lastChild(parent);
  switch (lastChild) {
    | Some(child) => {
        let _ = Node.removeChild(child, parent);
        removeChildren(parent);
      }
    | None => ()
  }
}

let drawTable = () => {
  open Webapi.Dom;
  open Belt.Array;
  let tableElement = document |> Document.getElementById("tableBody");
  switch (tableElement) {
    | Some(element) => {
        let _ = removeChildren(Element.asNode(element));
        let newIndices = sortIndices(makeBy(length(Data.csv.states), (i) => i));
        Js.log2("sorted: ", newIndices);
        let _ = map(newIndices, makeRow);
      }
    | None => ()
  }
}
