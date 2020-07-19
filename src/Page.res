module D = Webapi.Dom;
module Doc = Webapi.Dom.Document;
module Elem = Webapi.Dom.Element;
module Node = Webapi.Dom.Node;
module Evt = Webapi.Dom.Event;

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
  let setChosen = (id: string, chosen: bool): unit => {
    switch (Doc.getElementById(id, D.document)) {
      | Some(element) => {
          if (chosen) {
            Elem.setAttribute("class", "arrow chosen", element);
          } else {
            Elem.setAttribute("class", "arrow", element);
          }
        }
      | None => ()
    }
  }
  
  let setTitle = (titleStr) => {
  
    let setArrow = (dirStr) => {
      let dir = directionFromString(dirStr);
      setChosen(titleStr ++ "_" ++ dirStr, 
        columnFromString(titleStr) == pageState.column && dir == pageState.direction);
    }
    
    let col = columnFromString(titleStr);
    setChosen(titleStr, col == pageState.column);
    Belt.Array.forEach(["Ascending", "Descending"], setArrow);
  }
    
  let wordIds = ["State", "Total", "Per100K"];
  Belt.Array.forEach(wordIds, setTitle)
  
  let t1 = Doc.getElementById("timePeriod1", D.document);
  let t2 = Doc.getElementById("timePeriod2", D.document);
  switch ((t1, t2)) {
    | (Some(el1), Some(el2)) => {
        if (pageState.period == All) {
          Elem.setAttribute("selected", "selected", el2);
          Elem.removeAttribute("selected", el1);
        } else {
          Elem.setAttribute("selected", "selected", el1);
          Elem.removeAttribute("selected", el2);
        }
      }
    | (_, _) =>();
  }

}

let sortIndices = (indices: array<int>): array<int> => {
  let result = switch (pageState.column) {
    | State => Belt.SortArray.stableSortBy(indices, byStateName)
    | _ => Belt.SortArray.stableSortBy(indices, byTotal)
  }
  // Js.log(result);
  result;
}

let makeRow = (absoluteIndex, rankIndex) => {

  let tr = Doc.createElement("tr", D.document);
  
  let td0 = Doc.createElement("td", D.document);
  Elem.setInnerHTML(td0, string_of_int(absoluteIndex + 1));
    
  let td1 = Doc.createElement("td", D.document);
  Elem.setInnerHTML(td1, Data.csv.states[rankIndex].name);
  
  let td2 = Doc.createElement("td", D.document);
  Elem.setAttribute("class", "rightAlign", td2);
  let total = (pageState.period == All) ? Data.csv.states[rankIndex].totalCases :
    Data.csv.states[rankIndex].pastWeekCases;
  Elem.setInnerHTML(td2, Js.Float.toFixedWithPrecision(total, ~digits=0));
  
  let td3 = Doc.createElement("td", D.document);
  Elem.setAttribute("class", "rightAlign", td3);
  let perCapita = (pageState.period == All) ? Data.csv.states[rankIndex].totalCases :
    Data.csv.states[rankIndex].pastWeekCases;
  Elem.setInnerHTML(td3, Js.Float.toFixedWithPrecision(to100K(perCapita, rankIndex), ~digits=2));
  Node.appendChild(Elem.asNode(td0), Elem.asNode(tr));
  Node.appendChild(Elem.asNode(td1), Elem.asNode(tr));
  Node.appendChild(Elem.asNode(td2), Elem.asNode(tr));
  Node.appendChild(Elem.asNode(td3), Elem.asNode(tr))
  let table = Doc.getElementById("tableBody", D.document);
  switch (table) {
    | Some(el) => Node.appendChild(Elem.asNode(tr), Elem.asNode(el))
    | None => ()
  }
}

let rec removeChildren = (parent): unit => {
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
  open Belt.Array;
  let tableElement = Doc.getElementById("tableBody", D.document);
  switch (tableElement) {
    | Some(element) => {
        let _ = removeChildren(Elem.asNode(element));
        let newIndices = sortIndices(makeBy(length(Data.csv.states), (i) => i));
        let _ = mapWithIndex(newIndices, makeRow);
      }
    | None => ()
  }
  setHeaders();
}

let changeColumn = (evt) => {
  let col = D.EventTarget.unsafeAsElement(Evt.target(evt));
  pageState.column = columnFromString(Elem.id(col));
  drawTable();
}

let changeDirection = (evt) => {
  let colDir = D.EventTarget.unsafeAsElement(Evt.target(evt));
  let parts = Js.String2.split(Elem.id(colDir), "_");
  pageState.column = columnFromString(parts[0]);
  pageState.direction = directionFromString(parts[1]);
  drawTable();
}

let changeTimePeriod = (evt) => {
  let menu = Elem.unsafeAsHtmlElement(D.EventTarget.unsafeAsElement(Evt.target(evt)));
  pageState.period = (D.HtmlElement.value(menu) == "1") ? PastWeek : All;
  drawTable();
}

// utility routine
let addClick = (handler, element) => {
  D.EventTarget.addEventListener("click", handler, Elem.asEventTarget(element));
}

let setEventHandlers = () => {

  let attachDirectionEvent = (heading, arrow) => {
    switch (Doc.getElementById(heading ++ "_" ++ arrow, D.document)) {
      | Some(element) => addClick(changeDirection, element)
      | None => ();
    }
  }

  Belt.Array.forEach(["State", "Total", "Per100K"],
    (heading) => {
      switch (Doc.getElementById(heading, D.document)) {
        | Some(element) => {
            addClick(changeColumn, element);
            Belt.Array.forEach(["Ascending", "Descending"],
              (arrow) => {attachDirectionEvent(heading, arrow)})
            }
        | None => ()
      }
    });
    
  switch (Doc.getElementById("timePeriod", D.document)) {
    | Some(element) => addClick(changeTimePeriod, element)
    | None => ()
  }
    
}

let setTimePeriods = () => {
  let time1 = Doc.getElementById("timePeriod1", D.document);
  let time2 = Doc.getElementById("timePeriod2", D.document);
  let len = Belt.Array.length(Data.csv.dates);
  switch ((time1, time2)) {
    | (Some(elem1), Some(elem2)) => {
        Elem.setInnerHTML(elem1, Elem.innerHTML(elem1) ++ " " ++ Data.csv.dates[len - 8] ++ " - "
          ++ Data.csv.dates[len - 1])
        Elem.setInnerHTML(elem2, Elem.innerHTML(elem2) ++ " " ++ Data.csv.dates[0] ++ " - "
          ++ Data.csv.dates[len - 1])
        }
    | (_, _) => ()
  }
}


