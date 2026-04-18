let modifier doors skip =
    let rec modifierInner doors skip counter =
        match doors with
        | [] -> []                                                  //base case: end of hall
        | first::rest when counter >= skip ->                       //case: reached door marked for change
            not first::(modifierInner rest skip 0)                  //  open or close that door
        | first::rest ->                                            //case: reached door to skip
            first::(modifierInner rest skip (counter+1))            //  skip it
    modifierInner doors skip 0                                      //Initial state for walkthrough

let answerDoors doors =
    let rec modifyDoors skipRange doors modifier =                  //fold each door result to the next with
        List.fold modifier doors skipRange                          //with an increasing skip
    modifyDoors [0..99] doors modifier                              //Initial starting state

let initDoors = Array.create 100 false |> Array.toList              //Initialize all doors to closed (false)

answerDoors initDoors |> printfn "%A"                               //print answer (false is closed door)
