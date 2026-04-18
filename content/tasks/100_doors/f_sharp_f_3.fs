let modifier doors skip =
    let rec modifier' doors skip counter result =
        match doors with
        | [] -> result |> List.rev                                  //base case: end of hall
        | first::rest when counter >= skip ->                       //case: reached door marked for change
            modifier' rest skip 0 ((not first)::result)             //  open or close that door
        | first::rest ->                                            //case: reached door to skip
            modifier' rest skip (counter+1) (first::result)         //  skip it
    modifier' doors skip 0 []                                       //Initial state for walkthrough
