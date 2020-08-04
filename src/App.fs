module App

open Browser.Dom

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector("#calculate-button") :?> Browser.Types.HTMLButtonElement
let attackerInput = document.querySelector("#attacker-input") :?> Browser.Types.HTMLInputElement
let defenderInput = document.querySelector("#defender-input") :?> Browser.Types.HTMLInputElement

let output = document.querySelector("#output") :?> Browser.Types.HTMLDivElement

type State = { Attacker: int; Defender: int }
type StateProbability = { Probability: float; Outcome: State }
type Probabilities = StateProbability list
type Output = Map<State, Probabilities>

let probabilitiesForState state =
    if state.Attacker < 2 || state.Defender < 1 then
        sprintf "Cannot fight in state %A" state
        |> invalidOp
    
    let rec generateThrownNumbers throwCount =
        if throwCount = 0 then [[]]
        else
            throwCount - 1
            |> generateThrownNumbers
            |> List.collect (
                fun xs ->
                    [1..6] |> List.map (fun x -> x :: xs)
            )         

    let attackerThrows =
        if state.Attacker > 3 then 3 elif state.Attacker = 3 then 2 else 1
        |> generateThrownNumbers

    let defenderThrows =
        if state.Defender > 1 then 2 else 1
        |> generateThrownNumbers

    let rec throwOutcome (attackerThrows, defenderThrows) =
        let rec removeHighestThrow throws =
            match throws with
            | [] -> "Failed calculation" |> invalidOp
            | [x] -> x, []
            | x :: xs ->
                let (otherMax, otherXs) = removeHighestThrow xs
                if x > otherMax then
                    x, otherMax::otherXs
                else
                    otherMax, x::otherXs

        if defenderThrows |> List.isEmpty || attackerThrows |> List.isEmpty then 0, 0
        else
            let (attHigh, attXs) = attackerThrows |> removeHighestThrow
            let (defHigh, defXs) = defenderThrows |> removeHighestThrow
            let (attO, defO) = throwOutcome (attXs, defXs)
            if attHigh > defHigh then
                (attO, defO + 1)
            else
                (attO + 1, defO)

    attackerThrows
    |> List.collect (fun a -> defenderThrows |> List.map (fun d -> a, d))
    |> List.map (throwOutcome >> (fun (a,d) -> { Attacker = state.Attacker - a; Defender = state.Defender - d }))
    |> fun x ->
        let size = x |> List.length |> float
        let probability = 1.0 / size
        x |> List.map (fun x -> x, probability)
    |> List.groupBy fst
    |> List.map (fun (resultState, probabilities) -> resultState, probabilities |> List.sumBy snd)

let calculateProbabilities finalState =
    if finalState.Attacker < 2 then
        sprintf "Cannot attack with %d units" finalState.Attacker
        |> invalidOp
    elif finalState.Defender < 1 then
        sprintf "Cannot defend with %d units" finalState.Defender
        |> invalidOp
    else
        let initialStates =
            [
                [ 1..finalState.Attacker ] |> List.map (fun x -> { Attacker = x; Defender = 0 })
                [ 1..finalState.Defender ] |> List.map (fun x -> { Attacker = 1; Defender = x })
            ]
            |> List.concat
            |> List.map (fun x -> x, { Probability = 1.0; Outcome = x } |> List.singleton)
            |> Map.ofList

        let rec buildUntilFinalState (calculated: Output)  statesToBuild =
            match statesToBuild with
            | [] -> "Failed to find a way to the end result" |> invalidOp
            | x :: xs ->
                match calculated.TryGetValue x with
                | true, result ->
                    if x = finalState then
                        result
                    else
                        buildUntilFinalState calculated xs
                | false, _ ->
                    let probabilities =
                        probabilitiesForState x
                        |> List.map (
                            fun (state, prob) ->
                                state, prob, calculated |> Map.tryFind state
                        )

                    let calculationMissing =
                        probabilities
                        |> List.choose (
                            function
                            | state, _, None ->
                                Some state
                            | _ ->
                                None
                        )

                    if calculationMissing |> List.isEmpty then
                        probabilities
                        |> List.collect (
                            function
                            | _, prob, Some(outcomes) ->
                                outcomes
                                |> List.map (
                                    fun outcome ->
                                        outcome.Outcome, outcome.Probability * prob
                                )
                            | _ ->
                                List.empty
                        )
                        |> List.groupBy fst
                        |> List.map (
                            fun (s,probs) ->
                                { Probability = probs |> List.sumBy snd; Outcome = s }
                        ) 
                        |> Map.add x <| calculated
                        |> buildUntilFinalState <| x :: xs
                    else
                        calculationMissing @ (x :: xs)
                        |> buildUntilFinalState calculated

        buildUntilFinalState initialStates [ finalState ]

// Register our listener
myButton.onclick <- fun _ ->
    output.innerHTML <- ""

    try
        let attackerCount = attackerInput.valueAsNumber |> int
        let defenderCount = defenderInput.valueAsNumber |> int
        let finalState = { Attacker = attackerCount; Defender = defenderCount }
        let probs = finalState |> calculateProbabilities

        let attackerWins =
            probs
            |> List.filter (fun x -> x.Outcome.Defender = 0)
            |> List.sortByDescending (fun x -> x.Outcome.Attacker)

        let defenderWins =
            probs
            |> List.filter (fun x -> x.Outcome.Defender <> 0)
            |> List.sortByDescending (fun x -> x.Outcome.Defender)

        let numberToPerc num =
            num * 100.0 |> sprintf "%.2f%%"

        let createContent text cases (div: Browser.Types.HTMLElement) =
            let prob = cases |> List.sumBy (fun x -> x.Probability)
            let header = document.createElement("h1")
            header.innerText <- sprintf "%s: %s" text (prob |> numberToPerc)
            div.appendChild(header) |> ignore
            let list = document.createElement("ul")
            cases
            |> List.iter (
                fun case ->
                    let item = document.createElement("li")
                    item.innerText <- sprintf "Attacker: %d, Defender: %d, Probability: %s" case.Outcome.Attacker case.Outcome.Defender (case.Probability |> numberToPerc)
                    list.appendChild(item) |> ignore
            )
            div.appendChild(list) |> ignore

        createContent "Attacker wins" attackerWins output
        createContent "Defender wins" defenderWins output
    with
        | exc -> output.innerText <- exc.Message
