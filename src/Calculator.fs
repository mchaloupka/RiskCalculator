namespace RiskCalculator

open System.Collections.Generic

open RiskCalculator
open RiskCalculator.JsUtil

module Calculator =
    let rec private generateThrownNumbers throwCount =
        if throwCount = 0 then [[]]
        else
            throwCount - 1
            |> generateThrownNumbers
            |> List.collect (
                fun xs ->
                    [1..6] |> List.map (fun x -> x :: xs)
            )

    let rec private throwOutcome (attackerThrows, defenderThrows) =
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

    let private aggregateStates outputs =
        let rec aggregate (output: IDictionary<int, IDictionary<int, float>>) toProcess =
            match toProcess with
            | (state, probability) :: xs ->
                let attackerOutput =
                    match output.TryGetValue state.Attacker with
                    | true, x -> x
                    | false, _ ->
                        let x = Seq.empty |> dict
                        output.[state.Attacker] <- x
                        x

                let defenderOutput =
                    match attackerOutput.TryGetValue state.Defender with
                    | true, x -> x
                    | false, _ -> 0.0

                attackerOutput.[state.Defender] <- defenderOutput + probability

                aggregate output xs

            | _ -> output
        
        outputs
        |> aggregate (Seq.empty |> dict)
        |> dictToArray
        |> Array.collect (
            fun (attacker, attackerMap) ->
                attackerMap
                |> dictToArray
                |> Array.map (
                    fun (defender, probability) ->
                        { Outcome = { Attacker = attacker; Defender = defender }; Probability = probability }
                )
        )

    let private generatePossibleOutcome attackerThrowCounts defenderThrowCounts =
        let attackerThrows = attackerThrowCounts |> generateThrownNumbers
        let defenderThrows = defenderThrowCounts |> generateThrownNumbers

        attackerThrows
        |> List.collect (fun a -> defenderThrows |> List.map (fun d -> a, d))
        |> List.map (throwOutcome >> (fun (a,d) -> { Attacker = a; Defender = d }))
        |> fun x ->
            let size = x |> List.length |> float
            let probability = 1.0 / size
            x |> List.map (fun x -> x, probability)
        |> aggregateStates

    let private generatePossibleOutcomeForAttacker attackerThrowCounts =
        Map.empty
        |> Map.add 2 (generatePossibleOutcome attackerThrowCounts 2)
        |> Map.add 1 (generatePossibleOutcome attackerThrowCounts 1)

    let private possibleOutcomes = 
        Map.empty
        |> Map.add 3 (generatePossibleOutcomeForAttacker 3)
        |> Map.add 2 (generatePossibleOutcomeForAttacker 2)
        |> Map.add 1 (generatePossibleOutcomeForAttacker 1)

    let probabilitiesForState state =
        if state.Attacker < 2 || state.Defender < 1 then
            sprintf "Cannot fight in state %A" state
            |> invalidOp

        let attackerThrows =
            if state.Attacker > 3 then 3 elif state.Attacker = 3 then 2 else 1

        let defenderThrows =
            if state.Defender > 1 then 2 else 1

        possibleOutcomes.[attackerThrows].[defenderThrows]
        |> Array.map (
            fun probability ->
                { 
                    Outcome = { 
                        Attacker = state.Attacker - probability.Outcome.Attacker
                        Defender = state.Defender - probability.Outcome.Defender 
                    }
                    Probability = probability.Probability
                }
        )

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
                |> List.map (fun x -> x, { Probability = 1.0; Outcome = x } |> Array.singleton)
                |> Cache.build

            let rec buildUntilFinalState (calculated: Cache) statesToBuild =
                match statesToBuild with
                | [] -> "Failed to find a way to the end result" |> invalidOp
                | x :: xs ->
                    match calculated |> Cache.tryGetValue x with
                    | Some result ->
                        if x = finalState then
                            result
                        else
                            buildUntilFinalState calculated xs
                    | None ->
                        let probabilities =
                            probabilitiesForState x
                            |> Array.map (
                                fun probability ->
                                    probability.Outcome, probability.Probability, calculated |> Cache.tryGetValue probability.Outcome
                            )

                        let calculationMissing =
                            probabilities
                            |> Array.choose (
                                function
                                | state, _, None ->
                                    Some state
                                | _ ->
                                    None
                            )

                        if calculationMissing |> Array.isEmpty then
                            probabilities
                            |> Array.collect (
                                function
                                | _, prob, Some(outcomes) ->
                                    outcomes
                                    |> Array.map (fun outcome -> outcome.Outcome, outcome.Probability * prob)
                                | _ ->
                                    Array.empty
                            )
                            |> Array.toList
                            |> aggregateStates
                            |> Cache.add x <| calculated
                            |> buildUntilFinalState <| x :: xs
                        else
                            (calculationMissing |> Array.toList) @ (x :: xs)
                            |> buildUntilFinalState calculated

            buildUntilFinalState initialStates [ finalState ]
