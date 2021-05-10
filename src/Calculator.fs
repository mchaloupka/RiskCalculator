namespace RiskCalculator

type State = { Attacker: int; Defender: int }
type StateProbability = { Probability: float; Outcome: State }
type Probabilities = StateProbability list

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
            let rec aggregate output toProcess =
                match toProcess with
                | (state, probability) :: xs ->
                    let attackerOutput =
                        output
                        |> Map.tryFind state.Attacker
                        |> Option.defaultValue Map.empty

                    let defenderOutput =
                        attackerOutput
                        |> Map.tryFind state.Defender
                        |> Option.defaultValue 0.0

                    let newDefenderOutput = defenderOutput + probability
                    let newAttackerOutput =
                        attackerOutput
                        |> Map.add state.Defender newDefenderOutput
                   
                    output
                    |> Map.add state.Attacker newAttackerOutput
                    |> aggregate <| xs
                | _ -> output
            
            outputs
            |> aggregate Map.empty
            |> Map.toList
            |> List.collect (
                fun (attacker, attackerMap) ->
                    attackerMap
                    |> Map.toList
                    |> List.map (
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
        |> List.map (
            fun probability ->
                { 
                    Outcome = { 
                        Attacker = state.Attacker - probability.Outcome.Attacker
                        Defender = state.Defender - probability.Outcome.Defender 
                    }
                    Probability = probability.Probability
                }
        )