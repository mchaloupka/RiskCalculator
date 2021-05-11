namespace RiskCalculator

open JsUtil

type State = { Attacker: int; Defender: int }
type StateProbability = { Probability: float; Outcome: State }
type Probabilities = StateProbability array
type Cache = Probabilities array array

module Cache =
    let private tryFind (index: int) (arr: array<'T>): 'T option =
        let item = arr.[index]
        if (isDefined(item)) then
            Some item
        else
            None

    let tryGetValue (state: State) (cache: Cache) =
        tryFind state.Attacker cache
        |> Option.map (tryFind state.Defender)
        |> Option.flatten

    let add (state: State) (probabilities: Probabilities) (cache: Cache) =
        let attackerCache = 
            cache
            |> tryFind state.Attacker
            |> Option.defaultValue [||]

        attackerCache.[state.Defender] <- probabilities
        cache.[state.Attacker] <- attackerCache
        cache
        
    let build =
        let rec buildFromEntries cache toProcess =
            match toProcess with
            | (state, probabilities) :: xs ->
                cache |> add state probabilities |> buildFromEntries <| xs
            | _ -> cache

        buildFromEntries [||]