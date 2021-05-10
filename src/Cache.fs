namespace RiskCalculator

type Cache = Map<int, Map<int, Probabilities>>

module Cache =
    let tryGetValue (state: State) (cache: Cache) =
        match cache.TryFind state.Attacker with
        | Some attackerCache ->
            attackerCache.TryFind state.Defender
        | _ -> None

    let add (state: State) (probabilities: Probabilities) (cache: Cache) =
        let attackerCache = 
            cache
            |> Map.tryFind state.Attacker
            |> Option.defaultValue Map.empty
        
        let updatedAttackerCache = attackerCache |> Map.add state.Defender probabilities

        cache |> Map.add state.Attacker updatedAttackerCache
        
    let empty = Map.empty

    let build =
        let rec buildFromEntries cache toProcess =
            match toProcess with
            | (state, probabilities) :: xs ->
                cache |> add state probabilities |> buildFromEntries <| xs
            | _ -> cache

        buildFromEntries empty