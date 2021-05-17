module RiskCalculator.Components

open Feliz

type InputComponentProps = { Input: State; SetInput: State -> unit }

let InputComponent = React.functionComponent(fun (props: InputComponentProps) ->
    let setInput = props.SetInput
    let currentInput = props.Input
    let (currentError, setCurrentError) = React.useState("")
    
    let updateState = 
        React.useCallback ((fun newState ->
            let validateValue value name =
                if value > 0 && value <= 100 then
                    true
                else
                    sprintf "%s count should be more than 0 and less or equal 100" name |> setCurrentError
                    false
            
            if (validateValue newState.Attacker "Attacker") && (validateValue newState.Defender "Defender") then
                setCurrentError ""
                newState |> setInput    
        ), [| setCurrentError, setInput |])

    let parseNumber (defaultValue) (text: string) =
        match System.Int32.TryParse text with
        | true, n -> n
        | false, _ ->
            setCurrentError "Only numeric values are allowed"
            defaultValue
    
    let updateAttacker =
        React.useCallback ((fun newAttacker -> { currentInput with Attacker = parseNumber currentInput.Attacker newAttacker } |> updateState), [|currentInput, updateState|])

    let updateDefender =
        React.useCallback ((fun newDefender -> { currentInput with Defender = parseNumber currentInput.Defender newDefender } |> updateState), [|currentInput, updateState|])


    Html.div [
        yield Html.div [
            Html.text "Attacker: "
            Html.input [
                prop.type'.number
                prop.onChange updateAttacker
                prop.placeholder "Attacker"
                prop.value currentInput.Attacker
            ]
        ]
        yield Html.div [
            Html.text "Defender: "
            Html.input [
                prop.type'.number
                prop.onChange updateDefender
                prop.placeholder "Defender"
                prop.value currentInput.Defender
            ]
        ]
        if System.String.IsNullOrEmpty currentError |> not then
            yield Html.div [ Html.text currentError ]
    ]
)

let ResultsComponent = React.functionComponent(fun (props: State) ->
    let (computedInput, setComputedInput) = React.useState<State option>(None)
    let (computedResult, setComputedResult) = React.useState<Probabilities>([||])
    let (currentError, setCurrentError) = React.useState("")

    React.useEffect(
        (fun () ->
            try
                setCurrentError ""
                let input = props
                let result = input |> Calculator.calculateProbabilities
                result |> setComputedResult
                input |> Some |> setComputedInput
            with
            | e -> (e.Message |> setCurrentError)           
        ),
        [| box props; box setComputedInput; box setCurrentError; box setComputedResult |]
    )

    if System.String.IsNullOrEmpty currentError |> not then
        Html.div [
            currentError |> Html.text
        ]
    else if computedInput.IsSome && computedInput.Value = props then
        let attackerWins =
            computedResult
            |> Array.filter (fun x -> x.Outcome.Defender = 0)
            |> Array.sortByDescending (fun x -> x.Outcome.Attacker)
        
        let defenderWins =
            computedResult
            |> Array.filter (fun x -> x.Outcome.Defender <> 0)
            |> Array.sortByDescending (fun x -> x.Outcome.Defender)

        let numberToPerc num =
            num * 100.0 |> sprintf "%.2f%%"

        let createContent text cases =
            let prob = cases |> Array.sumBy (fun x -> x.Probability)

            Html.div [
                Html.h1 [ sprintf "%s: %s" text (prob |> numberToPerc) |> Html.text ]
                Html.ul (
                    cases
                    |> Array.toList
                    |> List.map (fun case -> sprintf "Attacker: %d, Defender: %d, Probability: %s" case.Outcome.Attacker case.Outcome.Defender (case.Probability |> numberToPerc))
                    |> List.map (Html.li)
                )
            ]

        Html.div [
            createContent "Attacker wins" attackerWins
            createContent "Defender wins" defenderWins
        ]
    else
        React.fragment []
)

let MainComponent = React.functionComponent(fun () ->
    let (input, setInput) = React.useState({ Attacker = 3; Defender = 3 })

    Html.div [
        InputComponent { Input = input; SetInput = setInput }
        ResultsComponent input
    ]
)
