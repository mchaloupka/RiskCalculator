module RiskCalculator.App

open RiskCalculator
open Browser.Dom

// Get a reference to our button and cast the Element to an HTMLButtonElement
let myButton = document.querySelector("#calculate-button") :?> Browser.Types.HTMLButtonElement
let attackerInput = document.querySelector("#attacker-input") :?> Browser.Types.HTMLInputElement
let defenderInput = document.querySelector("#defender-input") :?> Browser.Types.HTMLInputElement

let output = document.querySelector("#output") :?> Browser.Types.HTMLDivElement

// Register our listener
myButton.onclick <- fun _ ->
    output.innerHTML <- ""

    try
        let attackerCount = attackerInput.valueAsNumber |> int
        let defenderCount = defenderInput.valueAsNumber |> int
        let finalState = { Attacker = attackerCount; Defender = defenderCount }
        let probs = finalState |> Calculator.calculateProbabilities

        let attackerWins =
            probs
            |> Array.filter (fun x -> x.Outcome.Defender = 0)
            |> Array.sortByDescending (fun x -> x.Outcome.Attacker)

        let defenderWins =
            probs
            |> Array.filter (fun x -> x.Outcome.Defender <> 0)
            |> Array.sortByDescending (fun x -> x.Outcome.Defender)

        let numberToPerc num =
            num * 100.0 |> sprintf "%.2f%%"

        let createContent text cases (div: Browser.Types.HTMLElement) =
            let prob = cases |> Array.sumBy (fun x -> x.Probability)
            let header = document.createElement("h1")
            header.innerText <- sprintf "%s: %s" text (prob |> numberToPerc)
            div.appendChild(header) |> ignore
            let list = document.createElement("ul")
            cases
            |> Array.iter (
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
