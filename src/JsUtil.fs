module RiskCalculator.JsUtil

open Fable.Core.Util
open Fable.Core
open System.Collections.Generic

[<Emit("!!$0")>]
let isDefined(x: 'T): bool = jsNative

[<Emit("Array.from($0)")>]
let dictToArray (data: IDictionary<'TK, 'TV>): ('TK * 'TV) array = jsNative
