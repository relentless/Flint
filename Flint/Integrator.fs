module Integrator

open Parser
open SyntaxTree
open Printer
open Expander
open CoreFunctions
open Evaluator
open System.IO

let integrate environment functions expression =
    {Expression = expression; Environment = environment; Functions = functions }

let process input evaluationRecord =
    {evaluationRecord with Expression = input |> parse |> expand} |> evaluate

let result evaluationRecord = evaluationRecord.Expression

let print expression = printfn "%s" (expression |> toString)

let loadCoreLib() =
    File.ReadAllText("CoreLib.flint")
    |> parse
    |> expand
    |> integrate initialEnvironment initialFunctions
    |> evaluate

let execute text =
    {loadCoreLib() with Expression = text |> parse |> expand}
    |> evaluate 
    |> result
    |> toString 

let evalInitial expression = 
    expression
    |> expand
    |> integrate initialEnvironment initialFunctions 
    |> evaluate 
    |> result