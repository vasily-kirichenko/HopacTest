#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"
#r "packages/ExtCore.0.8.43/lib/net40/ExtCore.dll"

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type Buffer<'a> =
    { InsCh: Ch<'a>
      RemCh: Ch<'a> }

let server maxLen = Job.delay <| fun _ ->
    let buff = { InsCh = ch(); RemCh = ch() }
    let rec loop = function
        | [] -> Ch.take buff.InsCh >>= fun x -> loop [x]
        | (h :: t) as es when List.length es >= maxLen ->
            Ch.Alt.give buff.RemCh h >>. loop t
        | (h :: t) as es ->
            Alt.select [ Ch.Alt.give buff.RemCh h >>=? fun _ -> loop t
                         Ch.Alt.take buff.InsCh >>=? fun x -> loop (es @ [x]) ]
    Job.start (loop []) >>% buff

let insert buff x = Ch.give buff.InsCh x
let remove buff = Ch.take buff.RemCh

let buff: Buffer<Guid> = run (server 3)
start (Job.result (printfn "Removing...") >>. remove buff |>> fun x -> printfn "Removed %A" x)
start (Job.result (printfn "Inserting...") >>. 
    Job.result (Guid.NewGuid()) >>= fun guid ->
    insert buff guid |>> fun _ -> printfn "Inserted %A" guid)  


