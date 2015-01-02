#load "Header.fsx"
#r "System.Threading.Tasks"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System

let reqCh = ch()
let respCh = ch()

let server() = 
    Job.iterateServer () <| fun _ ->
        Ch.take reqCh >>= fun x -> Ch.give respCh (x * x)

let call x = Ch.give reqCh x >>.? Ch.take respCh 

start (server())
run (call 1000)
 
// ------------------

let rnd = Random().Next(100, 1000)
let c = ch()
job {
    do! Async.Sleep rnd
    do! c <-- "success!"
} |> start

Alt.choose [ c |>>? sprintf "Value: %s"
             Timer.Global.timeOutMillis 5000 >>%? sprintf "Timed out" ]
|> run

//let addCh = ch<int>()
//let subCh = ch<int>()
//let readCh = ch<int>()
//
//let accum: Alt<unit> =
//    let rec loop sum =
//        Alt.choose [ Ch.Alt.take addCh >>=? fun x -> upcast loop (sum + x)
//                     Ch.Alt.take subCh >>=? fun x -> upcast loop (sum - x)
//                     Ch.Alt.give readCh sum >>=? fun _ -> upcast loop sum ]
//    loop 0
//
//let server =
//    Job.iterateServer () <| fun _ ->
//        Alt.select [ accum >>%? printfn "accum!"
//                     Timer.Global.timeOut (TimeSpan.FromSeconds 2.) >>%? printfn "timeout!" ]
//
//start server
//run (Ch.give addCh 1)
//run (Ch.take readCh |>> printfn "%A") 
//run (Alt.pick accum)

