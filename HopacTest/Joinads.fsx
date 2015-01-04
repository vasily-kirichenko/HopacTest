// Joinads code
//
//let putString = Channel<string>("puts")
//let putInt = Channel<int>("puti")
//let get = SyncChannel<string>("get")
//
//join {
//  match! get, putString, putInt with
//  | repl, v, ? -> return react { yield repl.Reply("Echo " + v) }
//  | repl, ?, v -> return react { yield repl.Reply("Echo " + (string v)) } 
//}
//
//// Put 5 values to 'putString' and 5 values to 'putInt'
//for i in 1 .. 5 do 
//  putString.Call("Hello!")
//  putInt.Call(i)
//
//// Repeatedly call 'get' to read the next value. This is a blocking
//// operation, so it should be done from asynchronous workflow to 
//// avoid blocking physical threads.
//async { 
//  while true do
//    let! repl = get.AsyncCall()
//    printfn "got: %s" repl }
//|> Async.Start

#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Alt.Infixes
open Hopac.Job.Infixes

let putString = mb()
let putInt = mb()
let get = ch()

Job.foreverServer (
    Alt.choose [ putString >>=? fun v -> get <-- sprintf "Echo %s" v
                 putInt >>=? fun v -> get <-- sprintf "Echo %d" v ])
|> run

Job.foreverServer (get |>> printfn "GOT: %s") |> run

// Put 5 values to 'putString' and 5 values to 'putInt'
job {
    for i in 1 .. 5 do
        do! putString <<-+ "Hello!"
        do! putInt <<-+ i 
} |> run


