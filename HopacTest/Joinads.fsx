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


//let onePlaceBuffer() = 
//  let put = SyncChannel<string, unit>()
//  let get = SyncChannel<string>()
//  let empty = Channel<unit>()
//  let contains = Channel<string>()
//  
//  // Initially, the buffer is empty
//  empty.Call(())
//
//  join {
//    match! put, empty, get, contains with 
//    | (s, repl), (), ?, ? -> return react {
//      yield contains.Put(s)
//      yield repl.Reply() }
//    | ?, ?, repl, v -> return react {
//      yield repl.Reply(v)
//      yield empty.Put(()) } }
//
//  // Repeatedly try to put value into the buffer
//  async { do! Async.Sleep(1000)
//          for i in 0 .. 10 do
//            printfn "putting: %d" i
//            do! put.AsyncCall(string i)
//            do! Async.Sleep(500) }
//  |> Async.Start
//
//  // Repeatedly read values from the buffer and print them
//  async { while true do 
//            do! Async.Sleep(250)
//            let! v = get.AsyncCall()
//            printfn "got: %s" v }
//  |> Async.Start
  
let onePlaceBuffer() = 
  let put = SyncChannel<string, unit>()
  let get = SyncChannel<string>()
  let empty = Channel<unit>()
  let contains = Channel<string>()
  
  // Initially, the buffer is empty
  empty.Call(())

  join {
    match! put, empty, get, contains with 
    | (s, repl), (), ?, ? -> return react {
      yield contains.Put(s)
      yield repl.Reply() }
    | ?, ?, repl, v -> return react {
      yield repl.Reply(v)
      yield empty.Put(()) } }

  // Repeatedly try to put value into the buffer
  async { do! Async.Sleep(1000)
          for i in 0 .. 10 do
            printfn "putting: %d" i
            do! put.AsyncCall(string i)
            do! Async.Sleep(500) }
  |> Async.Start

  // Repeatedly read values from the buffer and print them
  async { while true do 
            do! Async.Sleep(250)
            let! v = get.AsyncCall()
            printfn "got: %s" v }
  |> Async.Start