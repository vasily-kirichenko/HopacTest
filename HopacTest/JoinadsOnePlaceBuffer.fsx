#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let put, get, empty, contains = ch(), ch(), ch(), ch()

// Initially, the buffer is empty
run (empty <-+ ())

Job.foreverServer (
    Alt.choose [ empty >>.? (put >>= Ch.send contains)
                 contains >>=? fun v -> Ch.send get v >>. Ch.send empty () ])
|> run

// Repeatedly try to put value into the buffer
job { do! Async.Sleep 1000
      for i in 0 .. 10 do
          printfn "putting: %d" i
          do! put <-- string i
          do! Async.Sleep 500 }
|> start

// Repeatedly read values from the buffer and print them
job { while true do 
          do! Async.Sleep 250
          let! v = get
          printfn "got: %s" v }
|> start

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
  
