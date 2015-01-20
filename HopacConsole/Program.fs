open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System.Threading.Tasks
open System.Threading

[<EntryPoint>]
let main _ =
    let time f =
        let w = System.Diagnostics.Stopwatch.StartNew()
        let res = f()
        w.Stop()
        res, w.Elapsed

    let n = 1000000
    let (start', finish), elapsed = time (fun _ ->
        let start' = ivar()
        let finish =
            seq {1..n}
            |> Seq.fold (fun prevV _ ->
                let v = ivar()
                (prevV >>= ((+) 1 >> IVar.fill v)) |> start
                v
            ) start'
        start', finish)
    
    printfn "%d jobs created in %O (%f jobs/sec)." n elapsed ((float n / elapsed.TotalMilliseconds) * 1000.)

    time (fun _ ->
        start (start' <-= 0)
        run finish)
    |> fun (res, elapsed) -> printfn "Result = %d, elapsed %O." res elapsed

    ///let e = new ManualResetEvent false

//    for _ in 1..n do
//        async { 
//            let! _ = Async.AwaitWaitHandle e
//            () 
//        } |> Async.Start

    
//    for _ in 1..n do
//        Task.Factory.StartNew (fun _ -> e.WaitOne()) |> ignore

//    for _ in 1..n do
//        job { do! Async.Sleep 1000000 } |> start
//
//    let s = ivar()
//    for _ in 1..n do
//        start (s |>> printfn "%A")

    printfn "done."

//    let rec fib n = 
//        if n < 2L then n
//        else fib (n - 1L) + fib (n - 2L)
//
//    let hfib_jobs = ref 0L
//
//    let rec hfib (n, level) = Job.delay <| fun () ->
//      hfib_jobs := !hfib_jobs + 1L
//      if n < 2L then
//        Job.result n
//      elif n < level then Job.result (fib n)
//      else
//        hfib (n-2L, level) <*> hfib (n-1L, level) |>> fun (x, y) ->
//        x + y
//
//    let afib_asyncs = ref 0L
//
//    let rec afib (n, level) = async {
//      afib_asyncs := !afib_asyncs + 1L
//      if n < 2L then
//        return n
//      elif n < level then return fib n
//      else
//        let! n2a = afib (n-2L, level) |> Async.StartChild
//        let! n1 = afib (n-1L, level)
//        let! n2 = n2a
//        return n2 + n1
//    }
//
//    let tfib_tasks = ref 0L
//
//    let rec tfib (n, level) =
//      if n < 2L then n
//      elif n < level then fib n
//      else
//        let n2t = Task.Factory.StartNew (fun _ -> 
//            tfib_tasks := !tfib_tasks + 1L
//            tfib (n-2L, level))
//        let n1 = tfib (n-1L, level)
//        n2t.Result + n1
//
//    let time f =
//        let w = System.Diagnostics.Stopwatch.StartNew()
//        f() |> ignore
//        w.Stop()
//        w.Elapsed
//
//    hfib_jobs := 0L
//    afib_asyncs := 0L
//    tfib_tasks := 0L
//
//    //fib 42L |> ignore
//    //let elapsed = time (fun _ -> hfib (42L, 15L) |> run)
//    //let elapsed = time (fun _ -> afib (42L, 15L) |> Async.RunSynchronously)
//    let elapsed = time (fun _ -> tfib (42L, 15L))
//  
//    printfn "Elapsed = %O" elapsed
//    printfn "Hopac jobs = %d, Asyncs = %d, Tasks = %d" !hfib_jobs !afib_asyncs !tfib_tasks
    System.Console.ReadKey() |> ignore
    0

