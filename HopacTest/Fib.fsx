#load "Header.fsx"
#load @"packages\FSharp.Charting.0.90.6\FSharp.Charting.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open FSharp.Charting
open System.Threading.Tasks

let rec fib n = 
    if n < 2L then n
    else fib (n - 1L) + fib (n - 2L)

let hfib_jobs = ref 0L

let rec hfib (n, level) = Job.delay <| fun _ ->
  hfib_jobs := !hfib_jobs + 1L
  if n < 2L then
    Job.result n
  elif n < level then Job.result (fib n)
  else
    hfib (n-2L, level) <*> hfib (n-1L, level) |>> fun (x, y) ->
    x + y

let hfib_promise_jobs = ref 0L

let rec hfibp (n, level) =
  hfib_promise_jobs := !hfib_promise_jobs + 1L
  if n < 2L then
    Job.result n
  elif n < level then Job.result (fib n)
  else
    hfibp (n-2L, level) |> Promise.start >>= fun n2p ->
    hfibp (n-1L, level) >>= fun n1 ->
    n2p |>> fun n2 -> n2 + n1

let hfib_opt_jobs = ref 0L

let rec hfibopt (n, level) =
  hfib_opt_jobs := !hfib_opt_jobs + 1L
  if n < 2L then Job.result n
  elif n < level then Job.result (fib n)
  else
    hfibopt (n-2L, level) <*> Job.delayWith hfibopt (n-1L, level) |>> fun (n2, n1) ->
    n2 + n1

let afib_asyncs = ref 0L

let rec afib (n, level) = async {
  afib_asyncs := !afib_asyncs + 1L
  if n < 2L then
    return n
  elif n < level then return fib n
  else
    let! n2a = afib (n-2L, level) |> Async.StartChild
    let! n1 = afib (n-1L, level)
    let! n2 = n2a
    return n2 + n1
}

let tfib_tasks = ref 0L

let rec tfib (n, level) =
  if n < 2L then n
  elif n < level then fib n
  else
    let n2t = Task.Factory.StartNew (fun _ -> 
        tfib_tasks := !tfib_tasks + 1L
        tfib (n-2L, level))
    let n1 = tfib (n-1L, level)
    n2t.Result + n1

let time f =
    let w = System.Diagnostics.Stopwatch.StartNew()
    f() |> ignore
    w.Stop()
    w.ElapsedMilliseconds

let n = 42L
let levels = [7L..12L]
let go name f =
    levels
    |> List.map (fun level -> printfn "%s (level = %d)" name level; level, time (fun _ -> f (n, level)))
    |> fun data -> Chart.Line (data, Name = name)

hfib_jobs := 0L
afib_asyncs := 0L
tfib_tasks := 0L
hfib_promise_jobs := 0L
hfib_opt_jobs := 0L
let sfib_time = time (fun _ -> fib n)
(levels |> List.map (fun level -> level, sfib_time) |> fun data -> Chart.Line (data, Name = "fib")) ::
([//"hfib", hfib >> run 
  //"hfibp", hfibp >> run
  "hfibopt", hfibopt >> run]
 //"afib", afib >> Async.RunSynchronously] 
 //"tfib", tfib ]
|> List.map (fun (name, f) -> go name f))
|> Chart.Combine

fib 42L
hfib (42L, 9L) |> run
hfibopt (42L, 9L) |> run
afib (42L, 17L) |> Async.RunSynchronously
tfib (42L, 11L)
  
!hfib_jobs, !hfib_promise_jobs, !hfib_opt_jobs, !afib_asyncs, !tfib_tasks



(*
1. Sequential, desktop GC

> run (fib 38L);;
Real: 00:00:09.259, CPU: 00:00:09.250, GC gen0: 4705, gen1: 1, gen2: 0
val it : int64 = 39088169L

2. Parallel, desktop GC
> run (fib 38L);;
Real: 00:00:04.751, CPU: 00:00:14.586, GC gen0: 5093, gen1: 1, gen2: 0
val it : int64 = 39088169L

3. Sequential, server GC
> run (fib 38L);;
Real: 00:00:08.638, CPU: 00:00:08.860, GC gen0: 229, gen1: 0, gen2: 0
val it : int64 = 39088169L

4. Parallel, server GC
> run (fib 38L);;
Real: 00:00:02.897, CPU: 00:00:17.425, GC gen0: 197, gen1: 0, gen2: 0
val it : int64 = 39088169L
*)