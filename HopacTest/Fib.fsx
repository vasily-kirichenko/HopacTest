#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let rec fib n = Job.delay <| fun () ->
  if n < 2L then
    Job.result n
  else
    fib (n-2L) <*> fib (n-1L) |>> fun (x, y) ->
    x + y

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