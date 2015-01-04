#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System.Linq
open System
open System.Security.Cryptography

let parallelMap (f: 'T -> 'R) input = 
    let rec loop input = Job.delay <| fun _ ->
        match input with 
        | [] -> Job.result []
        | x :: xs -> Job.lift f x <*> loop xs |>> fun (y, ys) -> y :: ys
    run (loop input)

// test with Sleep function

let sleepF _ = System.Threading.Thread.Sleep 10
let xs = [1..100]
xs |> List.map sleepF |> ignore 
xs.AsParallel().Select(sleepF).ToList() |> ignore
xs |> parallelMap sleepF |> ignore

// test with very fact function and big list

let calcF x = sin (float x) / cos (float x)
let ys = [1..5000000]
ys |> List.map calcF |> ignore
ys.AsParallel().Select(calcF).ToList() |> ignore
ys |> parallelMap calcF |> ignore

// test with calculation SHA1 of each elements of a relatively small list of 10kB data

let calcSha1 (x: byte[]) =
    use sha1 = SHA1.Create()
    sha1.ComputeHash x 
let rnd = Random()
let zs = [1..100000] |> List.map (fun _ -> 
    let bytes = Array.zeroCreate (10 * 1024)
    rnd.NextBytes bytes
    bytes)
zs |> List.map calcSha1 |> ignore
zs.AsParallel().Select(calcSha1).ToList() |> ignore
zs |> parallelMap calcSha1 |> ignore
  