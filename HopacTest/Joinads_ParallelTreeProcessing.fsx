﻿#load "Header.fsx"
#r @"packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll"

open Hopac
open Hopac.Job.Infixes
open FSharpx.TimeMeasurement
 
let nums = [ for i in 0L .. 1000L -> i + 5000000000000L ]

type Tree<'T> = 
  | Leaf of 'T 
  | Node of Tree<'T> * Tree<'T>

/// Creates a ballanced tree from a non-empty list
/// (odd elements are added to the left and even to the right)
let rec ballancedOfList list =
  match list with 
  | [] -> failwith "Cannot create tree of empty list"
  | [n] -> Leaf n
  | _ -> 
      // Split the elements into odd and even using their index
      let left, right =
        list |> List.mapi (fun i v -> i, v)
             |> List.partition (fun (i, v) -> i%2 = 0)
      // Create ballanced trees for both parts
      let left, right = List.map snd left, List.map snd right
      Node(ballancedOfList left, ballancedOfList right)

let isPrime num = 
  seq { 2L .. int64 (sqrt (float num)) } 
  |> Seq.forall (fun div -> num % div <> 0L)

// Create a list with large prime numbers
let primes = 
  nums |> List.map (fun v -> isPrime v, v) 
       |> List.filter fst |> List.map snd
// Create a list with some additional non-primes
let mixed = primes @ [ 2L .. 20L ]

// Created ballanced trees from both lists
let primeTree = ballancedOfList primes
let mixedTree = ballancedOfList mixed

let forall f tree =
  let rec loop tree =
    match tree with
    | Leaf v -> f v 
    | Node (left, right) ->
        // Process left and right branch
        loop left && loop right
  // Start the recursive processing & wait for the result
  loop tree

let parallelForall f tree =
  let rec loop tree = Job.delay <| fun _ ->
    match tree with
    | Leaf v -> Job.lift f v 
    | Node (left, right) ->
        // Process left and right branch in parallel
        loop left <*> loop right |>> fun (l, r) -> l && r
  // Start the recursive processing & wait for the result
  run (loop tree)

// Test processing on two sample trees
compareTwoRuntimes
    5
    "Sequential" (fun _ -> forall isPrime mixedTree)
    "Parallel" (fun _ -> parallelForall isPrime mixedTree)

compareTwoRuntimes
    5
    "Sequential" (fun _ -> forall isPrime primeTree)
    "Parallel" (fun _ -> parallelForall isPrime primeTree)
