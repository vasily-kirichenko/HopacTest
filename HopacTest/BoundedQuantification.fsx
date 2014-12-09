#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let defTrue: Alt<bool> = Alt.always true
let specializeOnFalse onFalse b = b >>= function true -> upcast defTrue | false -> onFalse