﻿#load "Header.fsx"

open Hopac
open Hopac.Extensions
open Hopac.Alt.Infixes
open System.Net
open System

let download (url: string) : Alt<string * string> = 
    Async.toAlt <| async {
        use webClient = new WebClient()
        let! res = webClient.AsyncDownloadString (Uri url)
        return url, res
    }
    
Alt.choose [ download "http://yahoo.com"
             download "http://google.com"
             download "http://ya.ru"
             Timer.Global.timeOut (TimeSpan.FromMilliseconds 3000.) >>%? ("timed", "out") ]
|> run


run (download "http://ya.ru")