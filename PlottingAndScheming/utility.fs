//namespace vanmeule.FSharp.PlottingAndScheming

(* Copyright (c) 2009, 2008, 2007, 2006 by André van Meulebrouck.  All rights reserved worldwide. *)

module PlottingAndScheming.Utility

let rec takeWhile p l =
  match l with
  | [] -> []
  | x :: xs when p x -> x :: takeWhile p xs
  | _ -> []