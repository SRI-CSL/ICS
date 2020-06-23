(* 
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 SRI International
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *)

(** Version information populated by build system *)

let debug =
  let d = ref false in
  assert (
    d := true ;
    true ) ;
  !d

module Build_info = Build_info.V1

let version_to_string = function
  | Some v -> Build_info.Version.to_string v
  | None -> "dev"

let version =
  Format.sprintf "ICS %s%s"
    (version_to_string (Build_info.version ()))
    (if debug then " (DEBUG = true)" else "")

let build_info =
  let libs =
    List.map
      (fun lib ->
        ( Build_info.Statically_linked_library.name lib
        , version_to_string
            (Build_info.Statically_linked_library.version lib) ) )
      (Build_info.Statically_linked_libraries.to_list ())
    |> List.sort Stdlib.compare
  in
  let max_length =
    List.fold_left (fun n (name, _) -> max n (String.length name)) 0 libs
  in
  String.concat "\n"
    ( Printf.sprintf "%-*s %s" (max_length + 2) "ICS:"
        (version_to_string (Build_info.version ()))
    :: Printf.sprintf "%-*s %b" (max_length + 2) "debug:" debug
    :: Printf.sprintf "%-*s %s" (max_length + 2) "ocaml:" Sys.ocaml_version
    :: "statically linked libraries:"
    :: List.map
         (fun (name, v) -> Printf.sprintf "- %-*s %s" max_length name v)
         libs )
