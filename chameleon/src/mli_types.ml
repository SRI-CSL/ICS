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


(* Simplified version of caml types. *)

type ground = 
  | Int 
  | Bool 
  | Unit 
  | String 
  | Value

let ground_to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Unit -> "unit"
  | String -> "string"
  | Value -> "value"

let ground_pp fmt t = 
  Format.fprintf fmt "%s" (ground_to_string t)


type t = {
  dom : ground list; 
  cod
 : ground
}

let pp fmt = function 
  | {dom = []; cod = c} ->
      Format.fprintf fmt "unit -> ";
      ground_pp fmt c
  | {dom = dl; cod = c} -> 
      List.iter (fun d -> ground_pp fmt d; Format.fprintf fmt " -> ") dl;
      ground_pp fmt c
    
let to_string x = 
  pp Format.str_formatter x;
  Format.flush_str_formatter ()
