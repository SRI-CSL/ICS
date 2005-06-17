(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

(*
external print_version: unit -> unit = "print_version"
*)

open Unix

let date = gmtime (time())
 
let version = Format.sprintf "ICS 2.1 (%d:%d:%d GMC, %d/%d/%d)" 
		date.tm_sec
		date.tm_min
		date.tm_hour
		date.tm_mday
		date.tm_mon
		(date.tm_year + 1900)

let debug = ref 0
