
(*i
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
 * 
 * Author: Harald Ruess
 i*)

type t = {
  vfocus : V.focus;
  dfocus : D.focus
}

let v f = f.vfocus
let d f = f.dfocus

let make v d = { vfocus = v; dfocus = d}

let empty = {
  vfocus = V.Focus.empty;
  dfocus = D.Focus.empty
}

let is_empty f =
  V.Focus.is_empty f.vfocus &&
  D.Focus.is_empty f.dfocus

let add_v v f = {f with vfocus = V.Focus.union v f.vfocus}
let add_d d f = {f with dfocus = D.Focus.union d f.dfocus}

let union f1 f2 = 
  {vfocus = V.Focus.union f1.vfocus f2.vfocus;
   dfocus = D.Focus.union f1.dfocus f2.dfocus}

let fold_v f p = V.Focus.fold f p.vfocus

let fold_d f p = D.Focus.fold f p.dfocus
