
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

(*s Module [Can]: canonization for terms and atoms. *)



(*s Canonizer Always returns the canonical variable for a term when it exists.
  \begin{tabular}{lll}
      [can s x]  & = & [v s x] & , the canonical variable equivalent to [x]. \\
      [can s f(a)] & = & [can_var(sigma th (f(can_th th s a)))] &,            
  \end{tabular}
  where [th] is the theory of [f], [can_th th s a] is just 
  [find_th s (can s a)], and [can_var s a] returns the canonical 
  variable equivalent to [a], if there is one, and [a], otherwise. *)

val term : Dp.t -> Term.t -> Term.t

val atom : Dp.t -> Atom.t -> Atom.t

val prop : Dp.t -> Prop.t -> Prop.t

