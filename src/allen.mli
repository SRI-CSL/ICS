
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

(*s Module [Allen]: Datatype for interval relations.  The intended
 interpretations are as follows.
 [i before j] (resp. [j after i] holds iff 
 [i] is to the left of [j] without touching it, [i meets j] (resp.
 [j metby i] holds iff [i]  is to the left of [j] and it touches [j],
 [i overlaps j] (resp. [j overlappedby i]) holds iff if the intersection
 of [i] and [j] is nonempty, and the endpoints of [i] are to-the-left of
 the corresponding endpoints of [j], [i sub j] (resp. [j super i]) holds
 if the denotation of [i] is a strict subset of the denotation of [j] not
 containing the endpoints of [j], 
 [i starts j] (resp. [j startedby i]) iff [i sub j] and in addition 
 the left endpoints of [i] and [j] are identical, [i finishes j]
 (resp. [j finishedby i]) iff [i sub j] and the right endpoints of
 [i] and [j] are identical, and finally, [i equals j] if the
 respective endpoints of [i] and [j] are equal. *)

type t =
  | Before   
  | After    
  | Meets    
  | MetBy    
  | Overlaps 
  | OverlappedBy
  | Sub      
  | Super    
  | Starts    
  | StartedBy 
  | Finishes  
  | FinishedBy
  | Equals
