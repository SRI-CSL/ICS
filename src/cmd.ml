

(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 i*)

(*i*)
open Ics
open Tools
open Format
(*i*)

  
(*s Current state. *)
  
let current = ref (init())

		
(*s Setting the level of trace output. The larger the natural [n], the more
    output is provided. *)

let verbose n =
  Ics.set_verbose n

    (*s The command [process] introduces a new fact. The state
      is left unchanged is an inconsistency is discovered.
    *)
  
let process a =
  match Ics.process !current a with
    | Ics.Consistent st ->
	current := st; Format.printf "Ok.@."
    | Ics.Valid ->
	Format.printf "Redundant.@."
    | Ics.Inconsistent ->
	Format.printf "Inconsistent.@."

let check t =
 (match Ics.process !current t with
    | Ics.Valid ->
	Format.printf "Valid.@."
    | Ics.Inconsistent ->
	Format.printf "Inconsistent.@."
    | Ics.Consistent _ ->
	Format.printf "No inconsistency detected.@.")

  (*s Print current state on standard output. *)

let curr () =
  Ics.state_pp !current
  
  
    (*s Querying the state. *)
    
let find = function
  | Some(a) ->
      let b = Ics.find !current a in
      Format.printf "@["; Ics.term_pp b; Format.printf "@]@."
  | None ->
      Ics.subst_pp (Ics.find_of !current)


let use = function
  | Some(a) ->
      Ics.list_pp Ics.term_pp (Ics.use !current a)
  | None ->
      Ics.map_pp Ics.terms_pp (Ics.use_of !current)

let ctxt () =
  Ics.list_pp Ics.eqn_pp (Ics.ctxt_of !current)


let cnstrnt = function
  | None ->
      Ics.map_pp Ics.cnstrnt_pp (Ics.cnstrnt_of !current)
  | Some(a) ->
      Ics.cnstrnt_pp(Ics.cnstrnt !current a)
 
   
let ext = function
  | None ->
      Ics.map_pp Ics.terms_pp (Ics.ext_of !current)
  | Some(a) ->
      let b = Ics.find !current a in 
      if Ics.term_eq a b then
	Ics.list_pp Ics.term_pp (Ics.ext !current b)
      else
	Format.printf "Undefined.@."

let uninterp = function
  | None ->
      Format.printf "to do@."
  | Some(a) ->
      Ics.list_pp Ics.term_pp (Ics.uninterp !current a)

	
	(*s Simplifiers *)

let sigma a =
  Format.printf "@["; Ics.term_pp a; Format.printf "@]@."
			 
let norm a =
  let b = Ics.norm !current a in
  Format.printf "@["; Ics.term_pp b; Format.printf "@]@."

let simp a =
  let b = Ics.simplify a in
  Format.printf "@["; Ics.term_pp b; Format.printf "@]@."
     
let can a =
  let b = Ics.can !current a in
  Format.printf "@["; Ics.term_pp b; Format.printf "@]@."

    
    (*s Solver command [solve]. *)
    
let solve x (a,b) =
  let st = !current in
  let e' = (Ics.can st a, Ics.can st b) in
  try
    Ics.subst_pp (Ics.solve x st e')
  with
      Exc.Inconsistent -> printf "Inconsistent.@."

   (*s Get solution for a term. *)

let solution a =
  let st = !current in
  if Ics.is_solvable a then
    match Ics.solution st a with
      | Some(b) ->
	  Format.printf "@["; Ics.term_pp b; Format.printf "@]@."
      | None ->
	  Format.printf "@[No solution@]@."
  else
    Format.printf "@[Not a solvable.@]@."
      
    
    (*s Order of terms. *)
    
let less (t1,t2) =
  Format.printf "%s.@."
    (if Ics.term_cmp t1 t2 < 0 then "Yes" else "No")

    
    (*s Reset and dropping into byte-code interpreter *)
			 
let reset () =
  Ics.reset ();
  current := init()

let drop () =
  failwith "drop"

    
    (*s Help functions. *)

let help () =
  Format.printf "help commands.         Lists all commands.@.";
  Format.printf "help syntax.           Outlines term syntax.@."

let help_commands () =
  Format.printf "assert <term>.          Asserts <term> to the current context.@.";
  Format.printf "                        There are three different outcomes. In case, <term> is@.";
  Format.printf "                        found to be implied by the current context, `Valid.' is @.";
  Format.printf "                        returned, and if <term> is found to be inconsistent, @.";
  Format.printf "                        'Inconsistent.' is printed. Otherwise, <term> is@.";
  Format.printf "                        added to the current context@.";
  Format.printf "check <term>.           Check if <term> holds or is unsatisfiable@.";
  Format.printf "                        Returns 'Valid.' if <term> is redundant, 'Inconsistent.'@.";
  Format.printf "                        if it is inconsistent, otherwise silent. In contrast @.";
  Format.printf "                        to the assert command, the context is not updated.@.";
  Format.printf "sigma <term>.           Normal form using theory-specific normalizations.@.";
  Format.printf "                        only. In particular, no context information is used.@.";
  Format.printf "can <term>.             Computes canonical representative of <term> using.@.";
  Format.printf "                        context information.@.";
  Format.printf "simp <term>.            Computes a normal form of <term>. 'simp' is like 'can',@.";
  Format.printf "                         but without recursive processing in conditionals.@.";
  Format.printf "norm <term>.            Simplifies all interpreted terms, and replaces @.";
  Format.printf "                        uninterpreted terms with their finds.@.";
  Format.printf "solve <term> = <term>.  Solves equation on terms.@.";
  Format.printf "find [<term>].          Canonical representative of the equivalence class@.";
  Format.printf "                        of <term> as stored in the context.@.";
  Format.printf "                        If <term> is omitted, all finds are displayed.@.";
  Format.printf "ext [<term>].           Prints equivalence class of <term> (without <term>).@.";
  Format.printf "                        If <term> is omitted, all extensions are displayed.@.";
  Format.printf "use [<term>].           Displays the canonical representatives in which@.";
  Format.printf "                        <term> occurs interpreted.@.";
  Format.printf "                        If <term> is omitted, all uses are shown.@.";
  Format.printf "cnstrnt [<term>].       Type interpretation of <term> wrt. current context.@.";
  Format.printf "                        If <term> is omitted, then all constraints are shown.@.";
  Format.printf "<term> << <term>.       Check term ordering.@.";
  Format.printf "verbose <int>.          Sets the verbose level.@.";
  Format.printf "reset.                  Reset to empty context.@.";
  Format.printf "drop.                   Drop into Caml bytecode (back with Main.repl ();;).@."

let help_syntax () =
  Format.printf
"<term> ::= ident                                            
         | <term> '(' <term> ',' ... ',' <term> ')'              Function application
         | <term> '[' <term> ':=' <term> ']'                     Function update
         | <prop> | <arith> | <tuple> | <array> | <set> | <bv>   Interpreted terms 
         | '(' <term> ')'                             

<arith> ::= <term> '+' <term>
          | <term> '-' <term>              
          | <term> '*' <term>
          | const ['/' const]               
          | '-' <term>                     

<tuple> ::= '(' <term> ',' ... ',' <term> ')'              Tuple  
          | proj '[' int ',' int ']' '(' <term> ')'         Projection

<set> ::= empty
        | full                         
        | <term> union <term> 
        | <term> diff <term>           
        | <term> symdiff <term>
        | <term> inter <term>          
        | compl <term> 

<bv> ::= constbv                                   
       | <fixed> ++ <fixed>                  Concatenation
       | <fixed> && <fixed>                  Bitwise conjunction
       | <fixed> || <fixed>                  Bitwise disjunction   
       | <fixed> ## <fixed>                  Bitwise xor
       | <fixed> '[' int ':' int ']'          Extraction

<fixed> ::= <term> 
          | <term> '[' int ']'
    
<prop> ::= true             
         | false            
         | <atom>            
         | <term> '&' <term>        Conjunction
         | <term> '|' <term>        Disjunction
         | <term> '=>' <term>        Implication
         | <term> '<=>' <term>       Biimplication
         | '~' <term>                Negation

<atom> ::= <unary> '(' <term> ')'
         | <term> <binary> <term>    

<binary> ::= '=' | '<>' | '<' | '>' | '<=' | '>' | '>=' | in | notin

<unary> ::= int | real | pos | neg | nonpos | nonneg   @."


