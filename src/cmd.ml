
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
  let b = Ics.simplify !current a in
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
      Exc.Inconsistent _ -> printf "Inconsistent.@."

    
    (*s Order of terms. *)
    
let less (t1,t2) =
  Format.printf "%s@."
    (if Ics.term_cmp t1 t2 < 0 then "True" else "False")

    
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
  Format.printf "can <term>.             Computes the canonical form of <term>.@.";
  Format.printf "solve <term> = <term>.  Solves equation on terms.@.";
  Format.printf "assert <term>.          Asserts <term> to the current context.@.";
  Format.printf "check <term>.           Check if <term> holds or is unsatisfiable in current context@.";
  Format.printf "                        Returns T if <term> is redundant, F if it is inconsistent,@.";
  Format.printf "                        otherwise silent.@.";
  Format.printf "find [<term>].          Representative of the equivalence class of <term>.@.";
  Format.printf "                        If <term> is omitted, the complete find structure is displayed.@.";
  Format.printf "ext [<term>].           Prints equivalence class of <term> without <term> itself.@.";
  Format.printf "                        If <term> is omitted, extensions of all canonical forms are displayed.@.";
  Format.printf "use [<term>].           Displays the canonical representatives in which";
  Format.printf "                        <term> occurs interpreted.@.";
  Format.printf "                        If <term> is omitted, the complete use structure is displayed.@.";
  Format.printf "cnstrnt [<term>].       Compute type interpretation of <term> wrt. current context.@.";
  Format.printf "                        If <term> is omitted, then the constraints for all canonical representatives";
  Format.printf "                        are displayed";
  Format.printf "verbose <int>.          Sets the verbose level.@.";
  Format.printf "reset.                  Reset to empty context.@.";
  Format.printf "drop.                   Drop into Caml if run on bytecode (back with Main.repl ();;).@."

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

<tuple> ::= '(' <term> ',' ... ',' <term> ')'                   Tuple  
          | proj '[' const ',' const ']' '(' <term> ')'         Projection

<set> ::= empty | full                         
        | <term> union <term> 
        | <term> diff <term>           
        | <term> symdiff <term>
        | <term> inter <term>          
        | compl <term> 

<bv> ::= constbv                                   
       | conc(<fixed>, <fixed>)               Concatenation
       | bvand(<fixed>,<fixed>)               Bitwise conjunction
       | bvor(<fixed>, <fixed>)               Bitwise disjunction   
       | bvxor(<fixed>,<fixed>)               Bitwise xor
       | extr '['const ':' const ']' <fixed>  Extraction

<fixed> ::= <term> 
          | <term> '[' const ']'
    
<prop> ::= true             
         | false            
         | <atom>            
         | <term> '&&' <term>        Conjunction
         | <term> '||' <term>        Disjunction
         | <term> '=>' <term>        Implication
         | <term> '<=>' <term>       Biimplication
         | '~' <term>                Negation

<atom> ::= <unary> '(' <term> ')'
         | <term> <binary> <term>    

<binary> ::= '=' | '<>' | '<' | '>' | '<=' | '>' | '>=' | in | notin

<unary> ::= int | real | pos | neg | nonpos | nonneg   @."


