(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

let on_help () = 
  Format.eprintf "@[";
  Format.eprintf "help.           Displays this mesage.@\n";
  Format.eprintf "help commands.  Lists all commands.@\n";
  Format.eprintf "help syntax.    Outlines syntactic categories.@]@."


let commands () =
  Format.eprintf "@[";
  Format.eprintf "assert <atom>.            Assert atom in current context.@\n";
  Format.eprintf "can <term>.               Canonical forms.@\n";
  Format.eprintf "ctxt.                     Current logical context.@\n";
  Format.eprintf "def <var> := <term>.      Term definition.@\n"; 
  Format.eprintf "diseq <term>.             Variables known to be disequal.@\n";
  Format.eprintf "dom <term>.               Domain of interpretation@\n";
  Format.eprintf "exit.                     Exiting ICS interpreter (or: Ctrl-D)@\n";
  Format.eprintf "forget.                   Clearing current logical context.@\n";
  Format.eprintf "find <th> <var>.          Interpretation of variable in theory.@\n";
  Format.eprintf "help [commands | syntax]. Help texts.@\n";
  Format.eprintf "inv <th> <term>.          External variable for term in theory@\n";
  Format.eprintf "prop <var> := <prop>.     Definition for a proposition.@\n"; 
  Format.eprintf "reset.                    Reinitializing ICS.@\n";
  Format.eprintf "restore <name>.           Restore current context.@\n";
  Format.eprintf "remove <name>.            Remove a saved current context.@\n";
  Format.eprintf "save <name>.              Save current context.@\n";
  Format.eprintf "sat <prop>.               Propositional satisfiability@\n";
  Format.eprintf "sig <var> : bv[<int>].    Signature for bitvector variables.@\n";
  Format.eprintf "sig <var> : [dom | real]. Signature for arithmetic variables.@\n";
  Format.eprintf "sigma (<term> | <atom>).  Normal forms.@\n";
  Format.eprintf "sign <term>.              Abstrat sign interpretation of a term.@\n";
  Format.eprintf "show.                     Solution sets and variable partitioning.@\n";
  Format.eprintf "solve <ith> <term> = <term>. Solve equation in interpreted theory.@\n";
  Format.eprintf "symtab [<name>].          Symbol table entries.@\n";
  Format.eprintf "trace <ident>,...,<ident>. Tracing.@\n"; 
  Format.eprintf "unsat <prop>.             Check for unsatisfiability.@\n";
  Format.eprintf "untrace.                  Stop tracing.@\n";
  Format.eprintf "use <th> <var>.           Occurrences of variables.@\n";
  Format.eprintf "valid <prop>.             Validity check.@\n";
  Format.eprintf "An interpreted theory <ith> is either 'a' for arithmetic, 't' for@\n";
  Format.eprintf "tuples, or 'bv' for bitvectors. In addition, <th> includes 'u' the@\n";
  Format.eprintf "theory of unintepreted terms";
  Format.eprintf "@]@."


let syntax () =
  let pr = Format.eprintf in
  pr "@[";
  pr "<term> ::= <var> @\n";  
  pr "         | <name> '(' <term> {',' <term>}* ')' >@\n";
  pr "         | <arith> | <tuple> | <array> | <sexpr> | <bv> | <boolean>@\n";
  pr "         | '(' <term> ')'@\n\n";
  pr "<var> ::= <ident> | <ident> '!' <int>@\n\n";
  pr "<arith> ::= <rational> | <term> {'+' | '-' | '*'} <term>@\n\n";
  pr "          | '-' <term> | <term> '^' <int>@\n";
  pr "<tuple> ::= '(' <term> {',' <term>}* ')'@\n";
  pr "          | 'proj[' <int> ',' <int> ']' '(' <term> ')'@\n\n";
  pr "<sexpr> ::= 'nil' | 'cons' '(' <term> ',' <term> ')'@\n";
  pr "          | {'car' | 'cdr'} '(' <term> ')'@\n\n";
  pr "<array> ::= <term> '[' <term> ':=' <term> ']'@\n";
  pr "          | <term> '[' <term> ']'@\n\n";
  pr "<boolean> ::= 'true' | 'false'@\n\n";
  pr "<bv> ::= '0b' {'0' | '1'}* | <term> '++' <term>@\n";
  pr "       | <term> '['<int>':'<int>']' '('<term>')'@\n";
  pr "       | <term> {'&&' | '||' | '##'} <term>@\n\n";
  pr "<atom> ::= 'tt' | 'ff' | <term> {'=' | '<>' | '<' | '>' | '<=' | '>='} <term>@\n";
  pr "<prop> ::= <atom> | '[' <prop> ']' | <prop> ['&' | '|' | '=>' | '<=>' | '#'] <prop>@\n";
  pr "           | '~'<prop>@\n | 'if' <prop> 'then' <prop> 'else' <prop> 'end'@\n";
  pr "<ident> ::=  ('A'|..|'Z'|'a'|..|'z') {'A'|..|'Z'|'a'|..|'z'|'\''|'0'|..|'9'}*@\n";
  pr "<int> ::= {'0'|..|'9'}+@\n";
  pr "<rat> ::= <int> {'/' <int>}@\n\n";
  pr "Comments start with percent and end with a newline.@\n";
  pr "Identifiers do not include any keywords and command names.@\n";
  pr "Operator precedence in increasing order:@\n";
  pr "'-', '+', '*', '/', '^', '++', '||', '##', '&&'.@\n";
  pr "Conventions:@\n";
  pr "nonterminals: <..>@\n";
  pr "terminals:  '..'@\n";
  pr "choice: .. | ..@\n";
  pr "optional: { .. }@\n";
  pr "zero or more repetions: {..}*@\n";
  pr "one or more repetions: {..}+@\n";
  pr "@]@."


