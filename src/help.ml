
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

let on_help () = 
  Format.eprintf "@[";
  Format.eprintf "help.           Displays this mesage.@\n";
  Format.eprintf "help commands.  Lists all commands.@\n";
  Format.eprintf "help syntax.    Outlines syntactic categories.@]@."


let commands () =
  Format.eprintf "@[";
  Format.eprintf "sigma (<term> | <atom>). Normal forms.@\n";
  Format.eprintf "can (<term> | <atom>).   Canonical forms.@\n";
  Format.eprintf "cnstrnt <term>.          Constraint associated with term.@\n";
  Format.eprintf "diseq <term>.            Variables known to be disequal.@\n";   
  Format.eprintf "solve <ith> <term> = <term>. Solve equation in interpreted theory.@\n";
  Format.eprintf "assert <atom>.           Assert atom in current context.@\n";
  Format.eprintf "ctxt.                    Current logical context.@\n";
  Format.eprintf "partition.               Current variable partitioning.@\n";
  Format.eprintf "solution <th>.           Solution sets for individual theory.@\n";
  Format.eprintf "show.                    Solution sets and variable partitioning.@\n";
  Format.eprintf "forget.                  Clearing current logical context.@\n";
  Format.eprintf "reset.                   Reinitializing ICS.@\n";
  Format.eprintf "find <th> <var>.         Interpretation of variable in theory.@\n";
  Format.eprintf "inv <th> <term>.         External variable for term in theory@\n"; 
  Format.eprintf "<term> << <term>.        Comparing two terms.@\n";
  Format.eprintf "verbose <int>.           Enable/disable tracing.@\n";
  Format.eprintf "def <var> := <term>.     Term definition.@\n";
  Format.eprintf "type <var> := <cnstrnt>. Type definition.@\n";
  Format.eprintf "sig <var> : bv[<int>].   Signature extension.@\n";
  Format.eprintf "save <name>.             Save current context.@\n";
  Format.eprintf "restore <name>.          Restore current context.@\n";
  Format.eprintf "remove <name>.           Remove a saved current context.@\n";
  Format.eprintf "symtab [<name>].         Symbol table entries.@\n";
  Format.eprintf "gc.                      Garbage collection in current context.@\n";
  Format.eprintf "help [commands | syntax]. Help texts.@\n";
  Format.eprintf "exit.                    Exiting ICS interpreter (or: Ctrl-D)@\n\n";
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
  pr "<atom> ::= <term> {'=' | '<>' | '<' | '>' | '<=' | '>='} <term>@\n";
  pr "         | <term> 'in' <cnstrnt>@\n\n";
  pr "<cnstrnt> ::= <ident>@\n";
  pr "            | ('int'|'real') {<left> '..' <right>}  @\n";
  pr "<left> ::= '(-inf' | '(' <rat> | '[' <rat>@\n";
  pr "<right> ::= 'inf)' | <rat> ')' | <rat> ']'@\n\n";
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

