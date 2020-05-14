/* 
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
 */

/* A simplified parser for interface declarations. */

%{
  open Mli_types
%}

%token <string> IDENT
%token <string> LONGIDENT
%token VAL COLON
%token INT BOOL UNIT STRING VALUE ARROW EOF
%right ARROW
%type <Mli_types.t> signature
%type <string * Mli_types.t> declaration
%start declaration
%start signature

%%

declaration: VAL IDENT COLON signature { ($2, $4) }

signature:
  constant EOF  { $1 } 
| functional EOF { $1 }
;

constant : ground            { { dom = []; cod = $1 } }

functional: dom ARROW ground { { dom = List.rev $1; cod = $3 } }

dom:
  ground           { [$1] }
| dom ARROW ground { $3 :: $1 }
;

ground:
  | INT                                  { Int }
  | BOOL                                 { Bool }
  | UNIT                                 { Unit }
  | STRING                               { String }
  | VALUE                                { Value }
;

%%

  
