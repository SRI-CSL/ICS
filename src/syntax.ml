let syntax () =
  printf
  "<term> ::= ident                            \n
            | const                            \n
            | <term> ( <term> , ... , <term> ) \n
            | <term> [ <term> := <term> ]      \n
                                               \n
            | <prop>                           \n
            | <arith>                          \n  
            | <tuple>                          \n
            | <array>                          \n
            | <set>                            \n
            | <bv>                             \n
            | ( <term> )                       \n
                                           \n
   <arith> ::= <term> + <term>                 \n
             | <term> - <term>                 \n
             | <term> * <term>                 \n
             | const / const                   \n
             | - <term>                        \n
                                                 \n
   <tuple> ::= ( <term> , ... , <term> )         \n
             | proj [ const , const ] ( <term> ) \n
                                                 \n
   <set> ::= empty                               \n
           | full                                \n
           | <term> union <term>                 \n
           | <term> diff <term>                  \n
           | <term> symdiff <term>               \n
           | <term> inter <term>                 \n
           | compl <term>                        \n
                                                       \n
   <bv> ::= constbv                                    \n
          | conc [ const , const ] ( <term> , <term> ) \n     
          | bw_and [ const ] ( <term> , <term> )       \n
          | bw_or [ const ] ( <term> , <term> )        \n
          | bw_xor [ const ] ( <term> , <term> )       \n
          | <term> [ const , const , const ]           \n
                                   \n
   <prop> ::= true                 \n
            | false                \n
            | <atom>               \n
            | <term> && <term>     \n
            | <term> || <term>     \n
            | <term> => <term>     \n
            | <term> <=> <term>    \n
            | ~ <term>             \n
                                   \n
   <atom> ::= <cnstrnt> ( <term> ) \n
            | <term> = <term>      \n  
            | <term> <> <term>     \n
            | <term> < <term>      \n
            | <term> <= <term>     \n
            | <term> > <term>      \n
            | <term> >= <term>     \n
            | <term> in <term>     \n
            | <term> notin <term>  \n
                                   \n
   <cnstrnt> ::= int | real | pos | neg | nonpos | nonneg   \n"
