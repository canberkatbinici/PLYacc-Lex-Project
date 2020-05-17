# ProgrammingLanguage

CSE PL Homework

# Programming Language “LEAF”

Yaprak Bulut 20160808005		--	git -> ”yaprakbulut” 

Canberk Atbinici 20160808016	--	git -> “canberkatbinici” 



Our programming language named by "leaf" has name come from group members names. Leaf is in Turkish "Yaprak".

This is initially done for Programming Language homework in Akdeniz University Computer Science Engineering program.

Our language is designed as  syntax of the algorithms in CAPSLOCK form. Our language is mainly under effect of Java language syntax.

 # Syntax

# Blocks and Commands


< program >  ::= < stmt_list >


< stmt_list >  ::= < stmt >
                 
                 | < stmt > ; < stmt_list >
                  
                  
< stmt >       ::=   < if-statement > 
                        
                        | < while-statement >
                        
                        | < assignment-statement >
                        
                        | < switch statement > 
                        
                        | < do statement >
                        
                        | < break statement >
                        
                        | < continue statement >
                        
                        | < return statement > 
                        
                        | < try statement >
                        
                        | < null-statement > 
                         
                         
< if statement >::= IF ( < condition > ) THEN < statement > 
                        
                        | IF ( < condition > ) THEN < statement > ELSE < statement >
                        
                        | IF ( < condition > ) THEN < statement >< elseif > ELSE < statement >
                               
                               
                               
< elseif statement > ::= ELF ( < expression > ) < statement > 
                        
                        | < elseif statement >
                                     
                                     
< while statement > ::= WHILE ( < expression > ) < statement > LOOPW


< switch statement > ::= SWITCH ( < expression > ) < switch labels >


< switch labels > ::= < switch label > | < switch labels > < switch label >


< switch label > ::= CASE < constant expression > : | DEFAULT :


< do statement > ::= DO < statement > WHILE ( < expression > ) LOOPW ;


< break statement > ::= STOP < identifier >? ;


< continue statement > ::= CONTINUE < identifier >? ;


< return statement > ::= RETURN < expression >? ;


< try statement > ::= try < block > < catches > | try < block > < catches >? < finally >


< catches > ::= < catch clause > | < catches > < catch clause >


< catch clause > ::= catch ( < formal parameter > ) < block >


< null-statement >   ::= ε


< assignment-statement > ::= < variable > = < expression > ;


# Expressions


< expression > ::= < assignment expression > 
                  
                  | < relational expression >
                  
                  | < logical expression >


< relational expression > ::= < expression > + [ = | < | <= | =< | > | >= | => |  != | => ]+ + < expression >


< assignment-expression > ::= < conditional-expression >
                               
                               | < left hand side > < assignment-operator > < assignment-expression >


< assignment-operator > ::= *
                        
                        | /
                        
                        | %
                        
                        | +
                        
                        | -
                        
                        | ^ 
                        
                        | >+
                        
                        | >-
                        
                        
< logical expression > ::= < expression > < logical operator > < expression >


< logical operator > ::=  && 
                      
                      | ><


# Tokens


< letter > ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"


< digits > ::= < digit > | < digits > < digit >


< signed integer > ::= < sign >? < digits >


< sign > ::= + | -


< digit > ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"


< boolean literal > ::= TRUE | FALSE


< keyword > ::= WHITESPACE | TRUE | FALSE | BOOL | FUNCTION | ENDF | RETURN | IF | ELSE | ELF | DO | WHILE | LOOPW | FOR | LOOPF | TRY | CATCH | THEN | SWITCH | CASE | IMPORT | STOP | CONTINUE | PRINTLOG | SCANLOG | NL | STR | INT | DINT | FLOAT |  




# How to run ?

Fist, to build project run "make" command.

Second, to run project run "make run" command. 

Third, to clear builds run "make clear" command.
