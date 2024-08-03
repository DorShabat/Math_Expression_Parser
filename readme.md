# Save Log
1. grammer.txt = last grammer we working with.
2. derivative.py - working example of derivative.
3. LRParser.py - working example of evaluation.
4. lr_table_Create.py - code to create lr table from excel table.
5. lr_table.xlsx - the lr table - last table we are working with it table3. 
6. LRParser_eval_and_derivative.py - initial start of combining evaluation and derivative.
7. LRParser_evaluateMathematicalExpression.py - try of derivative in other method (not in tree - not working).
8. main.py - temp file for testing.

# Grammar
in grammer.txt

```
E  -> E + T
    | E - T
    | T

T  -> T * F
    | T / F
    | F

F  -> G ^ F
    | G

G  -> ( E )
    | - G
    | Func ( E )
    | NUMBER
	| X

Func -> sin
     | cos
     | tg
     | arcsin
     | arccos
     | arctg
     | exp
     | ln

```

# Link for lr parser creation
https://cyberzhg.github.io/toolbox/

# Useful promts
```
I'm working on a Python project that does: an evaluation of mathematical expressions. 
it gets the expression as a string, and the value of X when X exists in the expression.
the way the evaluation works is by using the LR parser I built using the following grammar:
E  -> E + T
    | E - T
    | T

T  -> T * F
    | T / F
    | F

F  -> G ^ F
    | G

G  -> ( E )
    | - G
    | Func ( E )
    | NUMBER
	| X

Func -> sin
     | cos
     | tg
     | arcsin
     | arccos
     | arctg
     | exp
     | ln

read the code and tell me when you are ready for some further instructions for helping me.
```
# Derivative
derivative