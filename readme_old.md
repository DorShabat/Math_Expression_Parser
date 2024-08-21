# Save Log
1. grammer.txt = last grammer we working with.
2. derivative.py - working example of derivative.
3. LRParser.py - working example of evaluation.
4. lr_table_Create.py - code to create lr table from excel table.
5. lr_table.xlsx - the lr table - last table we are working with it table3. 
6. LRParser_eval_and_derivative.py - combining evaluation and derivative. - working! - evauation using lr parser table and derivative using operation tree.
7. main.py - temp file for testing.
8. lr_parsing_table.py - hard coded lr table.
9. tree_vizualize_functions.py - vizualize tree functions.
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

# all the operation for promts
```
+ , - , * , / , ^ , sin , cos , tg , arcsin , arccos , arctg , exp , ln
```

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

```
I'm working on a project that does: an evaluation of mathematical expressions and derivative of the expression.
it gets the expression as a string, and the value of X when X exists in the expression, 
in the main i evaluate the expression using X and then I calculate the derivative of the expression.
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

it evaluates the expression "on the fly".
to calculate the derivative I build an operation tree and then I calculate the derivative using the operation tree.

the operations that are supported are: + , - , * , / , ^ , sin , cos , tg , arcsin , arccos , arctg , exp , ln.

read the code and tell me when you are ready for some further instructions for helping me.
```
# Derivative
derivative

# imports we can use in husklle
```
import Data.Typeable
import Debug.Trace
import Prelude hiding
import Prelude.Unicode
import Data.List
import Data.Set
import Data.Map
import Data.Sequence
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Array.Unboxed
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel
import Numeric.LinearAlgebra.HMatrix
import Numeric.LinearAlgebra.Static
import Data.Time.Clock
```

# how to get the expression nice
1. copy the expression to mini gpt and ask for latex format in one line.
it important to do it in mini gpt.
```
can you make me the latex of the expression in one line?
```
2. enter this site https://latexeditor.lagrida.com/ (or any other site that can convert latex to math notation)
3. paste the latex format expression.
4. we can download the image, maybe crop it, maybe use it as it is.

**Note:**
for the expression: 
```
X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)+20*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)-(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)/10+(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)^2+2*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)*(X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654)
  
```
the latex format mini gpt gave me is:
```latex
X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654 + 2 \cdot \left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right) + 20 \cdot \left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right) - \frac{\left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right)}{10} + \left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right)^2 + 2 \cdot \left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right) \cdot \left(X^7 + \left(X + \frac{11 \cdot 2 \cdot X}{4}\right) + \sin(2X) + \cos(\sin(X)) - (\ln(81X)) \cdot \exp(2) + \arctan\left(\frac{X}{7}\right) \cdot \left(11 - 2 \cdot X^3\right) - \frac{63 \cdot \exp(X)}{9} + \tan\left(18^{4X}\right) - \frac{\cos(X - 6)}{5} \cdot X + X^X - X^{X^X} + X^{2^X} - X^{X^{X^X}} - 2 \cdot X \cdot 17 \cdot \sin(8X - 13) + 87654\right)

```