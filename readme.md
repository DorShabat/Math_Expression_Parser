
# Mathematical Expression Evaluation and Differentiation Using LR Parser

This project implements a system for evaluating mathematical expressions and computing their derivatives. It leverages a custom-built LR parser to process expressions, and it supports a wide range of mathematical operations, including basic arithmetic, trigonometric functions, exponentiation, and logarithms.

## Features

- **Evaluation**: The program evaluates mathematical expressions given as strings, including support for variables (e.g., `X`).
- **Differentiation**: It computes the derivative of the expression by constructing and traversing an operation tree.
- **Python and Haskell Implementations**: The project is implemented in both Python and Haskell, demonstrating flexibility across programming languages.
- **Tree Visualization**: In the Python version, the operation tree used for differentiation can be visualized using Graphviz.

## Grammar

The LR parser is built using the following context-free grammar:

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

This grammar allows the parser to recognize and process a wide variety of mathematical expressions.

## Usage

### Python

1. **Evaluation and Differentiation:**
   - The main functionality is encapsulated in the `Eval and Diff using LR Parser.py` file.
   - The script evaluates the expression and computes its derivative, printing the results to the console.

2. **Tree Visualization:**
   - The `tree_vizualize_functions.py` file contains functions to convert the operation tree into a `.png` file using Graphviz.

3. **Running the Program:**
   - Run the main Python script using:
     ```
     python3 Eval\ and\ Diff\ using\ LR\ Parser.py
     ```

### Haskell

- The Haskell implementation is available in the `Eval_and_Diff_using_LR_Parser.hs` file.
- It provides similar functionality for evaluating expressions and computing derivatives.

## Example

Given the expression:
```
X^7 + sin(X) - ln(X^2)
```

The program will:
1. Evaluate the expression for a given value of `X`.
2. Compute and display the derivative of the expression.
3. (Python) Visualize the operation tree for the expression.

## Comparison with Existing Solutions

The Python implementation includes a comparison against traditional evaluation and differentiation methods (e.g., using Python's `eval` and `SymPy`).


## Acknowledgments

- [Graphviz](https://graphviz.org/) for tree visualization.
- [SymPy](https://www.sympy.org/en/index.html) for symbolic computation in Python.
