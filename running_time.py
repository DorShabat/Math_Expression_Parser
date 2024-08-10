import sympy as sp
import time


def evaluate_expression_traditionally(expression, x_value):
    # Create a dictionary with X set to the given value
    local_vars = {"X": x_value}

    # Start timing
    start_time = time.time()

    # Evaluate the expression using eval and the dictionary of local variables
    result = eval(expression, {}, local_vars)

    # End timing
    end_time = time.time()

    # Calculate the execution time
    execution_time = end_time - start_time

    return result, execution_time


def differentiate_expression_traditionally(expression):
    # Define the symbol X
    X = sp.symbols('X')

    # Parse the expression into a SymPy expression
    sympy_expr = sp.sympify(expression)

    # Start timing
    start_time = time.time()

    # Differentiate the expression with respect to X
    derivative = sp.diff(sympy_expr, X)

    # End timing
    end_time = time.time()

    # Calculate the execution time
    execution_time = end_time - start_time

    return derivative, execution_time


def main():
    # Example expression
    # expression = "X**2 + 5"

    # Example value for X
    x_value = 2

    # Evaluate the expression
    result, eval_time = evaluate_expression_traditionally(expression, x_value)
    print(f"The result of the expression '{expression}' with X={x_value} is: {result}")
    print(f"Evaluation time: {eval_time:.8f} seconds")

    # Differentiate the expression
    derivative, diff_time = differentiate_expression_traditionally(expression)
    print(f"The derivative of the expression '{expression}' with respect to X is: {derivative}")
    print(f"Differentiation time: {diff_time:.8f} seconds")


# Run the main function
if __name__ == "__main__":
    main()
