import numpy as np
from lr_parsing_table import get_parsing_table

EVAL = 'eval'
DERIVATIVE = 'derivative'

def tokenize(expression):
    tokens = []
    i = 0
    while i < len(expression):
        char = expression[i]

        if char.isdigit() or (char == '.' and i + 1 < len(expression) and expression[i + 1].isdigit()):
            # Recognize numbers (integer and floating-point)
            num = char
            i += 1
            while i < len(expression) and (expression[i].isdigit() or expression[i] == '.'):
                num += expression[i]
                i += 1
            tokens.append(('NUMBER', float(num) if '.' in num else int(num)))

        elif char == "X":
            tokens.append(('VAR', char))
            i += 1

        elif char.isalpha():
            # Recognize functions
            func = char
            i += 1
            while i < len(expression) and expression[i].isalpha():
                func += expression[i]
                i += 1
            tokens.append(('FUNCTION', func))
        elif char in '+-*/^()':
            # Recognize operators and parentheses
            tokens.append(('OP', char))
            i += 1
        elif char in ' \t':
            # Skip whitespace
            i += 1
        else:
            raise RuntimeError(f'Unexpected character: {char}')

    tokens.append(('$', '$'))
    return tokens

class LRParser:
    def __init__(self, parsing_table, X):
        self.parsing_table = parsing_table
        self.X = X

    def parse(self, tokens, mode):
        states_stack = [0]
        index = 0
        token = tokens[index]
        value_stack = []

        while True:
            state = int(states_stack[-1])
            tableColNames = token[1]
            if token[0] == "NUMBER":
                tableColNames = token[0]
            if tableColNames in self.parsing_table[str(state)]:
                action = self.parsing_table[str(state)][tableColNames]
                if action.startswith('s'):
                    # Shift
                    next_state = int(action[1:])
                    states_stack.append(next_state)
                    if token[0] == 'VAR':
                        if mode == EVAL:
                            value_stack.append(self.X)
                        elif mode == DERIVATIVE:
                            value_stack.append(Node('X'))
                    else:
                        if mode == EVAL:
                            value_stack.append(token[1])
                        elif mode == DERIVATIVE:
                            value_stack.append(Node(token[1]))
                    index += 1
                    token = tokens[index]
                elif action.startswith('r'):
                    # Reduce
                    production = action[1:].strip()
                    if production.startswith('(') and production.endswith(')'):
                        production = production[1:-1].strip()
                    lhs, rhs = production.split(' -> ')
                    rhs_symbols = rhs.split()

                    valuesToReduce = []
                    for _ in rhs_symbols:
                        states_stack.pop()
                        valuesToReduce.append(value_stack.pop())

                    valuesToReduce.reverse()
                    if mode == EVAL:
                        result = self.evaluate(lhs, valuesToReduce)
                    else:  # mode == DERIVATE
                        result = self.build_tree(lhs, valuesToReduce)

                    state = states_stack[-1]
                    states_stack.append(self.parsing_table[str(state)][lhs])
                    value_stack.append(result)
                elif action == 'acc':
                    # Accept
                    if mode == EVAL:
                        return value_stack[0]
                    else:  # mode == DERIVATE
                        return value_stack[0].differentiate().tree_to_string()

                else:
                    raise RuntimeError(f'Unknown action: {action}')
            else:
                raise RuntimeError(f'Unexpected token: {token}')

    def evaluate(self, lhs, values):
        if lhs == 'E':
            if len(values) == 1:
                return values[0]
            elif values[1] == '+':
                return values[0] + values[2]
            elif values[1] == '-':
                return values[0] - values[2]
        elif lhs == 'T':
            if len(values) == 1:
                return values[0]
            elif values[1] == '*':
                return values[0] * values[2]
            elif values[1] == '/':
                return values[0] / values[2]
        elif lhs == 'F':
            if len(values) == 1:
                return values[0]
            elif values[1] == '^':
                return values[0] ** values[2]
        elif lhs == 'G':
            if len(values) == 1:
                return values[0]
            elif len(values) == 2:
                return -values[1]
            elif values[0] == '(':
                return values[1]
            elif len(values) == 4:
                func = values[0]
                arg = values[2]
                if func == 'sin':
                    return np.sin(arg)
                elif func == 'cos':
                    return np.cos(arg)
                elif func == 'tg':
                    return np.tan(arg)
                elif func == 'arcsin':
                    return np.arcsin(arg)
                elif func == 'arccos':
                    return np.arccos(arg)
                elif func == 'arctg':
                    return np.arctan(arg)
                elif func == 'exp':
                    return np.exp(arg)
                elif func == 'ln':
                    return np.log(arg)
        return values[0]

    def build_tree(self, lhs, values):
        if lhs == 'E':
            if len(values) == 1:
                return values[0]
            elif values[1].value == '+':
                return Node('+', values[0], values[2])
            elif values[1].value == '-':
                return Node('-', values[0], values[2])
        elif lhs == 'T':
            if len(values) == 1:
                return values[0]
            elif values[1].value == '*':
                return Node('*', values[0], values[2])
            elif values[1].value == '/':
                return Node('/', values[0], values[2])
        elif lhs == 'F':
            if len(values) == 1:
                return values[0]
            elif values[1].value == '^':
                return Node('^', values[0], values[2])
        elif lhs == 'G':
            if len(values) == 1:
                return values[0]
            elif len(values) == 2:
                return Node('-', None, values[1])
            elif values[0].value == '(':
                return values[1]
            elif len(values) == 4:
                func = values[0].value
                arg = values[2]
                return Node(func, None, arg)
        return values[0]

class Node:
    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def __str__(self):
        if self.left is None and self.right is None:
            return str(self.value)
        return f"({self.left} {self.value} {self.right})"

    def differentiate(self):
        if self.value == 'X':
            return Node(1)
        elif isinstance(self.value, int) or isinstance(self.value, float):
            return Node(0)

        if self.value == '+':
            return Node('+', self.left.differentiate(), self.right.differentiate())
        elif self.value == '-':
            if self.left is None:
                return Node('-', None, self.right.differentiate())
            return Node('-', self.left.differentiate(), self.right.differentiate())
        elif self.value == '*':
            return Node('+', Node('*', self.left.differentiate(), self.right),
                        Node('*', self.left, self.right.differentiate()))
        elif self.value == '/':
            return Node('/', Node('-', Node('*', self.left.differentiate(), self.right),
                                  Node('*', self.left, self.right.differentiate())),
                        Node('^', self.right, Node(2)))
        elif self.value == '^':
            base, exponent = self.left, self.right
            base_diff = base.differentiate()
            exponent_diff = exponent.differentiate()
            return Node('*', Node('^', base, exponent),
                        Node('+', Node('*', exponent, Node('/', base_diff, base)),
                             Node('*', Node('ln', None, base), exponent_diff)))
        elif self.value == 'sin':
            return Node('*', Node('cos', None, self.right), self.right.differentiate())
        elif self.value == 'cos':
            return Node('*', Node('*', Node(-1), Node('sin', None, self.right)), self.right.differentiate())
        elif self.value == 'tg':
            return Node('*', Node('/', Node(1), Node('^', Node('cos', None, self.right), Node(2))),
                        self.right.differentiate())
        elif self.value == 'arcsin':
            return Node('*',
                        Node('/', Node(1), Node('^', Node('-', Node(1), Node('^', self.right, Node(2))), Node(0.5))),
                        self.right.differentiate())
        elif self.value == 'arccos':
            return Node('*', Node(-1), Node('*', Node('/', Node(1),
                                                      Node('^', Node('-', Node(1), Node('^', self.right, Node(2))),
                                                           Node(0.5))), self.right.differentiate()))
        elif self.value == 'arctg':
            return Node('*', Node('/', Node(1), Node('+', Node(1), Node('^', self.right, Node(2)))),
                        self.right.differentiate())
        elif self.value == 'exp':
            return Node('*', Node('exp', None, self.right), self.right.differentiate())
        elif self.value == 'ln':
            return Node('*', Node('/', Node(1), self.right), self.right.differentiate())
        else:
            raise ValueError(f"Unsupported operation: {self.value}")

    def tree_to_string(self):
        if self.left is None and self.right is None:
            return str(self.value)
        left_str = self.left.tree_to_string() if self.left else ""
        right_str = self.right.tree_to_string() if self.right else ""
        if self.value in ["cos", "sin", "ln", "tg", "arcsin", "arccos", "arctg", "exp"]:
            return f"{left_str} {self.value} ({right_str})"
        return f"({left_str} {self.value} {right_str})"


def evaluate_expression_traditionally(expression, x_value): # for texting functionality
    import math
    import time
    # Define local variables to be used in eval
    local_vars = {
        "X": x_value,
        "math": math
    }

    # Replace operations with math equivalents
    expression = expression.replace("arcsin", "math.asin").replace("arccos", "math.acos").replace("arctg", "math.atan").replace("sin", "math.sin").replace("cos", "math.cos").replace("tg", "math.tan").replace("exp", "math.exp").replace("ln", "math.log").replace("^", "**")

    # Start timing
    start_time = time.time()

    # Evaluate the expression using eval and the dictionary of local variables
    result = eval(expression, {}, local_vars)

    # End timing
    end_time = time.time()

    # Calculate the execution time
    execution_time = end_time - start_time

    return result, execution_time


def differentiate_expression_traditionally(expression): # for texting functionality
    import time
    import sympy as sp
    # Define the symbol X
    X = sp.symbols('X')

    # Parse the expression into a SymPy expression
    sympy_expr = sp.sympify(expression.replace("^", "**").replace("tg", "tan").replace("arc", "a"))

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
    import time # for testing
    parsing_table = get_parsing_table()
    x_value = 2

    expression = "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654"

    try:
        print(f"\nf(X) = {expression}")
        print(f"len of expression f(X): {len(expression)}\n")

        # tokenize and create parser
        start_time = time.time()
        tokens = tokenize(expression)
        parser = LRParser(parsing_table, x_value)
        end_time = time.time()
        print(f"Tokenizer time: {(end_time - start_time):.8f} seconds.\n")

        # Evaluate f(x)
        start_time = time.time()
        evaluate_expression = parser.parse(tokens, EVAL)
        print(f"Eval(f({x_value})) = {evaluate_expression:.10f}".rstrip('0').rstrip('.'))
        end_time = time.time()
        print(f"Evaluation time: {(end_time - start_time):.8f} seconds.\n")

        # Derivative
        start_time = time.time()
        differentiated = parser.parse(tokens, DERIVATIVE)
        print(f"Diff(f(x)) = {differentiated}")
        end_time = time.time()
        print(f"Differentiate time: {(end_time - start_time):.8f} seconds.\n")

        # tokenize and eval the derivative
        tokens_for_differentiated = tokenize(differentiated)
        evaluate_differentiated = parser.parse(tokens_for_differentiated, EVAL)
        print(f"Diff(f({x_value})) = {evaluate_differentiated:.10f}".rstrip('0').rstrip('.'))


        # Testing Results:
        print("\n -- Testing the result compare to Python's Eval function and Sumpy's Diff ans Subs --")
        result, eval_time = evaluate_expression_traditionally(expression, x_value)
        print(f"Python's Eval time: {eval_time:.8f} seconds and the result is: {result}")

        derivative, diff_time = differentiate_expression_traditionally(expression)
        print(f"Sumpy's differentiation time: {diff_time:.8f} seconds.")

        import sympy as sp
        x = sp.symbols('X')
        result = derivative.subs(x, 2).evalf()
        print(f"Python's Eval of f'(X) is: {result}")


    except RuntimeError as e:
        print(f"Error parsing expression '{expression}': {e}")


if __name__ == "__main__":
    main()
