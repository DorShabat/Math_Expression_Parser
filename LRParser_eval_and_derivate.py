# save 02/08/2024 10:30
# evaluation working
import numpy as np
from lr_parsing_table import get_parsing_table3

EVAL = 'eval'
DERIVATIVE = 'derivative'


def tree_to_string(node):
    if node.left is None and node.right is None:
        return str(node.value)
    left_str = tree_to_string(node.left) if node.left else ""
    right_str = tree_to_string(node.right) if node.right else ""
    if node.value in ["cos", "sin", "ln", "tg", "arcsin", "arccos", "arctg", "exp"]:
        return f"{left_str} {node.value} ({right_str})"
    return f"({left_str} {node.value} {right_str})"


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
            # print()
            # print(f"states_stack==states = {states_stack}")
            # print(f"value_stack==valuesToReduce = {value_stack}")
            # print(f"str(state): {str(state)},tableColNames: {tableColNames}")
            if tableColNames in self.parsing_table[str(state)]:
                action = self.parsing_table[str(state)][tableColNames]
                # print(f"action:{action}")

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
                    # print(f"self.parsing_table[str({state})][{lhs}]: {self.parsing_table[str(state)][lhs]}")
                    states_stack.append(self.parsing_table[str(state)][lhs])
                    value_stack.append(result)
                elif action == 'acc':
                    # Accept
                    if mode == EVAL:
                        return value_stack[0]
                    else:  # mode == DERIVATE
                        return tree_to_string(value_stack[0].differentiate())

                else:
                    raise RuntimeError(f'Unknown action: {action}')
            else:
                raise RuntimeError(f'Unexpected token: {token}')

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
                if isinstance(values[0].value, (int, float)) and isinstance(values[2].value, (int, float)):
                    return Node(values[0].value * values[2].value)
                return Node('*', values[0], values[2])
            elif values[1].value == '/':
                if isinstance(values[0].value, (int, float)) and isinstance(values[2].value, (int, float)):
                    return Node(values[0].value / values[2].value)
                return Node('/', values[0], values[2])
        elif lhs == 'F':
            if len(values) == 1:
                return values[0]
            elif values[1].value == '^':
                if isinstance(values[0].value, (int, float)) and isinstance(values[2].value, (int, float)):
                    return Node(values[0].value ** values[2].value)
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


def main():
    parsing_table = get_parsing_table3()

    expressions1 = [
        "X+6",
        "sin(X)",
        "3 + 4",
        "(3 + 4)",
        "(2 * (3 + 4))",
        "(5 - 3)",
        "((2 + 3) * (4 - 1))",
        "-6",
        "sin(X)",
        "X^3",
        "cos(X)",
        "tg(X)",
        "arcsin(X)",
        "arccos(X)",
        "arctg(X)",
        "exp(X)",
        "ln(X)",
        "sin(2*X)",
        "cos(2*X)",
        "sin(2*cos(2*X)^(2*X))",
        "-6",
        "X",
        "-X",
        "8",
        "X + 8",
        "3*X",
        "X^2",
        "X^2 + 3*X + 1",
        "X^3",
        "X^2 * X + 3",
        "(X + 1)^2",
        "(2*X + 1)^2",
        "X^2 / X^2",
    ]
    '''
    value at x = 1: -10968.1737085657 + 38116.0419579044*I
    '''
    expressions = [
        "3+4",
        "X^3 + sin(X) - 3.14",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)",  # 103.265
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9",  # 50.1501
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(81^(4*X))-(cos(X-6)/5*X)",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(81^(4*X))-(cos(X-6)/5*X)",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(81^(4*X))-(cos(X-6)/5*X)+X^X",
        # 53.7063
        "X^X^X",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X-2*X*17*sin(8*X-13)",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)",
        "X^7+(X+11*2*X/4)+sin(X*2)+cos(sin(X))-(ln(81*X))*exp(2)+arctg(X/7)*(11-2*(X^3))-63*exp(X)/9+tg(18^(4*X))-(cos(X-6)/5*X)+X^X-X^X^X+X^2^X-X^X^X^X-2*X*17*sin(8*X-13)+87654",
        # "arcsin((76/(4^11+80*9))*X)+546-38*X/11*(X-4)+ln(8*X/11)-arccos((93/7654326543)*X)",
        "cos(11*(X^2-3*X^3+X+81))/X-41+X^5-287*48/X+tg(59*X-198)",
        "sin(80)",
    ]

    # expressions[0].replace("**", "^").replace("tan", "tg").replace("asin", "arcsin").replace("acos", "arccos").replace("atan", "arctg").replace("log", "ln").replace("pi", "3.141592653589793")
    # X = -1.9286689276512163
    X = 2
    parser = LRParser(parsing_table, X)
    for expression in expressions:
        try:
            # print(f"sp: {sp.sympify(expression).evalf(subs={x: X})}")

            expression = expression.replace("**", "^").replace("tan", "tg").replace("asin", "arcsin").replace("acos",
                                                                                                              "arccos").replace(
                "atg", "arctg").replace("log", "ln").replace("pi", "3.141592653589793")
            print(f"len of expression: {len(expression)}")

            tokens = tokenize(expression)
            # Evaluate
            print(f"f(x) = {expression}")
            evaluate_expression = parser.parse(tokens, EVAL)
            print(f"Eval(f({X})) = {evaluate_expression:.4f}".rstrip('0').rstrip('.'))

            differentiated = parser.parse(tokens, DERIVATIVE)
            print(f"Diff(f(x)) = {differentiated}")

            tokens_for_differentiated = tokenize(differentiated)
            evaluate_differentiated = parser.parse(tokens_for_differentiated, EVAL)
            print(f"Diff(f({X})) = {evaluate_differentiated:.4f}".rstrip('0').rstrip('.'))

            ###################################

            import sympy as sp
            x = sp.symbols('X')

            print("########################")
            derivative_us = sp.sympify(differentiated.replace("^", "**").replace("tg", "tan").replace("arc", "a"))
            print(f"derivative_us: {derivative_us}")
            expression_sp = sp.sympify(expression.replace("^", "**").replace("tg", "tan").replace("arc", "a"))
            derivative_sp = sp.diff(expression_sp, x)
            print(f"derivative_sp: {derivative_sp}")

            print(f"Are they equivalent? {derivative_us.subs(x, 2).evalf(4) == derivative_sp.subs(x, 2).evalf(4)}")

            print()
        except RuntimeError as e:
            print(f"Error parsing expression '{expression}': {e}")


if __name__ == "__main__":
    main()
