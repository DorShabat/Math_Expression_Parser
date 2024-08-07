# save 02/08/2024 10:30
# evaluation working
import numpy as np
from lr_parsing_table import get_parsing_table3

import graphviz
import sympy as sp

def tree_to_dot(node, dot=None):
    if dot is None:
        dot = graphviz.Digraph()

    node_id = str(id(node))
    label = str(node.value)
    dot.node(node_id, label)

    if node.left:
        left_id = str(id(node.left))
        dot.edge(node_id, left_id)
        tree_to_dot(node.left, dot)

    if node.right:
        right_id = str(id(node.right))
        dot.edge(node_id, right_id)
        tree_to_dot(node.right, dot)

    return dot


def create_tree_png(node, filename):
    dot = tree_to_dot(node)
    dot.render(filename, format='png', cleanup=True)

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
                return Node('-',None,self.right.differentiate())
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
                             Node('*', Node('ln', None,base), exponent_diff)))
        elif self.value == 'sin':
            return Node('*', Node('cos',None, self.right), self.right.differentiate())
        elif self.value == 'cos':
            return Node('*', Node('*', Node(-1), Node('sin',None, self.right)), self.right.differentiate())
        elif self.value == 'tg':
            return Node('*', Node('/', Node(1), Node('^', Node('cos',None, self.right), Node(2))), self.right.differentiate())
        elif self.value == 'arcsin':
            return Node('*',Node('/', Node(1), Node('^', Node('-', Node(1), Node('^', self.right, Node(2))), Node(0.5))),self.right.differentiate())
        elif self.value == 'arccos':
            return Node('*', Node(-1), Node('*', Node('/', Node(1),Node('^', Node('-', Node(1), Node('^', self.right, Node(2))),Node(0.5))), self.right.differentiate()))
        elif self.value == 'arctg':
            return Node('*', Node('/', Node(1), Node('+', Node(1), Node('^', self.right, Node(2)))), self.right.differentiate())
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

    def parse(self, tokens):
        stack = [0]
        index = 0
        token = tokens[index]
        value_stack = []

        while True:
            state = int(stack[-1])
            token_type = token[1]
            if token[0] == "NUMBER":
                token_type = token[0]

            if token_type in self.parsing_table[str(state)]:
                action = self.parsing_table[str(state)][token_type]

                if action.startswith('s'):
                    # Shift
                    next_state = int(action[1:])
                    stack.append(next_state)
                    if token[0] == 'VAR':
                        value_stack.append(Node('X'))
                    else:
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

                    values = []
                    for _ in rhs_symbols:
                        stack.pop()
                        values.append(value_stack.pop())

                    values.reverse()
                    result = self.build_tree(lhs, values)

                    state = stack[-1]
                    stack.append(self.parsing_table[str(state)][lhs])
                    value_stack.append(result)
                elif action == 'acc':
                    # Accept
                    return value_stack[0]
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
                return Node('-',None,values[1])
            elif values[0].value == '(':
                return values[1] #####################
            elif len(values) == 4:
                func = values[0].value
                arg = values[2]
                return Node(func,None, arg)
        return values[0]





def tree_to_string(node):
    if node.left is None and node.right is None:
        return str(node.value)
    left_str = tree_to_string(node.left) if node.left else ""
    right_str = tree_to_string(node.right) if node.right else ""
    if node.value in ["cos","sin","ln","tg","arcsin","arccos","arctg","exp"]:
        return f"{left_str} {node.value} ({right_str})"
    return f"({left_str} {node.value} {right_str})"

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

def evaluate(node, X):
    if node.value == 'X':
        return X
    elif isinstance(node.value, (int, float)):
        return node.value

    left_val = evaluate(node.left, X) if node.left else None
    right_val = evaluate(node.right, X) if node.right else None

    if node.value == '+':
        return left_val + right_val
    elif node.value == '-':
        return left_val - right_val if left_val is not None else -right_val
    elif node.value == '*':
        return left_val * right_val
    elif node.value == '/':
        return left_val / right_val
    elif node.value == '^':
        return left_val ** right_val
    elif node.value == 'sin':
        return np.sin(right_val)
    elif node.value == 'cos':
        return np.cos(right_val)
    elif node.value == 'tg':
        return np.tan(right_val)
    elif node.value == 'arcsin':
        return np.arcsin(right_val)
    elif node.value == 'arccos':
        return np.arccos(right_val)
    elif node.value == 'arctg':
        return np.arctan(right_val)
    elif node.value == 'exp':
        return np.exp(right_val)
    elif node.value == 'ln':
        return np.log(right_val)
    else:
        raise ValueError(f"Unsupported operation: {node.value}")

# Adding this function into your provided script



def main():
    parsing_table = get_parsing_table3()

    expressions = [
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

    #expressions = ["2*X*2+6","X^2+X*2+6","sin(X)"]
    x = sp.symbols('X')

    X = 2
    parser = LRParser(parsing_table, X)
    for expression in expressions:

        tokens = tokenize(expression)
        try:
            op_tree = parser.parse(tokens)
            # create_tree_png(op_tree, f"tree/{i}_op_tree")

            op_tree_str = tree_to_string(op_tree)
            print(f"op_tree_str: {op_tree_str}")
            op_tree_eval = evaluate(op_tree,X)
            print(f"op_tree_eval: {op_tree_eval}")

            differentiated_tree = op_tree.differentiate()
            # differentiated_tree_simplify_str = simplify(differentiated_tree)

            # create_tree_png(differentiated_tree, f"tree/{i}_parsed_expr")
            differentiated_tree_str = tree_to_string(differentiated_tree)
            print(f"differentiated_tree_str: {differentiated_tree_str}")

            derivative_us = sp.sympify(differentiated_tree_str.replace("^", "**").replace("tg", "tan").replace("arc", "a"))
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
