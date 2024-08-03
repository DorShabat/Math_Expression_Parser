import graphviz
import numpy as np
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

def get_parsing_table3():
    parsing_table = {
        '0': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
              'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'E': 1, 'T': 2, 'F': 3, 'G': 4,
              'Func': 7}, '1': {'+': 's18', '-': 's19', '$': 'acc'},
        '2': {')': 'r( E -> T )', '*': 's20', '+': 'r( E -> T )', '-': 'r( E -> T )', '/': 's21', '$': 'r( E -> T )'},
        '3': {')': 'r( T -> F )', '*': 'r( T -> F )', '+': 'r( T -> F )', '-': 'r( T -> F )', '/': 'r( T -> F )',
              '$': 'r( T -> F )'},
        '4': {')': 'r( F -> G )', '*': 'r( F -> G )', '+': 'r( F -> G )', '-': 'r( F -> G )', '/': 'r( F -> G )',
              '^': 's22', '$': 'r( F -> G )'},
        '5': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
              'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'E': 23, 'T': 2, 'F': 3, 'G': 4,
              'Func': 7},
        '6': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
              'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'G': 24, 'Func': 7},
        '7': {'(': 's25'},
        '8': {')': 'r( G -> NUMBER )', '*': 'r( G -> NUMBER )', '+': 'r( G -> NUMBER )', '-': 'r( G -> NUMBER )',
              '/': 'r( G -> NUMBER )', '^': 'r( G -> NUMBER )', '$': 'r( G -> NUMBER )'},
        '9': {')': 'r( G -> X )', '*': 'r( G -> X )', '+': 'r( G -> X )', '-': 'r( G -> X )', '/': 'r( G -> X )',
              '^': 'r( G -> X )', '$': 'r( G -> X )'}, '10': {'(': 'r( Func -> sin )'}, '11': {'(': 'r( Func -> cos )'},
        '12': {'(': 'r( Func -> tg )'}, '13': {'(': 'r( Func -> arcsin )'}, '14': {'(': 'r( Func -> arccos )'},
        '15': {'(': 'r( Func -> arctg )'}, '16': {'(': 'r( Func -> exp )'}, '17': {'(': 'r( Func -> ln )'},
        '18': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'T': 26, 'F': 3, 'G': 4, 'Func': 7},
        '19': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'T': 27, 'F': 3, 'G': 4, 'Func': 7},
        '20': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'F': 28, 'G': 4, 'Func': 7},
        '21': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'F': 29, 'G': 4, 'Func': 7},
        '22': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'F': 30, 'G': 4, 'Func': 7},
        '23': {')': 's31', '+': 's18', '-': 's19'},
        '24': {')': 'r( G -> - G )', '*': 'r( G -> - G )', '+': 'r( G -> - G )', '-': 'r( G -> - G )',
               '/': 'r( G -> - G )', '^': 'r( G -> - G )', '$': 'r( G -> - G )'},
        '25': {'(': 's5', '-': 's6', 'NUMBER': 's8', 'X': 's9', 'arccos': 's14', 'arcsin': 's13', 'arctg': 's15',
               'cos': 's11', 'exp': 's16', 'ln': 's17', 'sin': 's10', 'tg': 's12', 'E': 32, 'T': 2, 'F': 3, 'G': 4,
               'Func': 7},
        '26': {')': 'r( E -> E + T )', '*': 's20', '+': 'r( E -> E + T )', '-': 'r( E -> E + T )', '/': 's21',
               '$': 'r( E -> E + T )'},
        '27': {')': 'r( E -> E - T )', '*': 's20', '+': 'r( E -> E - T )', '-': 'r( E -> E - T )', '/': 's21',
               '$': 'r( E -> E - T )'},
        '28': {')': 'r( T -> T * F )', '*': 'r( T -> T * F )', '+': 'r( T -> T * F )', '-': 'r( T -> T * F )',
               '/': 'r( T -> T * F )', '$': 'r( T -> T * F )'},
        '29': {')': 'r( T -> T / F )', '*': 'r( T -> T / F )', '+': 'r( T -> T / F )', '-': 'r( T -> T / F )',
               '/': 'r( T -> T / F )', '$': 'r( T -> T / F )'},
        '30': {')': 'r( F -> G ^ F )', '*': 'r( F -> G ^ F )', '+': 'r( F -> G ^ F )', '-': 'r( F -> G ^ F )',
               '/': 'r( F -> G ^ F )', '$': 'r( F -> G ^ F )'},
        '31': {')': 'r( G -> ( E ) )', '*': 'r( G -> ( E ) )', '+': 'r( G -> ( E ) )', '-': 'r( G -> ( E ) )',
               '/': 'r( G -> ( E ) )', '^': 'r( G -> ( E ) )', '$': 'r( G -> ( E ) )'},
        '32': {')': 's33', '+': 's18', '-': 's19'},
        '33': {')': 'r( G -> Func ( E ) )', '*': 'r( G -> Func ( E ) )', '+': 'r( G -> Func ( E ) )',
               '-': 'r( G -> Func ( E ) )', '/': 'r( G -> Func ( E ) )', '^': 'r( G -> Func ( E ) )',
               '$': 'r( G -> Func ( E ) )'}}
    return parsing_table


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

        #elif self.value == '^':
        #    if isinstance(self.right.value, int) or isinstance(self.right.value, float): # a in the exp
        #        new_exp = Node('-', self.right, Node(1))
        #        return Node('*', Node('*', self.right, Node('^', self.left, new_exp)),self.left.differentiate())
        #    else:
        #        return Node('*', Node('^', self.left, self.right), Node('+', Node('*', self.right.differentiate(), Node('ln', self.left)),
        #                         Node('*', Node('/', self.right, self.left), self.left.differentiate())))

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

        # f(g(x)) - not implemented yet !!!!!!

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
                    result = self.evaluate(lhs, values)

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

    def evaluate(self, lhs, values):
        if lhs == 'E':
            if len(values) == 1:
                return values[0]
            elif values[1].value == '+':
                if isinstance(values[0].value, (int, float)) and isinstance(values[2].value, (int, float)):
                    return Node(values[0].value + values[2].value)
                if values[0].value == 0:
                    return Node(values[2].value)
                if values[2].value == 0:
                    return Node(values[0].value)
                return Node('+', values[0], values[2])
            elif values[1].value == '-':
                if isinstance(values[0].value, (int, float)) and isinstance(values[2].value, (int, float)):
                    return Node(values[0].value - values[2].value)
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
                return values[1]
            elif len(values) == 4:
                func = values[0].value
                arg = values[2]
                return Node(func,None, arg)
        return values[0]

    def differentiate_expression(self, expression,i):
        tokens = tokenize(expression)
        parsed_expr = self.parse(tokens)
        #create_tree_png(parsed_expr, f"derivative/{i}_parsed_expr")

        differentiated_expr = parsed_expr.differentiate()
        
        #tokens2 = tokenize(tree_to_string(differentiated_expr))
        #parsed_expr2 = self.parse(tokens2)
        #print(parsed_expr2)

        return differentiated_expr

def tree_to_string(node):
    if node.left is None and node.right is None:
        return str(node.value)
    left_str = tree_to_string(node.left) if node.left else ""
    right_str = tree_to_string(node.right) if node.right else ""
    if node.value in ["cos","sin","ln","tg","arcsin","arccos","arctg","exp"]:
        return f"{left_str} {node.value} ({right_str})"
    return f"({left_str} {node.value} {right_str})"


def test_differentiation():
    parsing_table = get_parsing_table3()
    X = 2
    parser = LRParser(parsing_table, X)
    expressions = [
        #"-6",
        #"sin(X)",
        #"X^3",
        #"cos(X)",
        #"tg(X)",
        #"arcsin(X)",
        #"arccos(X)",
        #"arctg(X)",
        #"exp(X)",
        #"ln(X)",
        "sin(2*X)",
        "cos(2*X)",
        "sin(2*cos(2*X)^(2*X))",
        "-6",
        "X",
        "8",
        "X + 8",
        "3*X",
        "X^2",

        "X^2 + 3*X + 1",
        "X^3",
        "X^2 * X + 3",
        "(X + 1)^2",
        "(2*X + 1)^2",

        "X^2 / X^2"
    ]
    i = 0
    for expression in expressions:

        derivative = parser.differentiate_expression(expression,i)
        derivative_str = tree_to_string(derivative)
        #create_tree_png(derivative, f"derivative/{i}_derivative")
        i += 1
        np.gradient("cos(x)")
        print(f"Expression: {expression}")
        print(f"Derivative: {derivative_str}")


        x = sp.symbols('X')

        derivative_us = sp.sympify(derivative_str.replace("^", "**").replace("tg","tan").replace("arc","a"))
        print(f"derivative_us: {derivative_us}")

        expression_sp = sp.sympify(expression.replace("^", "**").replace("tg", "tan").replace("arc","a"))
        derivative_sp = sp.diff(expression_sp, x)
        print(f"derivative_sp: {derivative_sp}")


        print(f"Are they equivalent? {derivative_us.subs(x,2).evalf(4) == derivative_sp.subs(x,2).evalf(4)}")
        #equivalence = sp.Eq(sp.sympify(derivative_us), sp.sympify(derivative_sp))
        #print(f"Are they equivalent? {equivalence}")

        print()


if __name__ == "__main__":
    test_differentiation()
