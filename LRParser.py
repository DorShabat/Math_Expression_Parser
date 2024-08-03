# save 02/08/2024 10:30
# evaluation working
import numpy as np


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
                        value_stack.append(self.X)
                    else:
                        value_stack.append(token[1])
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


def main():
    parsing_table = get_parsing_table3()
    expressions = [
        ("X+6", 8),
        ("sin(0)", 0),
        ("3 + 4", 7),  # Expected: 7
        ("(3 + 4)", 7),  # Expected: 7
        ("(2 * (3 + 4))", 14),  # Expected: 14
        ("(5 - 3)", 2),  # Expected: 2
        ("((2 + 3) * (4 - 1))", 15),  # Expected: 15
    ]
    X = 2
    parser = LRParser(parsing_table, X)
    for expression, expected in expressions:

        tokens = tokenize(expression)
        try:
            result = parser.parse(tokens)
            print(f"The result of the expression '{expression}' is {result}. Expected: {expected}")
        except RuntimeError as e:
            print(f"Error parsing expression '{expression}': {e}")


if __name__ == "__main__":
    main()
