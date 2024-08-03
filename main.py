class TreeNode:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None


def simplify_expression_tree(root):
    if root is None:
        return 0, 0  # (constant part, coefficient of x)

    # If the node is a leaf and contains 'x'
    if root.value == 'x':
        return 0, 1

    # If the node is a leaf and contains a number
    if root.left is None and root.right is None:
        return float(root.value), 0

    # Recursively simplify left and right subtrees
    left_constant, left_coeff = simplify_expression_tree(root.left)
    right_constant, right_coeff = simplify_expression_tree(root.right)

    # Apply the operator on the values from left and right subtrees
    if root.value == '+':
        return left_constant + right_constant, left_coeff + right_coeff
    elif root.value == '-':
        return left_constant - right_constant, left_coeff - right_coeff
    elif root.value == '*':
        if left_coeff != 0 and right_coeff != 0:
            raise ValueError("Expression cannot have x^2 term")
        elif left_coeff != 0:
            return left_constant * right_constant, left_coeff * right_constant
        elif right_coeff != 0:
            return left_constant * right_constant, right_coeff * left_constant
        else:
            return left_constant * right_constant, 0
    elif root.value == '/':
        if right_coeff != 0:
            raise ValueError("Expression cannot divide by x")
        return left_constant / right_constant, left_coeff / right_constant
    elif root.value == '^':
        raise ValueError("Expression cannot handle exponentiation with variable x")

    raise ValueError(f"Unknown operator: {root.value}")
# Creating the nodes for the expression (4 * x) * 5
root = TreeNode('*')
root.left = TreeNode('*')
root.right = TreeNode('5')
root.left.left = TreeNode('4')
root.left.right = TreeNode('x')

# Simplify the tree
constant, coefficient = simplify_expression_tree(root)
print(f"Simplified expression: {constant} + {coefficient}*x")
