import graphviz
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