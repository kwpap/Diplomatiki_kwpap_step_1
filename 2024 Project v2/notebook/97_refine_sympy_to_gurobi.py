import sympy as sp
from gurobipy import Model, LinExpr, QuadExpr, GRB

# Define a function to convert SymPy expression to Gurobi expression
def sympy_to_gurobi(sympy_expr, symbol_map):
    """
    Recursively convert a SymPy expression to a Gurobi expression.
    
    Parameters:
        sympy_expr (sp.Expr): SymPy expression to convert.
        symbol_map (dict): Mapping from SymPy symbols to Gurobi variables.
        
    Returns:
        Gurobi expression (LinExpr, QuadExpr, or constant).
    """
    if isinstance(sympy_expr, sp.Symbol):
        return symbol_map[sympy_expr]
    
    elif isinstance(sympy_expr, sp.Add):
        return sum(sympy_to_gurobi(arg, symbol_map) for arg in sympy_expr.args)
    
    elif isinstance(sympy_expr, sp.Mul):
            result = 1
            for arg in sympy_expr.args:
                result *= sympy_to_gurobi(arg, symbol_map)
            return result
    
    elif isinstance(sympy_expr, sp.Pow):
        base, exp = sympy_expr.args
        if exp == 2:
            base_expr = sympy_to_gurobi(base, symbol_map)
            return QuadExpr(base_expr * base_expr)
        else:
            raise ValueError("Non-quadratic powers are not supported by Gurobi.")
    
    elif isinstance(sympy_expr, sp.Number):
        return float(sympy_expr)
    
    else:
        raise ValueError(f"Unsupported SymPy expression: {sympy_expr}")

# Example usage
x, y = sp.symbols('x y')
sympy_expr = -3*x**2 - 2*y**2 - 4*y*x
print("Sympy expression:", sympy_expr)

# Create a Gurobi model
model = Model("example")
x_gurobi = model.addVar(name="x", lb=3, vtype=GRB.CONTINUOUS, ub = 10)
y_gurobi = model.addVar(name="y", lb=3, vtype=GRB.CONTINUOUS, ub = 10)

# Map SymPy symbols to Gurobi variables
symbol_map = {x: x_gurobi, y: y_gurobi}

# Convert SymPy expression to Gurobi expression
gurobi_expr = sympy_to_gurobi(sympy_expr, symbol_map)

# Set the objective and optimize
model.setObjective(gurobi_expr, GRB.MAXIMIZE)
model.params.OutputFlag = 0  # Suppress output
model.optimize()

# Print the results
for v in model.getVars():
    print(f"{v.varName}: {v.X}")
print(f"Objective: {model.objVal}")
print("Sympy evaluation: %.1f" % float(sympy_expr.subs({x: x_gurobi.X, y: y_gurobi.X}).evalf()))
