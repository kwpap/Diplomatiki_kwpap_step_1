import sympy as sp
from gurobipy import Model, LinExpr, QuadExpr, GRB

# Define a function to convert SymPy expression to Gurobi expression
def sympy_to_gurobi(sympy_expr, symbol_map, model, aux_var_count=[0]):
    """
    Recursively convert a SymPy expression to a Gurobi expression, 
    handling exponentials, powers, divisions, and other complex expressions with auxiliary variables and constraints.
    
    Parameters:
        sympy_expr (sp.Expr): SymPy expression to convert.
        symbol_map (dict): Mapping from SymPy symbols to Gurobi variables.
        model (gurobipy.Model): Gurobi model to add constraints for complex expressions.
        aux_var_count (list): A list to keep track of the auxiliary variable count.
        
    Returns:
        Gurobi expression (LinExpr, QuadExpr, or constant).
    """
    if isinstance(sympy_expr, sp.Symbol):
        return symbol_map[sympy_expr]
    
    elif isinstance(sympy_expr, sp.Add):
        return sum(sympy_to_gurobi(arg, symbol_map, model, aux_var_count) for arg in sympy_expr.args)
    
    elif isinstance(sympy_expr, sp.Mul):
        result = 1
        for arg in sympy_expr.args:
            result *= sympy_to_gurobi(arg, symbol_map, model, aux_var_count)
        return result
    
    elif isinstance(sympy_expr, sp.Pow):
        base, exp = sympy_expr.args
        
        # Always create an auxiliary variable for the base
        base_expr = sympy_to_gurobi(base, symbol_map, model, aux_var_count)
        aux_var_name = f"pow_base_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        base_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        model.addConstr(base_aux_var == base_expr)
        
        if exp == 2:
            # Handle quadratic expression
            return QuadExpr(base_aux_var * base_aux_var)
        else:
            # Handle non-quadratic powers using general constraints
            exp_value = float(exp)
            aux_var_name = f"pow_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            pow_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
            model.addGenConstrPow(base_aux_var, pow_aux_var, exp_value)
            return pow_aux_var
    
    elif isinstance(sympy_expr, sp.exp):
        arg_expr = sympy_to_gurobi(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"exp_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", lb=0, vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        exp_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        model.addGenConstrExp(arg_aux_var, exp_aux_var)
        return exp_aux_var
    
    elif isinstance(sympy_expr, sp.log):
        arg_expr = sympy_to_gurobi(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"log_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        log_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        model.addGenConstrLog(arg_aux_var, log_aux_var)
        return log_aux_var

    elif isinstance(sympy_expr, sp.sin):
        arg_expr = sympy_to_gurobi(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"sin_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        sin_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        # Add piecewise constraints here for approximation
        return sin_aux_var

    elif isinstance(sympy_expr, sp.cos):
        arg_expr = sympy_to_gurobi(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"cos_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        cos_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        # Add piecewise constraints here for approximation
        return cos_aux_var

    elif isinstance(sympy_expr, sp.tan):
        arg_expr = sympy_to_gurobi(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"tan_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        tan_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        # Add piecewise constraints here for approximation
        return tan_aux_var

    elif isinstance(sympy_expr, sp.Mul) and any(isinstance(arg, sp.Pow) and arg.exp == -1 for arg in sympy_expr.args):
        # Handling division by separating numerator and denominator
        numerator = 1
        denominator = 1
        for arg in sympy_expr.args:
            if isinstance(arg, sp.Pow) and arg.exp == -1:
                denominator *= arg.base
            else:
                numerator *= arg
        
        # Handle numerator
        num_expr = sympy_to_gurobi(numerator, symbol_map, model, aux_var_count)
        num_aux_var_name = f"num_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        num_aux_var = model.addVar(name=num_aux_var_name, vtype=GRB.CONTINUOUS)
        model.addConstr(num_aux_var == num_expr)
        
        # Handle denominator
        denom_expr = sympy_to_gurobi(denominator, symbol_map, model, aux_var_count)
        denom_aux_var_name = f"denom_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        denom_aux_var = model.addVar(name=denom_aux_var_name, vtype=GRB.CONTINUOUS)
        model.addConstr(denom_aux_var == denom_expr)
        
        # Create the auxiliary variable for the inverse of the denominator
        inv_denom_aux_var_name = f"inv_denom_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        inv_denom_aux_var = model.addVar(name=inv_denom_aux_var_name, vtype=GRB.CONTINUOUS)
        model.addConstr(inv_denom_aux_var * denom_aux_var == 1)
        
        # Final expression: numerator * (1/denominator)
        return num_aux_var * inv_denom_aux_var

    elif isinstance(sympy_expr, sp.Number):
        return float(sympy_expr)
    
    else:
        raise ValueError(f"Unsupported SymPy expression: {sympy_expr}")

def calculate_output(firm, firms, dummy_market, sectors):

    # Calculate the output of the firm

    # Sum of all other outputs
    sum_other_outputs = 0
    for i in range(len(firms)):
        if firms[i]['id'][0] != firm['id'][0] and firms[i]['sector_id'][0] == firm['sector_id'][0]:
            sum_other_outputs += firms[i]['actual_output'][0]
    
    #Get the price of the permits
    permit_price = dummy_market['permit_price'][0]
    # Get the price demand function of the sector
    price_demand_function = sectors[firm['sector_id'][0]]['price_demand_function'][0]
    # Get the abatement cost function of the firm
    abatement_cost_function = firm['abatement_cost_function'][0]
    # Get the production cost function of the firm
    production_cost_function = firm['production_cost_function'][0]
    # Get the free emission multiplier of the sector
    free_emission_multiplier = sectors[firm['sector_id'][0]]['free_emission_multiplier'][0]


    out, em = sp.symbols('out em')


    # Calculate the output of the firm


    #print("Price to demand Function: {}".format(price_demand_function.subs(x, sum_other_outputs + out)))
    income = (price_demand_function.subs(x, sum_other_outputs + out) - production_cost_function.subs(x, out))*out
    abatement = -abatement_cost_function.subs(x, out - em)
    trading = - permit_price * (em -free_emission_multiplier * out)
    profit_expr = income + abatement + trading

    m = gp.Model("firm")
    output = m.addVar(vtype=GRB.CONTINUOUS, name="output", lb=0)
    emission = m.addVar(vtype=GRB.CONTINUOUS, name="emission", lb=0)
    symbol_map = {out: output, em: emission}
    profit = sympy_to_gurobi(profit_expr, symbol_map)
    m.setObjective(profit, GRB.MAXIMIZE)
    gur_abatement = sympy_to_gurobi(abatement, symbol_map)
    m.addConstr(emission <= output)
    m.Params.LogToConsole = 0
    #m.write("wut.lp")
    m.optimize()
    # print("income: {}".format(income.subs(out, output.X).subs(em, emission.X)))
    # print("abatement: {}".format(abatement.subs(out, output.X).subs(em, emission.X)))
    # print("trading: {}".format(trading.subs(out, output.X).subs(em, emission.X)))
    # for v in m.getVars():
    #     print(f"{v.varName}: {v.X}")
    # return sum_other_outputs
    return output.X, emission.X, profit_expr.subs(out, output.X).subs(em, emission.X).evalf()

