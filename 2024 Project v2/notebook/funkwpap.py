import sympy as sp, numpy as np, gurobipy as gb, pandas as pd
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
    try:
        # Create a temporary variable to hold the result
        temp_expr = sympy_expr.simplify()
        temp_expr = temp_expr.apart()

        # Only if both operations succeed, update sympy_expr
        sympy_expr = temp_expr
    except Exception as e:
        # Handle the exception and continue with the old value
        # print(f"An error occurred: {e}")
            a=5
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
        

        if isinstance(exp, sp.Number):
            # Handle non-quadratic powers using general constraints
            exp_value = float(exp)
            aux_var_name = f"pow_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            pow_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
            model.addGenConstrPow(base_aux_var, pow_aux_var, exp_value)
            return pow_aux_var
        else:
            # Handle symbolic powers using general constraints
            aux_var_name = f"pow_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            pow_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)

            # Convert the base to a Gurobi expression
            base_expr = sympy_to_gurobi(base, symbol_map, model, aux_var_count)

            # Create an auxiliary variable for the base
            base_aux_var_name = f"base_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            base_aux_var = model.addVar(name=base_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addConstr(base_aux_var == base_expr)

            # Create an auxiliary variable for the logarithm of the base
            log_base_aux_var_name = f"log_base_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            log_base_aux_var = model.addVar(name=log_base_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addGenConstrLog(base_aux_var, log_base_aux_var)

            # Convert the exponent to a Gurobi expression
            exp_expr = sympy_to_gurobi(exp, symbol_map, model, aux_var_count)

            # Create an auxiliary variable for the product of the exponent and the logarithm of the base
            prod_aux_var_name = f"prod_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            prod_aux_var = model.addVar(name=prod_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addConstr(prod_aux_var == exp_expr * log_base_aux_var)

            # Create the exponential constraint
            model.addGenConstrExp(prod_aux_var, pow_aux_var)

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


    elif isinstance(sympy_expr, sp.Number):
        return float(sympy_expr)
    
    else:
        raise ValueError(f"Unsupported SymPy expression: {sympy_expr}")
    
x = sp.symbols('x')

class Regulator:
    _id_counter = 1

    def __init__(self, name, permit_price, emission_cap):
        self.id = Regulator._id_counter
        Regulator._id_counter += 1
        self.name = name
        self.permit_price = permit_price
        self.emission_cap = emission_cap

    def __repr__(self):
        return f"Regulator(id={self.id}, name='{self.name}', permit_price={self.permit_price}, emission_cap={self.emission_cap})"

# Global registries for sectors and countries
sector_registry = {}
country_registry = {}
firm_registry = {}

class Sector:
    _id_counter = 1

    def __init__(self, name, price_demand_function, free_emission_multiplier):
        self.id = Sector._id_counter
        Sector._id_counter += 1
        self.name = name
        self.price_demand_function = price_demand_function
        self.free_emission_multiplier = free_emission_multiplier
        self.firms = []  # List to store firms in this sector

        # Register this sector in the global registry
        sector_registry[self.id] = self

    def add_firm(self, firm):
        self.firms.append(firm)

    def __repr__(self):
        return f"Sector(id={self.id}, name='{self.name}', firms={len(self.firms)})"

class Country:
    _id_counter = 1

    def __init__(self, name, size):
        self.id = Country._id_counter
        Country._id_counter += 1
        self.name = name
        self.size = size
        self.firms = []  # List to store firms in this country

        # Register this country in the global registry
        country_registry[self.id] = self

    def add_firm(self, firm):
        self.firms.append(firm)

    def __repr__(self):
        return f"Country(id={self.id}, name='{self.name}', firms={len(self.firms)})"

class Firm:
    _id_counter = 1

    def __init__(self, name, sector, country, production_cost_function, abatement_cost_function, actual_output, emission, profit, BAU_output=0, BAU_emission=0):
        self.id = Firm._id_counter
        Firm._id_counter += 1
        self.name = name

        # Determine sector and country from either object or ID
        if isinstance(sector, Sector):
            self.sector = sector
        elif isinstance(sector, int):
            self.sector = sector_registry.get(sector)

        if isinstance(country, Country):
            self.country = country
        elif isinstance(country, int):
            self.country = country_registry.get(country)

        self.production_cost_function = production_cost_function
        self.abatement_cost_function = abatement_cost_function
        self.actual_output = actual_output
        self.emission = emission
        self.profit = profit
        self.BAU_output = BAU_output
        self.BAU_emission = BAU_emission

        # Register this firm in the global registry
        firm_registry[self.id] = self
        
        # Register this firm with its sector and country
        if self.sector:
            self.sector.add_firm(self)
        if self.country:
            self.country.add_firm(self)

    def __repr__(self):
        return f"Firm(id={self.id}, name='{self.name}', sector_id={self.sector.id if self.sector else None}, country_id={self.country.id if self.country else None}, actual_output={self.actual_output}, emission={self.emission}, profit={self.profit})"
    