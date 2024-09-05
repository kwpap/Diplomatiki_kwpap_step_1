import sympy as sp, numpy as np, gurobipy as gb, pandas as pd
from gurobipy import Model, LinExpr, QuadExpr, GRB
import sys

def get_emission(firms):
    total_emission = 0
    for firm in firms:
        total_emission += firm.emission
    return total_emission


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
y = sp.symbols('y')

class Regulator:
    _id_counter = 1

    def __init__(self, name, permit_price, emission_cap):
        self.id = Regulator._id_counter
        Regulator._id_counter += 1
        self.name = name
        self.permit_price = permit_price
        self.emission_cap = emission_cap
        self.sector_registry = {}
        self.country_registry = {}
        self.firm_registry = {}

    def __repr__(self):
        return f"Regulator(id={self.id}, name='{self.name}', permit_price={self.permit_price}, emission_cap={self.emission_cap})"
    
    def optimize_them_all(self, print_output=False,print_diff=False, precision = 0.01, max_iter = 30, BAU = False):
        repeat = True
        counter = 0
        while repeat and counter < max_iter:
            counter +=1
            max_diff = 0
            repeat = False

            max_output = {}
            for sector in self.sector_registry.values():
                for firm in sector.firms:
                    output, emission, profit = firm.calculate_output(verbose=False, BAU = BAU)
                    max_output[firm.name] = max(max_output.get(firm.name, 0), output)

                    if abs(output - firm.actual_output)>precision or abs(emission - firm.emission)>precision:
                        repeat = True
                    max_diff = max(max_diff, abs(output - firm.actual_output), abs(emission - firm.emission))
                    firm.actual_output = output
                    firm.emission = emission
                    firm.profit = profit
                    if(print_output):
                        print("Firm {} has output: {:5f} and emission: {:5f} and profit: {:2f}".format(firm.name, firm.actual_output, firm.emission, profit))
            if(print_diff): 
                sys.stdout.write("\rMax diff: {:5f}".format(max_diff))
                sys.stdout.flush()
        if counter == max_iter:
            print("It doesn't converge initially for cap = {}".format(self.emission_cap))
            # In this case, the calculation will be different. 
            # Step 1: For every firm, assign random values to the output of the other firms and calculate the output of the firm. 
            # Step 2: Repeat step 1 10 times, and take the average of the outputs.

            for sector in self.sector_registry.values():
                for firm in sector.firms:
                    output_list = []
                    for i in range(10):
                        for firm in sector.firms:
                            firm.actual_output = np.random.uniform(0, max_output[firm.name])
                        output, emission, profit = firm.calculate_output(BAU = BAU)
                        output_list.append(output)
                    firm.actual_output = np.mean(output_list)

            # Step 3: Use the average as a starting point and repeat the optimization process. This time, the new value can affect the old one by up to 10%.
            # Step 4: Repeat step 3 until the difference between the new and old values is less than 1%.
            a = 0.1
            repeat = True
            precision *= 10
            counter = 0
            while repeat and counter < max_iter*10:
                lp_counter = 0
                max_diff = 0
                repeat = False
                counter +=1
                for sector in self.sector_registry.values():
                    for firm in sector.firms:

                        output, emission, profit = firm.calculate_output(BAU = BAU)
                        if abs(output - firm.actual_output)>precision or abs(emission - firm.emission)>precision:
                            repeat = True
                        max_diff = max(max_diff, abs(output - firm.actual_output), abs(emission - firm.emission))
                        firm.actual_output = firm.actual_output*(1-a) + output*a
                        firm.emission = emission
                        firm.profit = profit
                        if(print_output):
                            print("Firm {} has output: {:2f} and emission: {:2f} and profit: {:2f}".format(firm.name, firm.actual_output, firm.emission, profit))
                if(print_diff): 
                    sys.stdout.write("\rMax diff: {:3f}".format(max_diff))
                    sys.stdout.flush()
            # Step 5: If it doesn't converge, return an error message.
            if counter == max_iter:
                print("It doesn't converge")

    def find_optimal_permit_price_to_meet_the_emission_cap_requirements(self, precision = 0.1, permit_price_tolerance = 0.5, x_low = 0, x_high = 1000):

        while x_high - x_low > permit_price_tolerance:
            x_mid = (x_high + x_low)/2
            self.permit_price = x_mid
            self.optimize_them_all(print_output=False, print_diff=True, precision = precision)
            total_emission = get_emission(self.firm_registry.values())
            sys.stdout.write("\rPermit price: {:2f}".format(self.permit_price))
            sys.stdout.flush()
            if total_emission > self.emission_cap:
                x_low = x_mid
            else:
                x_high = x_mid
            self.permit_price = x_mid
            total_emission = get_emission(self.firm_registry.values())
        if total_emission > self.emission_cap:
            x_mid = x_high
            self.permit_price = x_high
            self.optimize_them_all(print_output=False, print_diff=True, precision = precision)
            total_emission = get_emission(self.firm_registry.values())
        print("Permit price: {} and total emission: {} and emission cap {}".format(self.permit_price, total_emission, self.emission_cap))
        return x_mid



class Sector:
    _id_counter = 1

    def __init__(self, name, price_demand_function, free_emission_multiplier, regulator):
        self.id = Sector._id_counter
        Sector._id_counter += 1
        self.name = name
        self.price_demand_function = price_demand_function
        self.free_emission_multiplier = free_emission_multiplier
        self.firms = []  # List to store firms in this sector

        # Register this sector in the global registry
        regulator.sector_registry[self.id] = self

    def add_firm(self, firm):
        self.firms.append(firm)

    def __repr__(self):
        return f"Sector(id={self.id}, name='{self.name}', firms={len(self.firms)})"

class Country:
    _id_counter = 1

    def __init__(self, name, size,regulator):
        self.id = Country._id_counter
        Country._id_counter += 1
        self.name = name
        self.size = size
        self.firms = []  # List to store firms in this country

        # Register this country in the global registry
        regulator.country_registry[self.id] = self

    def add_firm(self, firm):
        self.firms.append(firm)

    def __repr__(self):
        return f"Country(id={self.id}, name='{self.name}', firms={len(self.firms)})"

class Firm:
    _id_counter = 1

    def __init__(self, name, sector, country, production_cost_function, abatement_cost_function, actual_output, emission, profit, regulator, BAU_output=0, BAU_emission=0):
        self.id = Firm._id_counter
        Firm._id_counter += 1
        self.name = name

        # Determine sector and country from either object or ID
        if isinstance(sector, Sector):
            self.sector = sector
        elif isinstance(sector, int):
            self.sector = regulator.sector_registry.get(sector)

        if isinstance(country, Country):
            self.country = country
        elif isinstance(country, int):
            self.country = regulator.country_registry.get(country)

        self.production_cost_function = production_cost_function
        self.abatement_cost_function = abatement_cost_function
        self.actual_output = actual_output
        self.emission = emission
        self.profit = profit
        self.BAU_output = BAU_output
        self.BAU_emission = BAU_emission
        self.regulator = regulator

        # Register this firm in the global registry
        regulator.firm_registry[self.id] = self
        
        # Register this firm with its sector and country
        if self.sector:
            self.sector.add_firm(self)
        if self.country:
            self.country.add_firm(self)

    def __repr__(self):
        return f"Firm(id={self.id}, name='{self.name}', sector_id={self.sector.id if self.sector else None}, country_id={self.country.id if self.country else None}, actual_output={self.actual_output}, emission={self.emission}, profit={self.profit})"


    def calculate_output(self, verbose=False, writeLP=False, BAU=False):
        # Calculate the output of the firm
        sector = self.sector
        regulator = self.regulator
        # Sum of all other outputs
        sum_other_outputs = 0
        for i in range(len(sector.firms)):
            if sector.firms[i].id != self.id:
                sum_other_outputs += sector.firms[i].actual_output
        
        #Get the price of the permits
        permit_price = regulator.permit_price
        # Get the price demand function of the sector
        price_demand_function = sector.price_demand_function
        # Get the abatement cost function of the firm
        abatement_cost_function = self.abatement_cost_function
        # Get the production cost function of the firm
        production_cost_function = self.production_cost_function
        # Get the free emission multiplier of the firm
        free_emission_multiplier = sector.free_emission_multiplier

        out, em = sp.symbols('out em')
        # Calculate the output of the firm
        #print("Price to demand Function: {}".format(price_demand_function.subs(x, sum_other_outputs + out)))
        income = (price_demand_function.subs(x, sum_other_outputs + out) - production_cost_function.subs(x, out))*out
        abatement = -abatement_cost_function.subs({x: out - em, y: em})
        trading = - permit_price * (em -free_emission_multiplier * out)
        if BAU: # If BAU is True, then the firm is operating at Business as Usual, meaning that there is no objective to abate emissions nor to trade a valueless item
            abatement = 0
            trading = 0
        
        profit_expr = income + abatement + trading
        # print("Profit Expression: {}".format(profit_expr))
        # Solve the optimization problem
        m = gb.Model("firm")
        output = m.addVar(vtype = gb.GRB.CONTINUOUS, name = "output", lb = 0)
        emission = m.addVar(vtype = gb.GRB.CONTINUOUS, name = "emission", lb = 0)
        symbol_map = {out: output, em: emission}
        profit = sympy_to_gurobi(profit_expr, symbol_map, m)
        m.setObjective(profit, gb.GRB.MAXIMIZE) 
        m.addConstr(emission <= output)

        m.params.OutputFlag = 1 if verbose else 0

        if writeLP:
            m.write(f"firm{self.id}.lp")
        #check if output.X is None
        m.optimize()
        return output.X, emission.X, profit.getValue()

