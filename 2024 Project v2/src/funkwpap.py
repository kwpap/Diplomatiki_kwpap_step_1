import sympy as sp, numpy as np, gurobipy as gb, pandas as pd
from gurobipy import Model, LinExpr, QuadExpr, GRB
import sys
import random
import sqlite3

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
        self.BAU_emissions = 0
        self.BAU_profit = 0

        # Initialize states as a dictionary of dictionaries
        self.states = {
            "current": {},
            "best": {},
            "previous": {},
            "average_of_10_random": {}
        }

    def save_state(self, state_name="current"):
        """Save the current state of all firms."""
        self.states[state_name] = {
            firm.name: (firm.actual_output, firm.emission, firm.profit)
            for firm in self.firm_registry.values()
        }

    def load_state(self, state_name="current"):
        """Load a saved state and apply it to all firms."""
        if state_name in self.states:
            for firm_name, values in self.states[state_name].items():
                firm = next((f for f in self.firm_registry.values() if f.name == firm_name), None)
                if firm:
                    firm.actual_output, firm.emission, firm.profit = values
        else:
            print(f"State '{state_name}' does not exist.")

    def print_state(self, state_name="current"):
        """Print the saved state for debugging purposes."""
        if state_name in self.states:
            print(f"State '{state_name}':")
            for firm_name, (output, emission, profit) in self.states[state_name].items():
                print(f"Firm: {firm_name}, Output: {output}, Emission: {emission}, Profit: {profit}")
        else:
            print(f"State '{state_name}' does not exist.")

    def __repr__(self):
        return f"Regulator(id={self.id}, name='{self.name}', permit_price={self.permit_price}, emission_cap={self.emission_cap})"
    
    def calculate_average_of_10_states(self, BAU = False, max_output = {}):
        # Here we calculate an average state of 10 random states, to be used as a starting point for the optimization process.
        # Step 1: For every firm, assign random values to the output of the other firms and calculate the output of the firm. 
        # Step 2: Repeat step 1 10 times, and take the average of the outputs.
        temp_output_dict = {}
        temp_emission_dict = {}
        temp_profit_dict = {}
        # Iterate over all sectors
        for sector in self.sector_registry.values():
            # Iterate over all firms in the sector
            for firm in sector.firms:
                temp_output_dict[firm.name] = []
                # Repeat 10 times
                for i in range(10):
                    # Iterate over all sectors and firms again
                    for sector_inner in self.sector_registry.values():
                        for firm_inner in sector_inner.firms:
                            # Set actual_output to a random value
                            firm_inner.actual_output = np.random.uniform(0, max_output[firm_inner.name])
                    
                    # Calculate output, emission, and profit
                    output, emission, profit = firm.calculate_output(BAU=BAU)
                    temp_output_dict[firm.name].append(output)
                    temp_emission_dict[firm.name].append(emission)
                    temp_profit_dict[firm.name].append(profit)

        # Set actual_output to the mean of the values in temp_output_dict
        for sector in self.sector_registry.values():
            for firm in sector.firms:
                firm.actual_output = np.mean(temp_output_dict[firm.name])
                firm.emission = np.mean(temp_emission_dict[firm.name])
                firm.profit = np.mean(temp_profit_dict[firm.name])
        self.save_state(state_name="average_of_10_random")

    def optimize_them_all(self, print_output=False,print_diff=False, precision = 0.01, BAU = False, size_of_diffs = 2):
        iterations = 0
        last_diffs = [999999999]*size_of_diffs
        repeat = True
        second_stage = False
        a = 1
        max_output = {}
        lowest_diff = 999999999
        while repeat :
            repeat = False
            max_diff = 0
            iterations += 1
            for sector in self.sector_registry.values():
                for firm in sector.firms:
                    output, emission, profit = firm.calculate_output(verbose=False, BAU = BAU)
                    max_output[firm.name] = max(max_output.get(firm.name, 0), output)
                    if abs(output - firm.actual_output)>precision or abs(emission - firm.emission)>precision:
                        repeat = True
                    max_diff = max(max_diff, abs(output - firm.actual_output), abs(emission - firm.emission))
                    firm.actual_output = firm.actual_output*(1-a) + output*a
                    firm.emission = firm.emission * (1-a) + emission*a
                    firm.profit = firm.calculate_profit(BAU = BAU)

                    if(print_output):
                        print("Firm {} has output: {:5f} and emission: {:5f} and profit: {:2f}".format(firm.name, firm.actual_output, firm.emission, profit))
            self.save_state()
            if(max_diff < lowest_diff):
                lowest_diff = max_diff
                self.states["best"] = self.states["current"] if self.states["previous"] == {} else self.states["previous"]
            if(print_diff): 
                sys.stdout.write("\rMax diff: {:.2f}, permit price = {:.1f}, cap = {:.0f}, second_stage = {}, a = {:.2f}".format(max_diff, self.permit_price, self.emission_cap, second_stage, a))
                sys.stdout.flush()
            # print(max(last_diffs))
            if max_diff > max(last_diffs):
                # print("It failed to converge with permit = {}, cap = {}, a = {}".format(self.permit_price, self.emission_cap, a))
                a = a*0.9
                repeat = True
                self.load_state("average_of_10_random") if second_stage else self.load_state("best")
            if a<0.1 and second_stage: # There is no stage 3
                self.load_state("best")
                print ("Everything failed, best result is {}".format(lowest_diff))
                repeat = False
            if a<0.1:
                second_stage = True
                a = 1
                self.load_state("average_of_10_random")
            
            last_diffs[iterations%size_of_diffs] = max_diff
            self.save_state("previous")

    def BAU_calculator(self, precision = 0.01, print_diff = False):
        self.optimize_them_all(precision = precision, print_diff = print_diff, BAU = True)
        self.BAU_emissions = sum([firm.actual_output for firm in self.firm_registry.values()])
        self.BAU_profit = sum([firm.profit for firm in self.firm_registry.values()])
        for firm in self.firm_registry.values():
            firm.BAU_output = firm.actual_output
            firm.BAU_emission = firm.actual_output
            firm.BAU_profit = firm.profit

    def find_optimal_permit_price_to_meet_the_emission_cap_requirements(self, precision = 0.1, permit_price_tolerance = 0.5, x_low = 0, x_high = 1000, size_of_diffs = 10):

        while x_high - x_low > permit_price_tolerance:
            x_mid = (x_high + x_low)/2
            self.permit_price = x_mid
            self.optimize_them_all(print_output=False, print_diff=True, precision = precision, size_of_diffs= size_of_diffs)
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

    def __init__(self, name, sector, country, production_cost_function, abatement_cost_function, actual_output, emission, profit, regulator, BAU_output=0, BAU_emission=0, BAU_profit=0):
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
        self.BAU_profit = BAU_profit
        self.regulator = regulator
        self.BAU_emission = 0

        # Register this firm in the global registry
        regulator.firm_registry[self.id] = self
        
        # Register this firm with its sector and country
        if self.sector:
            self.sector.add_firm(self)
        if self.country:
            self.country.add_firm(self)

    def __repr__(self):
        return f"Firm(id={self.id}, name='{self.name}', sector_id={self.sector.id if self.sector else None}, country_id={self.country.id if self.country else None}, actual_output={self.actual_output}, emission={self.emission}, profit={self.profit})"


    def calculate_sales(self):
        # Calculate the sales of the firm
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
        # Get the production cost function of the firm
        production_cost_function = self.production_cost_function


        out, em = sp.symbols('out em')
        # Calculate the output of the firm
        #print("Price to demand Function: {}".format(price_demand_function.subs(x, sum_other_outputs + out)))
        income = (price_demand_function.subs(x, sum_other_outputs + out) - production_cost_function.subs(x, out))*out
        return income.subs(out, self.actual_output).evalf()

    def calculate_abatement(self):
        abatement_cost_function = self.abatement_cost_function
        out, em = sp.symbols('out em')
        abatement = abatement_cost_function.subs({x: self.actual_output - self.emission, y: self.emission})
        return abatement.evalf()

    def calculate_trading(self):
        sector = self.sector
        regulator = self.regulator
        # Sum of all other outputs

        #Get the price of the permits
        permit_price = regulator.permit_price
        # Get the free emission multiplier of the firm
        free_emission_multiplier = sector.free_emission_multiplier
        out, em = sp.symbols('out em')
        trading = permit_price * (em -free_emission_multiplier * out)
        return trading.subs({out: self.actual_output, em: self.emission}).evalf()

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
    
    def calculate_profit(self, BAU = False):
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
        if BAU: 
            abatement = 0
            trading = 0
        return (income + abatement + trading).subs({out: self.actual_output, em: self.emission}).evalf()

    