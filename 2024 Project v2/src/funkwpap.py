import sympy as sp, numpy as np, gurobipy as gb, pandas as pd
from gurobipy import Model, LinExpr, QuadExpr, GRB
import sys
import random

def get_emission(firms):
    total_emission = 0
    for firm in firms:
        total_emission += firm.emission
    return total_emission



def eq_check_condition(cond_value, expected, precision=0.001):
    """Check if the condition value meets the expected criteria."""
    if expected == "zero" and abs(cond_value) < precision:
        return "✔️", True
    elif expected == "negative" and cond_value < 0:
        return "✔️", True
    elif expected == "positive" and cond_value > 0:
        return "✔️", True
    else:
        return "⚠️", False

def eq_format_number(value):
    """Format number in scientific notation if too large or small, otherwise to 4 decimals."""
    if abs(value) >= 10000 or abs(value) < 0.001:
        return f"{value:>+9.2e}"  # Scientific notation with proper alignment
    else:
        return f"{value:>+9.4f}"  # Fixed-point notation for normal numbers

x, y = sp.symbols('x y')

def sympy_to_gurobi(sympy_expr, symbol_map, model):
    """
    Convert a SymPy expression to a Gurobi expression, handling polynomials of up to the second degree.
    
    Parameters:
        sympy_expr (sp.Expr): SymPy expression to convert.
        symbol_map (dict): Mapping from SymPy symbols to Gurobi variables.
        model (gurobipy.Model): Gurobi model to add constraints for complex expressions.
        
    Returns:
        Gurobi expression (LinExpr, QuadExpr, or constant).
    """
    def is_simple_expression(expr):
        expr = expr.simplify()
        expr = expr.expand()
        if isinstance(expr, (sp.Symbol, sp.Number)):
            return True
        elif isinstance(expr, sp.Add) or isinstance(expr, sp.Mul):
            return all(is_simple_expression(arg) for arg in expr.args)
        elif isinstance(expr, sp.Pow):
            base, exp = expr.args
            return exp == 2 and is_simple_expression(base)
        return False

    if is_simple_expression(sympy_expr):
        return sympy_to_gurobi_Quadratics_only(sympy_expr, symbol_map, model)
    else:
        return sympy_to_gurobi_complete(sympy_expr, symbol_map, model)

def sympy_to_gurobi_Quadratics_only(sympy_expr, symbol_map, model):
    """
    Convert a SymPy expression to a Gurobi expression, handling polynomials of up to the second degree.
    
    Parameters:
        sympy_expr (sp.Expr): SymPy expression to convert.
        symbol_map (dict): Mapping from SymPy symbols to Gurobi variables.
        model (gurobipy.Model): Gurobi model to add constraints for complex expressions.
        
    Returns:
        Gurobi expression (LinExpr, QuadExpr, or constant).
    """
    if isinstance(sympy_expr, sp.Symbol):
        return symbol_map[sympy_expr]
    
    elif isinstance(sympy_expr, sp.Add):
        return sum(sympy_to_gurobi_Quadratics_only(arg, symbol_map, model) for arg in sympy_expr.args)
    
    elif isinstance(sympy_expr, sp.Mul):
        result = 1
        for arg in sympy_expr.args:
            result *= sympy_to_gurobi_Quadratics_only(arg, symbol_map, model)
        return result
    
    elif isinstance(sympy_expr, sp.Pow):
        base, exp = sympy_expr.args
        
        if exp == 2:
            # Handle quadratic expressions
            base_expr = sympy_to_gurobi_Quadratics_only(base, symbol_map, model)
            return base_expr * base_expr
        elif exp == 1:
            # Any base to the power of 1 is the base itself
            return sympy_to_gurobi_Quadratics_only(base, symbol_map, model)
        elif exp == 0:
            # Any base to the power of 0 is 1
            return 1
        else:
            raise ValueError(f"Unsupported power: {exp}")
    
    elif isinstance(sympy_expr, sp.Number):
        return float(sympy_expr)
    
    else:
        raise ValueError(f"Unsupported SymPy expression: {sympy_expr}")

def sympy_to_gurobi_complete(sympy_expr, symbol_map, model, aux_var_count=[0]):
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
        pass
    if isinstance(sympy_expr, sp.Symbol):
        return symbol_map[sympy_expr]
    
    elif isinstance(sympy_expr, sp.Add):
        return sum(sympy_to_gurobi_complete(arg, symbol_map, model, aux_var_count) for arg in sympy_expr.args)
    
    elif isinstance(sympy_expr, sp.Mul):
        result = 1
        for arg in sympy_expr.args:
            result *= sympy_to_gurobi_complete(arg, symbol_map, model, aux_var_count)
        return result
    
    elif isinstance(sympy_expr, sp.Pow):
        base, exp = sympy_expr.args
        
        # Check for special cases of exponents
        if exp == 0:
            # Any base to the power of 0 is 1
            return 1
        if exp == 1:
            # Any base to the power of 1 is the base itself
            return sympy_to_gurobi_complete(base, symbol_map, model, aux_var_count)


        # Always create an auxiliary variable for the base
        base_expr = sympy_to_gurobi_complete(base, symbol_map, model, aux_var_count)
        aux_var_name = f"pow_base_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        base_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        model.addConstr(base_aux_var == base_expr)
        
        if exp == 2:
            # Add a quadratic constraint for the square of the base
            aux_var_name = f"quad_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            quad_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
            model.addQConstr(quad_aux_var == base_aux_var * base_aux_var)
            return quad_aux_var

        if isinstance(exp, sp.Number):
            # Handle non-quadratic powers using general constraints
            exp_value = float(exp)
            aux_var_name = f"pow_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            pow_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
            model.addGenConstrPow(base_aux_var, pow_aux_var, exp_value)
            return pow_aux_var
        else:
            # Handle symbolic exponents using logarithms
            aux_var_name = f"pow_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            pow_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)

            # Create an auxiliary variable for the base
            base_aux_var_name = f"base_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            base_aux_var = model.addVar(name=base_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addConstr(base_aux_var == base_expr)

            # Convert the exponent to a Gurobi expression
            exp_expr = sympy_to_gurobi_complete(exp, symbol_map, model, aux_var_count)

            # Create auxiliary variable for log(base)
            log_base_aux_var_name = f"log_base_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            log_base_aux_var = model.addVar(name=log_base_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addGenConstrLog(base_aux_var, log_base_aux_var)


            # Create auxiliary variable for exp * log(base)
            prod_aux_var_name = f"prod_aux_{aux_var_count[0]}"
            aux_var_count[0] += 1
            prod_aux_var = model.addVar(name=prod_aux_var_name, vtype=GRB.CONTINUOUS)
            model.addConstr(prod_aux_var == exp_expr * log_base_aux_var)


            # Add exponential constraint for exp(prod_aux_var)
            model.addGenConstrExp(prod_aux_var, pow_aux_var)

            return pow_aux_var
        
    
    elif isinstance(sympy_expr, sp.exp):
        arg_expr = sympy_to_gurobi_complete(sympy_expr.args[0], symbol_map, model, aux_var_count)
        aux_var_name = f"exp_aux_{aux_var_count[0]}"
        aux_var_count[0] += 1
        arg_aux_var = model.addVar(name=f"aux_{aux_var_name}_arg", lb=0, vtype=GRB.CONTINUOUS)
        model.addConstr(arg_aux_var == arg_expr)
        exp_aux_var = model.addVar(name=aux_var_name, vtype=GRB.CONTINUOUS)
        model.addGenConstrExp(arg_aux_var, exp_aux_var)
        return exp_aux_var
    
    elif isinstance(sympy_expr, sp.log):
        arg_expr = sympy_to_gurobi_complete(sympy_expr.args[0], symbol_map, model, aux_var_count)
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
    def destroy_all(self):
        # Clean up firms
        for firm in self.firm_registry.values():
            firm.cleanup()
            del firm
        self.firm_registry.clear()

        # Clean up sectors
        for sector in self.sector_registry.values():
            sector.cleanup()
            del sector
        self.sector_registry.clear()

        # Clean up countries
        for country in self.country_registry.values():
            country.cleanup()
            del country
        self.country_registry.clear()

        # Reset other attributes
        self.BAU_emissions = 0
        self.BAU_profit = 0
        self.states = {
            "current": {},
            "best": {},
            "previous": {},
            "average_of_10_random": {}
        }

        print(f"{self.name} regulator destroyed")

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

    def find_optimal_permit_price_to_meet_the_emission_cap_requirements(self, precision = 0.1, permit_price_tolerance = 0.5, x_low = 0, x_high = 1000, size_of_diffs = 2):

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
            self.optimize_them_all(print_output=False, print_diff=True, precision = precision, size_of_diffs= size_of_diffs)
            total_emission = get_emission(self.firm_registry.values())
        print("Permit price: {} and total emission: {} and emission cap {}".format(self.permit_price, total_emission, self.emission_cap))
        return x_mid
    def optimization_with_least_squares(self, BAU = False, gurobi_print = False, lp_file = "least_squares.lp", print_output = False):
    
        m = Model("Least Squares")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        gurobi_output = {}
        gurobi_emission = {}
        
        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            
            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            xx_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"xx{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_emission[firm.id] = xx_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[x_sym] = xx_var
            
            m.addConstr(qq_var >= xx_var)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs({x: sympy_output[firm.id] - sympy_emission[firm.id], y: sympy_emission[firm.id]})
            firm_trading = -pp * (sympy_emission[firm.id] - sect.free_emission_multiplier * sympy_output[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            sympy_objective += sp.diff(firm_profit, sympy_output[firm.id])**2 + sp.diff(firm_profit, sympy_emission[firm.id])**2
            # print("Firm {} has profit: {}".format(firm.name, firm_profit))  
            # print("FOD: {}".format(sp.diff(firm_profit, sympy_output[firm.id])))
            # print("FED: {}".format(sp.diff(firm_profit, sympy_emission[firm.id])))
        sympy_objective += (self.emission_cap - sum(sympy_emission.values()))**2
        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))

        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write("least_squares.lp")
        m.optimize()

        if m.status == gb.GRB.OPTIMAL:
            print("Optimal solution found")
        else:
            print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_emission[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m
    
    def optimization_with_positive_constaints(self, BAU = False, gurobi_print = True, lp_file = "optimization_with_positive_constaints.lp", print_output = True):
    
        m = Model("Positive Constraints")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        gurobi_output = {}
        gurobi_emission = {}
        
        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            
            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            xx_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"xx{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_emission[firm.id] = xx_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[x_sym] = xx_var
            
            m.addConstr(qq_var >= xx_var)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs({x: sympy_output[firm.id] - sympy_emission[firm.id], y: sympy_emission[firm.id]})
            firm_trading = -pp * (sympy_emission[firm.id] - sect.free_emission_multiplier * sympy_output[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            profit_dq = sp.diff(firm_profit, sympy_output[firm.id])
            profit_dx = sp.diff(firm_profit, sympy_emission[firm.id])
            sympy_objective += profit_dq + profit_dx
            m.addConstr(sympy_to_gurobi(profit_dq, symbol_map, m) >= 0)
            m.addConstr(sympy_to_gurobi(profit_dx, symbol_map, m) >= 0)
        sympy_objective += (self.emission_cap - sum(sympy_emission.values()))**2


        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))

        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write("least_squares.lp")
        m.optimize()

        if m.status == gb.GRB.OPTIMAL:
            print("Optimal solution found")
        else:
            print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_emission[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m
    
    def optimization_with_least_squares_ab(self, BAU = False, gurobi_print = False, lp_file = "least_squares_e.lp", print_output = False):
    
        m = Model("Least Squares e")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        sympy_abatement = {}
        gurobi_output = {}
        gurobi_abatement = {}
        
        # ab = abatement ab = q - x (output - emission)

        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            ab_sym = sp.symbols(f"ab{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            sympy_abatement[firm.id] = ab_sym
            


            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            ab_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"ab{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_abatement[firm.id] = ab_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[ab_sym] = ab_var
            
            m.addConstr(qq_var >= ab_var)
            m.addConstr(ab_var >= 0)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs(x, sympy_abatement[firm.id])
            firm_trading = -pp * ((1 - sect.free_emission_multiplier) * sympy_output[firm.id] - sympy_abatement[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            profit_dq = sp.diff(firm_profit, sympy_output[firm.id])
            profit_dab = sp.diff(firm_profit, sympy_abatement[firm.id])
            sympy_objective += profit_dq **2 + profit_dab **2
            # m.addConstr(sympy_to_gurobi(profit_dq, symbol_map, m) >= 0)
            # m.addConstr(sympy_to_gurobi(profit_dab, symbol_map, m) >= 0)
        # sympy_objective += (self.emission_cap + sum(sympy_output.values())- sum(sympy_abatement.values()))**2
        m.addConstr(sum(gurobi_output.values())- sum(gurobi_abatement.values()) == self.emission_cap)

        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))

        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write("Least Squares e.lp")
        m.optimize()

        if m.status == gb.GRB.OPTIMAL:
            print("Optimal solution found")
        else:
            print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_output[firm.id].X - gurobi_abatement[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m
    
    def optimization_with_positive_constraints_ab(self, BAU = False, gurobi_print = False, lp_file = "positive_constraints_ab.lp", print_output = False):
    
        m = Model("positive_constraints_ab")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        sympy_abatement = {}
        gurobi_output = {}
        gurobi_abatement = {}
        
        # ab = abatement ab = q - x (output - emission)

        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            ab_sym = sp.symbols(f"ab{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            sympy_abatement[firm.id] = ab_sym
            


            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            ab_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"ab{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_abatement[firm.id] = ab_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[ab_sym] = ab_var
            
            m.addConstr(qq_var >= ab_var)
            m.addConstr(ab_var >= 0)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs(x, sympy_abatement[firm.id])
            firm_trading = -pp * ((1 - sect.free_emission_multiplier) * sympy_output[firm.id] - sympy_abatement[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            profit_dq = sp.diff(firm_profit, sympy_output[firm.id])
            profit_dab = sp.diff(firm_profit, sympy_abatement[firm.id])
            sympy_objective += profit_dq + profit_dab
            m.addConstr(sympy_to_gurobi(profit_dq, symbol_map, m) >= 0)
            m.addConstr(sympy_to_gurobi(profit_dab, symbol_map, m) >= 0)
        # sympy_objective += (self.emission_cap + sum(sympy_output.values())- sum(sympy_abatement.values()))**2
        m.addConstr(sum(gurobi_output.values())- sum(gurobi_abatement.values()) == self.emission_cap)

        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))

        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write("positive_constraints_ab.lp")
        m.optimize()

        if m.status == gb.GRB.OPTIMAL:
            print("Optimal solution found")
        else:
            print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_output[firm.id].X - gurobi_abatement[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m

    def optimization_everything_constrained_and_ab(self, BAU = False, gurobi_print = True, lp_file = "everything_constrained_and_ab.lp", print_output = True):
    
        m = Model("everything_constrained_and_ab")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        sympy_abatement = {}
        gurobi_output = {}
        gurobi_abatement = {}
        
        # ab = abatement ab = q - x (output - emission)

        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            ab_sym = sp.symbols(f"ab{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            sympy_abatement[firm.id] = ab_sym
            


            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            ab_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"ab{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_abatement[firm.id] = ab_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[ab_sym] = ab_var
            
            m.addConstr(qq_var >= ab_var)
            m.addConstr(ab_var >= 0)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs(x, sympy_abatement[firm.id])
            firm_trading = -pp * ((1 - sect.free_emission_multiplier) * sympy_output[firm.id] - sympy_abatement[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            profit_dq = sp.diff(firm_profit, sympy_output[firm.id])
            profit_dab = sp.diff(firm_profit, sympy_abatement[firm.id])
            # sympy_objective += profit_dq + profit_dab
            sympy_objective = profit_dq
            m.addConstr(sympy_to_gurobi(profit_dq, symbol_map, m) >= 0)
            m.addConstr(sympy_to_gurobi(profit_dab, symbol_map, m) == 0)
        # sympy_objective += (self.emission_cap + sum(sympy_output.values())- sum(sympy_abatement.values()))**2
        m.addConstr(sum(gurobi_output.values())- sum(gurobi_abatement.values()) == self.emission_cap)

        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))

        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write(lp_file)
        m.optimize()

        if m.status == gb.GRB.OPTIMAL:
            print("Optimal solution found")
        else:
            print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_output[firm.id].X - gurobi_abatement[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m


    def optimization_concave_formulation_ab(self, BAU = False, success_print = False, gurobi_print = False, lp_file = "optimization_concave_formulation_ab.lp", print_output = False):
    
        m = Model("optimization_concave_formulation_ab")
        
        # Define one pair of output and emission for each firm for sympy and for gurobi and the dictionary of them
        symbol_map = {}
        sympy_output = {}
        sympy_emission = {}
        sympy_abatement = {}
        gurobi_output = {}
        gurobi_abatement = {}
        
        # ab = abatement ab = q - x (output - emission)

        for firm in self.firm_registry.values():
            q_sym = sp.symbols(f"q{firm.id}")
            x_sym = sp.symbols(f"x{firm.id}")
            ab_sym = sp.symbols(f"ab{firm.id}")
            sympy_output[firm.id] = q_sym
            sympy_emission[firm.id] = x_sym
            sympy_abatement[firm.id] = ab_sym
            


            qq_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"qq{firm.id}", lb=0)
            ab_var = m.addVar(vtype=gb.GRB.CONTINUOUS, name=f"ab{firm.id}", lb=0)
            gurobi_output[firm.id] = qq_var
            gurobi_abatement[firm.id] = ab_var
            
            symbol_map[q_sym] = qq_var
            symbol_map[ab_sym] = ab_var
            
            m.addConstr(qq_var >= ab_var)
            m.addConstr(ab_var >= 0)
        
        pp = sp.symbols('pp')  # Permit price
        ppp = m.addVar(vtype=gb.GRB.CONTINUOUS, name='ppp', lb=0)
        symbol_map[pp] = ppp
                
        # Define the objective function
        sympy_objective = 0
        for firm in self.firm_registry.values():
            firm_profit = 0
            sect = firm.sector
            sum_sector_outputs = 0
            for i in range(len(sect.firms)):
                sum_sector_outputs += sympy_output[sect.firms[i].id]
            firm_revenew = sect.price_demand_function.subs(x, sum_sector_outputs) * sympy_output[firm.id]
            firm_abatement = -firm.abatement_cost_function.subs(x, sympy_abatement[firm.id])
            firm_trading = -pp * ((1 - sect.free_emission_multiplier) * sympy_output[firm.id] - sympy_abatement[firm.id])
            if BAU:
                firm_abatement = 0
                firm_trading = 0
            firm_profit += firm_revenew + firm_abatement + firm_trading
            profit_dq = sp.diff(firm_profit, sympy_output[firm.id])
            profit_dab = sp.diff(firm_profit, sympy_abatement[firm.id])
            # sympy_objective += profit_dq + profit_dab
            sympy_objective += - profit_dq - profit_dab
            m.addConstr(sympy_to_gurobi(profit_dq, symbol_map, m) <= 0)
            m.addConstr(sympy_to_gurobi(profit_dab, symbol_map, m) <= 0)
        # sympy_objective += (self.emission_cap + sum(sympy_output.values())- sum(sympy_abatement.values()))**2
        m.addConstr(sum(gurobi_output.values())- sum(gurobi_abatement.values()) == self.emission_cap)

        
        # print("Sum sector outputs: {}".format(sum_sector_outputs))
        # print("Sympy Objective: {}".format(sympy_objective))
        gurobi_objective = sympy_to_gurobi(sympy_objective, symbol_map, m)
        m.setObjective(gurobi_objective, gb.GRB.MINIMIZE)
        m.params.OutputFlag = 1 if gurobi_print else 0
        m.write(lp_file)
        m.optimize()
        if success_print:
            if m.status == gb.GRB.OPTIMAL:
                print("Optimal solution found")
            else:
                print("No solution found")
        
        if BAU:
            for firm in self.firm_registry.values():
                firm.BAU_output = gurobi_output[firm.id].X
                firm.BAU_emission = gurobi_output[firm.id].X
            self.BAU_emissions = sum([firm.BAU_emission for firm in self.firm_registry.values()])
        else:
            for firm in self.firm_registry.values():
                firm.actual_output = gurobi_output[firm.id].X
                firm.emission = gurobi_output[firm.id].X - gurobi_abatement[firm.id].X
            self.permit_price = ppp.X
        if print_output:
            for firm in self.firm_registry.values():
                print(f"Firm {firm.name} has output {firm.actual_output} and emission {firm.emission}")
            print(f"Permit price: {ppp.X}")
        return m


    def equilibrium_tester(self, print_header = True, precision = 0.001, output = False, full_output = False):
        print_header = print_header and (full_output or output)
        x, y = sp.symbols('x y')
        q1, x1 = sp.symbols('q1 x1')
        firms_data = []
        for sector in self.sector_registry.values():
            sum_sector_outputs = 0
            for firm in sector.firms:
                sum_sector_outputs += firm.actual_output
            for firm in sector.firms:

                firm_revenew = sector.price_demand_function.subs(x, sum_sector_outputs - firm.actual_output + q1) * q1
                firm_abatement = -firm.abatement_cost_function.subs({x: q1 - x1, y: x1})
                firm_trading = -self.permit_price * (x1 - sector.free_emission_multiplier * q1)
                firm_profit = firm_revenew + firm_abatement + firm_trading
                cond1 = sp.diff(firm_profit, q1)
                cond2 = sp.diff(firm_profit, x1)
                cond3 = sp.diff(firm_profit, x1, 2)
                cond4 = sp.diff(firm_profit, q1, 2)
                # print("Firm {} has cond2: {}".format(firm.name, cond2))
                cond5 = cond3 * cond4 - (sp.diff(firm_profit, q1, x1))**2
                cond1 = cond1.subs({q1: firm.actual_output, x1: firm.emission}).evalf()
                cond2 = cond2.subs({q1: firm.actual_output, x1: firm.emission}).evalf()
                cond3 = cond3.subs({q1: firm.actual_output, x1: firm.emission}).evalf()
                cond4 = cond4.subs({q1: firm.actual_output, x1: firm.emission}).evalf()
                cond5 = cond5.subs({q1: firm.actual_output, x1: firm.emission}).evalf()
                firms_data.append([firm.name, cond1, cond2, cond3, cond4, cond5])
        max_FOC_1 = max([abs(firm_data[1]) for firm_data in firms_data])
        max_FOC_2 = max([abs(firm_data[2]) for firm_data in firms_data])
        max_SOC = max([firm_data[3] for firm_data in firms_data])
        max_SOC_2 = max([firm_data[4] for firm_data in firms_data])
        min_Hessian = min([firm_data[5] for firm_data in firms_data])
        firms_data.insert(0, ["Worst", max_FOC_1, max_FOC_2, max_SOC, max_SOC_2, min_Hessian])
        worst_value = max(max_FOC_1, max_FOC_2)

        if output and not full_output:
            firms_data = firms_data[0:1]
            # print(firms_data)
        if print_header:
            # Print table header
            print(f"{'Firm':<10} | {'FOC 1':<12} | {'FOC 2':<13} | {'SOC 1':<13} | {'SOC 2':<12} | {'Hessian':<12} | Status")
            print("-" * 85)
        if full_output or output:
                # Iterate through each firm's data and print the status
            for firm_data in firms_data:
                firm_name, cond1, cond2, cond3, cond4, cond5 = firm_data
                
                # Check each condition and get the status (✔️ or ⚠️)
                cond1_status, cond1_ok = eq_check_condition(cond1, "zero", precision)
                cond2_status, cond2_ok = eq_check_condition(cond2, "zero", precision)
                cond3_status, cond3_ok = eq_check_condition(cond3, "negative")
                cond4_status, cond4_ok = eq_check_condition(cond4, "negative")
                cond5_status, cond5_ok = eq_check_condition(cond5, "positive")
                
                # Calculate how many conditions are OK
                conditions_ok = sum([cond1_ok, cond2_ok, cond3_ok, cond4_ok, cond5_ok])
                
                # Format each condition with scientific notation where necessary
                print(f"{firm_name:<10} | {eq_format_number(cond1)} {cond1_status} | {eq_format_number(cond2)} {cond2_status} | "
                    f"{eq_format_number(cond3)} {cond3_status} | {eq_format_number(cond4)} {cond4_status} | "
                    f"{eq_format_number(cond5)} {cond5_status} | {conditions_ok}/5")
        
        return abs(firms_data[0][1])<precision and abs(firms_data[0][2])<precision and firms_data[0][3]<0 and firms_data[0][4]<0 and firms_data[0][5]>0, worst_value



            




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

    def cleanup(self):
        # Custom cleanup logic for Sector
        print(f"Cleaning up sector {self.name}")

    def add_firm(self, firm):
        self.firms.append(firm)

    def __repr__(self):
        return f"Sector(id={self.id}, name='{self.name}', firms={len(self.firms)})"

    def get_consumer_surplus(self):
        sum_of_production = 0
        for firm in self.firms:
            sum_of_production += firm.actual_output
        return sp.integrate(self.price_demand_function, (x, 0, sum_of_production)) - sum_of_production * self.price_demand_function.subs(x, sum_of_production).evalf()

class Country:
    _id_counter = 1

    def __init__(self, name, size,regulator, GDP_per_capita = 0, population = 0, industry_percentage =0):
        self.id = Country._id_counter
        Country._id_counter += 1
        self.name = name
        self.size = size
        self.GDP_per_capita = GDP_per_capita
        self.population = population
        self.industry_percentage = industry_percentage
        self.GDP = GDP_per_capita * population
        self.firms = []  # List to store firms in this country

        # Register this country in the global registry
        regulator.country_registry[self.id] = self

    def cleanup(self):
        # Custom cleanup logic for Country
        print(f"Cleaning up country {self.name}")

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
        self.abatement = 0
        self.permits_costs = 0
        self.sales = 0
        self.permits_used = 0
        self.free_permits = 0
        self.permits_bought = 0

        # Register this firm in the global registry
        regulator.firm_registry[self.id] = self
        
        # Register this firm with its sector and country
        if self.sector:
            self.sector.add_firm(self)
        if self.country:
            self.country.add_firm(self)

    def cleanup(self):
        # Custom cleanup logic for Firm
        print(f"Cleaning up firm {self.name}")

    def __repr__(self):
        return f"Firm(id={self.id}, name='{self.name}', sector_id={self.sector.id if self.sector else None}, country_id={self.country.id if self.country else None}, actual_output={self.actual_output}, emission={self.emission}, profit={self.profit})"

    def calculate_profit_components(self):
        sector = self.sector
        regulator = self.regulator
        # Sum of all other outputs
        sum_other_outputs = 0
        for i in range(len(sector.firms)):
            if sector.firms[i].id != self.id:
                sum_other_outputs += sector.firms[i].actual_output
        
        permit_price = regulator.permit_price
        price_demand_function = sector.price_demand_function
        production_cost_function = self.production_cost_function
        abatement_cost_function = self.abatement_cost_function


        out, em = sp.symbols('out em')
        # Calculate the output of the firm
        income = (price_demand_function.subs(x, sum_other_outputs + out) - production_cost_function.subs(x, out))*out
        self.sales = income.subs(out, self.actual_output).evalf()
        self.abatement = abatement_cost_function.subs({x: self.actual_output - self.emission, y: self.emission}).evalf()
        self.permits_used = self.emission
        self.free_permits = sector.free_emission_multiplier * self.actual_output
        self.permits_bought = self.permits_used - self.free_permits
        self.permits_costs = permit_price * self.permits_bought
        self.profit = self.sales - self.abatement - self.permits_costs
        




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
        # return output.X, emission.X, profit.getValue()
        return output.X, emission.X, m.ObjVal
    
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

    