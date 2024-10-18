import sys
import os
# Get the absolute path of the current script
current_dir = os.path.dirname(os.path.abspath(__file__))

# Add the parent directory of 'src' to the sys.path
sys.path.append(os.path.abspath(os.path.join(current_dir, '..', 'src')))

# Construct the absolute path to the CSV file
csv_file_path = os.path.join(current_dir, '..', 'data', 'generated', 'exp04_Cap_vs_permit_price.csv')

# Construct the absolute path to the log file
log_file_path = os.path.join(current_dir, 'experiment.log')

from funkwpap import *
import sympy as sp, pandas as pd, numpy as np, tqdm, time, sys, matplotlib.pyplot as plt
from scipy.optimize import fsolve
from random import random
from scipy.optimize import minimize
from gurobipy import *
import signal, ast, concurrent.futures
import multiprocessing
import concurrent.futures
import signal
from contextlib import contextmanager
import logging

class TimeoutException(Exception): pass

@contextmanager
def time_limit(seconds):
    def signal_handler(signum, frame):
        raise TimeoutException("Timed out!")
    signal.signal(signal.SIGALRM, signal_handler)
    signal.alarm(seconds)
    try:
        yield
    finally:
        signal.alarm(0)



# Configure logging
logging.basicConfig(
    filename=log_file_path,
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
x = sp.symbols('x')
cap = 200 # total emission cap
Regulator9 = Regulator("test1", permit_price = 2.4079, emission_cap = cap)
sector1 = Sector('cement', price_demand_function= 100 - 0.1*x, free_emission_multiplier= 0, regulator= Regulator9)
sector2 = Sector('steel', price_demand_function=150 - 0.1*x, free_emission_multiplier= 0, regulator= Regulator9)
sector3 = Sector('paper', price_demand_function= 200 - 0.02*x**1.5, free_emission_multiplier= 0, regulator= Regulator9)
country1 = Country('DE', 1, regulator= Regulator9)
country2 = Country('FI', 0.5, regulator= Regulator9)
country3 = Country('GR', size= 0.1, regulator= Regulator9)

# Create Firms using objects
firm1 = Firm('firm1', 1, 1, x*0, 10*x+ 2*x**2 + 0.1*x**3 , 0, 0, 0, regulator= Regulator9)
firm2 = Firm('firm2', 1, 2, x*0, 11*x+ 3*x**2 + 0.2*x**3, 0, 0, 0, regulator= Regulator9)
firm3 = Firm('firm3', 1, 3, x*0, 5*x+ 4*x**2 + 5*x**3 , 0, 0, 0, regulator= Regulator9)
firm4 = Firm('firm4', 2, 1, x*0, 7*x+ 5*x**2 + 3*x**3 , 0, 0, 0, regulator= Regulator9)
firm5 = Firm('firm5', 2, 2, x*0, 1*x+ 6*x**2 + 2*x**3 , 0, 0, 0, regulator= Regulator9)
firm6 = Firm('firm6', 2, 3, x*0, 2*x+ 7*x**2 + 3*x**3 , 0, 0, 0, regulator= Regulator9)
firm7 = Firm('firm7', 3, 1, x*0, 3*x+ 8*x**2 + 4*x**3 , 0, 0, 0, regulator= Regulator9)
firm8 = Firm('firm8', 3, 2, x*0, 4*x+ 9*x**2 + 10*x**3 , 0, 0, 0, regulator= Regulator9)
firm9 = Firm('firm9', 3, 3, x*0, 5*x+ 10*x**2 + 11*x**3 , 0, 0, 0, regulator= Regulator9)


def optimization_with_least_squares_ab():
    Regulator9.optimization_with_least_squares_ab(gurobi_print=False, print_output=False)

def optimization_with_positive_constraints_ab():
    Regulator9.optimization_with_positive_constraints_ab(gurobi_print=False, print_output=False)

def optimization_with_positive_constraints():
    Regulator9.optimization_with_positive_constaints(gurobi_print=False, print_output=False)

def optimization_with_least_squares():
    Regulator9.optimization_with_least_squares(gurobi_print=False, print_output=False)

def optimize_them_all():
    Regulator9.optimize_them_all()
def optimization_concave_problem():
    Regulator9.optimization_concave_formulation_ab(gurobi_print=False, print_output=False)

# Load the data from the CSV file
df = pd.read_csv(csv_file_path)

# Initialize lists to store updated values
free_alloc = list(df['Free Allocation'])
caps2 = list(df['Emission Cap'])
permit_prices = list(df['Permit Price'])
outputs = [ast.literal_eval(output) for output in df['Outputs']]
emissions = [ast.literal_eval(emission) for emission in df['Emissions']]
profits = [ast.literal_eval(profit) for profit in df['Profits']]


# Iterate over each row in the DataFrame
for index, row in tqdm.tqdm(df.iterrows(), total=df.shape[0]):
    # Update the regulator and firms with data from the row
    Regulator9.emission_cap = row['Emission Cap']
    for firm, output, emission, profit in zip(Regulator9.firm_registry.values(), outputs[index], emissions[index], profits[index]):
        firm.actual_output = float(output)
        firm.emission = float(emission)

    for firm in Regulator9.firm_registry.values():
        firm.profit = firm.calculate_profit()
    # Run the equilibrium tester
    equilibrium_result, worst = Regulator9.equilibrium_tester(precision=1, full_output=False, output=True)
    print(f"Starting row {index} with emission cap {Regulator9.emission_cap} and old worst {worst}")
    logging.info(f"Starting row {index} with emission cap {Regulator9.emission_cap} and old worst {worst}")
    # Store the best result
    best_result = {
        'Emission Cap': Regulator9.emission_cap,
        'Permit Price': Regulator9.permit_price,
        'Outputs': tuple([firm.actual_output for firm in Regulator9.firm_registry.values()]),
        'Emissions': tuple([firm.emission for firm in Regulator9.firm_registry.values()]),
        'Profits': tuple([firm.profit for firm in Regulator9.firm_registry.values()]),
        'Worst': worst
    }


    for i in range(6):
        print(f"Running method {i}")
        if i == 0:
            try:
                with time_limit(60):
                    optimization_concave_problem()
            except TimeoutException as e:
                logging.info("Concave problem timed out! on row {index}")
                print("Timed out!")
                continue
        elif i == 1:
            try:
                with time_limit(60):
                    optimization_with_least_squares_ab()
            except TimeoutException as e:
                logging.info("least_squares_ab problem timed out! on row {index}")
                print("Timed out!")
                continue
        elif i == 2:
            try:
                with time_limit(60):
                    optimization_with_positive_constraints_ab()
            except TimeoutException as e:
                logging.info("least_squares_ab problem timed out! on row {index}")
                print("Timed out!")
                continue
        elif i == 3:
            try:    
                with time_limit(60):
                    optimization_with_positive_constraints()
            except TimeoutException as e:
                logging.info("least_squares_ab problem timed out! on row {index}")
                print("Timed out!")
                continue
        elif i == 4:
            try:
                with time_limit(60):
                    continue
                    # optimization_with_least_squares()
            except TimeoutException as e:
                logging.info("least_squares_ab problem timed out! on row {index}")
                print("Timed out!")
                continue
        elif i == 5:
            try:
                with time_limit(60):
                    optimize_them_all()
            except TimeoutException as e:
                logging.info("least_squares_ab problem timed out! on row {index}")
                print("Timed out!")
        else:
            continue


        equilibrium_result, new_worst = Regulator9.equilibrium_tester(precision=1, output=True)
        if new_worst < 0.001:
            logging.info(f"Using method {i}, the worst was updated finally with worst: {new_worst}.")
            break
        print(f"Method {i} finished with worst: {new_worst}")
        if new_worst < best_result['Worst']:
            logging.info(f"Using method {i}, the worst was updated from {best_result['Worst']} to {new_worst}.")
            best_result = {
                'Emission Cap': Regulator9.emission_cap,
                'Permit Price': Regulator9.permit_price,
                'Outputs': tuple([firm.actual_output for firm in Regulator9.firm_registry.values()]),
                'Emissions': tuple([firm.emission for firm in Regulator9.firm_registry.values()]),
                'Profits': tuple([firm.profit for firm in Regulator9.firm_registry.values()]),
                'Worst': new_worst
            }

    # Update the lists with the best values
    caps2[index] = best_result['Emission Cap']
    permit_prices[index] = best_result['Permit Price']
    outputs[index] = best_result['Outputs']
    emissions[index] = best_result['Emissions']
    profits[index] = best_result['Profits']

# Save the updated DataFrame to the CSV file
df = pd.DataFrame({
    'Free Allocation': free_alloc,
    'Emission Cap': caps2,
    'Permit Price': permit_prices,
    'Outputs': [str(output) for output in outputs],
    'Emissions': [str(emission) for emission in emissions],
    'Profits': [str(profit) for profit in profits]
})
df.to_csv(csv_file_path, index=False)
