import sys
import os
# Get the current directory of this script
current_dir = os.path.dirname(os.path.abspath(__file__))
# Construct the path to the src directory
src_path = os.path.abspath(os.path.join(current_dir, '..', 'src'))
# Append the src directory to the Python path
sys.path.append(src_path)
# Import funkwpap module
from funkwpap import *

@pytest.fixture
def regulator():
    cap = 200  # total emission cap
    return Regulator(name="Regulator19", permit_price=2.4079, emission_cap=cap)

@pytest.fixture
def sector1(regulator):
    x = sp.symbols('x')
    return Sector(
        name='cement',
        price_demand_function=100 - 0.1*x,
        free_emission_multiplier=0,
        regulator=regulator
    )

@pytest.fixture
def sector2(regulator):
    x = sp.symbols('x')
    return Sector(
        name='steel',
        price_demand_function=150 - 0.1*x,
        free_emission_multiplier=0,
        regulator=regulator
    )

@pytest.fixture
def country_gr(regulator):
    return Country(
        name='GR',
        size=0.1,
        regulator=regulator
    )

@pytest.fixture
def firms(regulator, sector1, sector2, country_gr):
    x = sp.symbols('x')
    firm1 = Firm('firm1', sector1, country_gr, x*0, 10*x + 2*x**2 + 0.1*x**3, 0, 0, 0, regulator)
    firm2 = Firm('firm2', sector1, country_gr, x*0, 11*x + 3*x**2 + 0.2*x**3, 0, 0, 0, regulator)
    firm3 = Firm('firm3', sector1, country_gr, x*0, 5*x + 4*x**2 + 5*x**3, 0, 0, 0, regulator)
    firm4 = Firm('firm4', sector2, country_gr, x*0, 7*x + 5*x**2 + 3*x**3, 0, 0, 0, regulator)
    firm5 = Firm('firm5', sector2, country_gr, x*0, 1*x + 6*x**2 + 2*x**3, 0, 0, 0, regulator)
    firm6 = Firm('firm6', sector2, country_gr, x*0, 2*x + 7*x**2 + 3*x**3, 0, 0, 0, regulator)
    return [firm1, firm2, firm3, firm4, firm5, firm6]