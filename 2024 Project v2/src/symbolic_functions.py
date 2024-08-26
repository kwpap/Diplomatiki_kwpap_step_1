# Symbolic functions for the project

import sympy as sp

x = sp.symbols('x')
function_pool = [
    Add(Pow(Symbol('x'), Integer(2)), Float('0.5', precision=53)),
    Mul(Integer(15), log(Symbol('x'))),
    sin(Symbol('x')),
    exp(Symbol('x')),
    Add(Pow(Symbol('x'), Integer(3)), Mul(Integer(-1), Integer(2), Symbol('x')), Integer(1)),
]
