import sympy as sp

class Sector:
    def __init__(self, pollution_rate_expr, demand_curve_expr, abatement_cost_expr, production_cost_expr):
        x = sp.symbols('x')
        self.pollution_rate_expr = pollution_rate_expr
        self.demand_curve_expr = demand_curve_expr
        self.abatement_cost_expr = abatement_cost_expr
        self.production_cost_expr = production_cost_expr


        self.pollution_rate_func = sp.lambdify(x, pollution_rate_expr, 'numpy')
        self.demand_curve_func = sp.lambdify(x, demand_curve_expr, 'numpy')
        self.abatement_cost_func = sp.lambdify(x, abatement_cost_expr, 'numpy')
        self.production_cost_func = sp.lambdify(x, production_cost_expr, 'numpy')

        self.product_price = 1

        def calculate_product_price():
            # Calculate the product price based on the demand curve of all the companies on that sector
            pass


class Firm:
    def __init__(self, sector, country, production_output, custom_exprs=None):
        # production_output q_i
        # emissions = x_i
        # free_allocation = \Phi(q_i)
        self.sector = sector
        self.country = country
        self.production_output = production_output
        self.free_allocation = 0
        self.emissions = 0



        x = sp.symbols('x')

        # Setting up default functions from sector or overriding with custom expressions
        if custom_exprs:
            self.pollution_rate_expr = custom_exprs.get('pollution_rate', sector.pollution_rate_expr)
            self.demand_curve_expr = custom_exprs.get('demand_curve', sector.demand_curve_expr)
            self.abatement_cost_expr = custom_exprs.get('abatement_cost', sector.abatement_cost_expr)
            self.production_cost_expr = custom_exprs.get('production_cost', sector.production_cost_expr)
        else:
            self.pollution_rate_expr = sector.pollution_rate_expr
            self.demand_curve_expr = sector.demand_curve_expr
            self.abatement_cost_expr = sector.abatement_cost_expr
            self.production_cost_expr = sector.production_cost_expr

        # Lambdifying expressions for computational use
        self.pollution_rate_func = sp.lambdify(x, self.pollution_rate_expr, 'numpy')
        self.demand_curve_func = sp.lambdify(x, self.demand_curve_expr, 'numpy')
        self.abatement_cost_func = sp.lambdify(x, self.abatement_cost_expr, 'numpy')
        self.production_cost_func = sp.lambdify(x, self.production_cost_expr, 'numpy')

    def calculate_emissions(self):
        return self.pollution_rate_func(self.production_output)

    def calculate_profit(self, market_permit_price):
        # Calculating revenue based on market demand and current output
        revenue = self.demand_curve_func(self.production_output) * self.production_output
        # Cost of production
        production_cost = self.production_cost_func(self.production_output)
        # Abatement costs to reduce emissions to current level
        abatement_cost = self.abatement_cost_func(self.production_output - self.emissions)
        # Cost or gain from buying or selling permits
        permit_cost = market_permit_price * (self.emissions - self.free_allocation)

        # Total profit calculation
        self.profit = revenue - production_cost - abatement_cost - permit_cost
        return self.profit


    def make_production_decision(self):
        # Make production decision based on profit
        pass

class Market:
    def __init__(self):
        self.companies = []
        self.sectors = []
        self.market_prices = {}
        permmit_price = 0

    def add_firm(self, firm):
        self.companies.append(firm)
        self.update_sectors()

    def update_sectors(self):
        self.sectors = list(set([firm.sector for firm in self.companies]))



    def calculate_market_emissions(self):
        for sector in self.sectors:
            emissions = sum([firm.calculate_emissions() for firm in self.companies if firm.sector == sector])
            print(f"Total emissions for {sector.__class__.__name__}: {emissions}")
    
    def calculate_market_prices(self):
        for sector in self.sectors:
            x = sp.symbols('x')
            demand_curve_expr = [firm.demand_curve_expr for firm in self.companies if firm.sector == sector][0]
            demand_curve_func = sp.lambdify(x, demand_curve_expr, 'numpy')
            price = sp.solve(demand_curve_expr, x)[0]
            self.market_prices[sector] = price

    def calculate_permit_price(self):
        # Calculate the permit price based on the market emissions
        pass

    def update_market_conditions(self):
        # Sequentially update market and permit prices and notify firms to adjust their outputs and emissions.

    def run_optimization(self):
        # Define the objective function or constraints based on market needs
        pass

    def show_companies(self):
        """
        Display basic information about each firm in the market.
        """
        print("Market Companies Overview:")
        for firm in self.companies:
            print(f"Firm linked to {firm.country.name}, Sector: {firm.sector.__class__.__name__}, Production Level: {firm.production_output}")

    def show_market_details(self):
            """
            Display detailed information about the market, including each firm and the economic functions they use.
            """
            print("Detailed Market Description:")
            for idx, firm in enumerate(self.companies):
                print(f"\nFirm {idx + 1} - Country: {firm.country.name}")
                print(f"  Sector: {firm.sector.__class__.__name__}")
                print(f"  Production Level: {firm.production_output}")
                print("  Economic Functions:")
                print(f"    Pollution Rate Function: {sp.pretty(firm.pollution_rate_expr)}")
                print(f"    Demand Curve Function: {sp.pretty(firm.demand_curve_expr)}")
                print(f"    Abatement Cost Function: {sp.pretty(firm.abatement_cost_expr)}")
                print(f"    Production Cost Function: {sp.pretty(firm.production_cost_expr)}")


class Country:
    def __init__(self, name):
        self.name = name

class Regulator:
    def __init__(self):
        self.permit_allocation_strategy = None

    def allocate_permits(self):
        # Allocate permits based on the strategy
        pass
    def adjust_permit_policy(self):
        # Adjust permit policy based on market conditions
        pass



# Example usage
x = sp.symbols('x')
steel_sector = Sector(pollution_rate_expr=x**2, demand_curve_expr=-x + 50, abatement_cost_expr=2*x, production_cost_expr=x**2 + x + 1)
usa = Country("USA")
steel_firm = Firm(sector=steel_sector, country=usa)
market = Market()
market.add_firm(steel_firm)
market.run_optimization()
market.show_companies()
market.show_market_details()