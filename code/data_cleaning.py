import pandas as pd
import numpy as np
import sklearn
from random import sample
from itertools import compress

raw_cars_data = pd.read_csv("https://raw.githubusercontent.com/NigelPetersen/STA2101-data/main/cleaned_cars.csv")
raw_cars_data = raw_cars_data.drop("location_region", axis = 1)
list(raw_cars_data["engine_fuel"].unique())
raw_cars_data["engine_fuel"] = raw_cars_data["engine_fuel"].replace("gas", "gasoline")

def get_proportions(var):
    d = {}
    for value in list(raw_cars_data[var].unique()):
        d[value] = [100*len(raw_cars_data.loc[raw_cars_data[var] == value])/len(raw_cars_data)]
    return pd.DataFrame(d)
get_proportions("engine_fuel")
raw_cars_data["engine_fuel"] = raw_cars_data["engine_fuel"].replace(["hybrid-petrol", "hybrid-diesel", "electric"], "other")
get_proportions("engine_type")
raw_cars_data = raw_cars_data.drop("engine_fuel", axis = 1)

raw_cars_data["body_type"] = raw_cars_data["body_type"].replace("cabriolet", "convertible")

