# neural net for peace-war
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv('pw_all_data_with_demo.csv') #read from dataset
data.head() # view first few rows of the data