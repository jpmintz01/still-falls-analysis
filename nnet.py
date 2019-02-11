# neural net for peace-war
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

data = pd.read_csv('pw_all_data_with_demo', header = True) #read from dataset

X = data.iloc[:,0] # read first column
y = data.iloc[:,1] # read second column
m = len(y) # number of training example
data.head() # view first few rows of the data