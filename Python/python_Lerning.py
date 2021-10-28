



#################################################
### Python Directory and Files Management
#################################################

import os
## Get Current Directory
os.getcwd()
print(os.getcwd())

## Changing Directory
os.chdir('C:\\')

## List Directories and Files
os.listdir()

## Renaming a Directory or a File
os.rename('old.txt','new_one')


## Removing Directory or File
os.remove('old.txt')


https://www.w3school.com.cn/python/python_file_open.asp




f = open("C:\\Users\\zbai\\Desktop\\Python\\Python_for_Data_Analysis\\datasets\\babynames\\yob1880.txt", "r")
print(f.read())



script_dir = os.path.dirname(__file__) #<-- absolute dir the script is in
rel_path = "C:/Users/bai/Desktop/Python/Python for Data Analysis/datasets/babynames/yob1880.txt"
abs_file_path = os.path.join(script_dir, rel_path)



 

 




import pandas as pd # for data manipulation
import numpy as np # for data manipulation
import sklearn
from sklearn.linear_model import LinearRegression # to build a LR model for comparison
import plotly.graph_objects as go # for data visualization
import plotly.express as px # for data visualization 
import statsmodels.api as sm # to build a LOWESS model
from scipy.interpolate import interp1d # for interpolation of new data points
