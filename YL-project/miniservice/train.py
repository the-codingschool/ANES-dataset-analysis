import sys
if len(sys.argv) == 1:
    print("ERROR: You need to specify the file to which to save the model!")

import pandas as pd
from sklearn import tree

# read csv
df = pd.read_csv("./../../data/anes_timeseries_2020.csv")

dfsubset = df[[
    "V201115",
    "V201121",
    "V201124",
    "V201130",
    "V201142",
    "V201136",
    "V201139",
    "V202143",
    "V202144",
    "V201324",
    "V201336"
]]
from sklearn import tree
from joblib import dump
X = dfsubset
Y = df["V201033"]
clf = tree.DecisionTreeClassifier()
clf = clf.fit(X, Y)

dump(clf, sys.argv[1])
