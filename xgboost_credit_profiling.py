from xgboost.sklearn import XGBRFClassifier
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.cluster import AgglomerativeClustering
from xgboost import XGBClassifier
from sklearn.metrics import accuracy_score
from sklearn.model_selection import train_test_split

def find_missing(df):
  return round(df.isnull().sum() / df.shape[0] * 100, 1)


def show_missing(df, colname):
  return df[df[colname].isnull()].index


def replace_missing(df, colname, idx, replacement):
  df.loc[idx, colname] = df.loc[idx, colname].fillna(replacement)
  return df


def replace_revolver(df, colname, threshold):
  for i in threshold:
    df.loc[df[colname] > i, colname] = df.loc[df[colname] > i, colname] / (i * 10)
  return df


def aggregate_by_outcome(df, colname):
  unique_vals = np.sort(df[colname].unique())
  i_val       = []
  non_default = []
  default     = []
  for uv in unique_vals:
    counts = (df[df[colname] == uv]).groupby(['SeriousDlqin2yrs']).size()
    non_deflt = 0
    yes_deflt = 0
    if 0 in counts.index:
      non_deflt = counts[0]
    if 1 in counts.index:
      yes_deflt = counts[1]
    i_val.append(uv)
    non_default.append(non_deflt)
    default.append(yes_deflt)
  return i_val, non_default, default


def compute_cond_prob(df, colname):
  i_val, good, flawed = aggregate_by_outcome(df, colname)
  tmp = pd.DataFrame({'LateVal': i_val, 'Good': good, 'Flawed': flawed})
  tmp['Total'] = tmp['Good'] + tmp['Flawed']
  px  = tmp['Total'] / tmp['Total'].sum()
  pyx = tmp['Flawed'] / tmp['Total']
  py  = np.sum(pyx * px)
  pxy = (pyx * px) / py
  res = pd.DataFrame({'LateDays': i_val, 'Prob': pxy})
  return res


def assign_labels(df, colname):
  probs = compute_cond_prob(df, colname)
  arr = probs.Prob.to_numpy()
  arr = arr.reshape(len(arr), 1)
  clusters1 = AgglomerativeClustering(n_clusters = 3, linkage = "ward").fit(arr)
  label = clusters1.labels_
  probs['Label'] = label
  return probs


def iterate_labels(df):
  colnames = ['NumberOfTime30-59DaysPastDueNotWorse',
              'NumberOfTimes90DaysLate',
              'NumberOfTime60-89DaysPastDueNotWorse']
  list_of_labels = []
  for i in colnames:
    local_labels = assign_labels(df, i)
    list_of_labels.append(local_labels)
  return list_of_labels


def replace_with_labels(df, colnames, labels):
  for i in np.arange(len(colnames)):
    local_labels  = labels[i]
    local_labels  = local_labels.drop('Prob', axis = 1)
    local_colname = colnames[i]
    label_colname = local_colname + "_Label"
    local_labels  = local_labels.rename(columns = {"LateDays": local_colname,
                                                   "Label": label_colname})
    df = pd.merge(df, local_labels, on = local_colname, how = "left")
  return df


def make_dummies(df, replace_colnames):
  new_colnames = [i + "_Label" for i in replace_colnames]
  for colname in new_colnames:
    tmp = pd.get_dummies(df[colname], prefix = colname)
    df  = df.join(tmp)
  df = df.drop(new_colnames, axis = 1)
  return df


train = pd.read_csv('cs-training.csv').drop(['Unnamed: 0'], axis = 1)
test  = pd.read_csv('cs-test.csv').drop(['Unnamed: 0'], axis = 1)
# train.duplicated().sum()
# train.isnull().sum()
# find_missing(train)
# show_missing(train, 'NumberOfDependents')
# train['MonthlyIncome'].agg(['median']) # Median: 5400
train = train.drop_duplicates()
dependents_idx = show_missing(train, 'NumberOfDependents')
income_idx     = show_missing(train, 'MonthlyIncome')
train = replace_missing(train, 'NumberOfDependents', dependents_idx, 0)
train = replace_missing(train, 'MonthlyIncome', income_idx, 5400)

# In revolving credit 97.7% of data is below 1.00, the rest represent outliers
# Do not remove the outliers as they represent an important part of defaulters
# train['RevolvingUtilizationOfUnsecuredLines'].describe()
# train['RevolvingUtilizationOfUnsecuredLines'].quantile([0.978])
# (train[train['RevolvingUtilizationOfUnsecuredLines'] > 1]).groupby(['SeriousDlqin2yrs']).size()
train = replace_revolver(train, 'RevolvingUtilizationOfUnsecuredLines', [10000, 1000, 100, 10, 1])

# All 3 variables describing late payments have the same error in 96 and 98 times
# A good idea here maybe to split the data into windows of data:
# like 0 -- window 1, [1, 2, 3] -- window 2 etc.
# Since the count of groups is different for every variable, create the windows
# separately for each of them.
# train.groupby(['NumberOfTime30-59DaysPastDueNotWorse']).size()
# train.groupby(['NumberOfTimes90DaysLate']).size()
# train.groupby(['NumberOfTime60-89DaysPastDueNotWorse']).size()
# Save labels for each columns for later since we need them to convert test
# dataset to the same format using the same labels
train_labels = iterate_labels(train)
replace_colnames = ['NumberOfTime30-59DaysPastDueNotWorse',
                    'NumberOfTimes90DaysLate',
                    'NumberOfTime60-89DaysPastDueNotWorse']
train = replace_with_labels(train, replace_colnames, train_labels)
train = train.drop(replace_colnames, axis = 1)
train = make_dummies(train, replace_colnames)

# Debt ratio must be between 0 and 1 as it is a percentage of monthly income
# and it can't be greater than 1 since a person is not able to pay more than
# their income.
# Replace all values greated than 1 with median value that was computed without
# outliers and wrong numbers. The impact from replacement will not be negative
# since the change in the default rate is not significant.
# (train[train['DebtRatio'] > 1]).groupby(['SeriousDlqin2yrs']).size() / train[train['DebtRatio'] > 1].shape[0]
# (train[train['DebtRatio'] <= 1]).groupby(['SeriousDlqin2yrs']).size() / train[train['DebtRatio'] <= 1].shape[0]
# train.groupby(['SeriousDlqin2yrs']).size() / train.shape[0]
debt_ratio_med = train.loc[train['DebtRatio'] <= 1, 'DebtRatio'].agg('median')
train.loc[train['DebtRatio'] > 1, 'DebtRatio'] = debt_ratio_med

# Convert Number of Dependents to windowed version using this split for windows
# based on the probabilities of deault for each value:
# 0 -- group0, 1 -- group1, 2:3 -- group2, 4+ -- group3
# compute_cond_prob(train, 'NumberOfDependents')
train['DepNum'] = 3
train.loc[train['NumberOfDependents'] == 0, 'DepNum'] = 0
train.loc[train['NumberOfDependents'] == 1, 'DepNum'] = 1
train.loc[train['NumberOfDependents'] == 2, 'DepNum'] = 2
train.loc[train['NumberOfDependents'] == 3, 'DepNum'] = 3
tmp = pd.get_dummies(train.DepNum, prefix = "DepNum_")
train = train.join(tmp)
train = train.drop('NumberOfDependents', axis = 1)
train = train.drop('DepNum', axis = 1)

# Extract validation data from train data to be able to check overfitting and
# use it to run genetic algorithm for paramter search
X = train.drop(['SeriousDlqin2yrs'], axis=1)
y = train['SeriousDlqin2yrs']
X_train, X_val, y_train, y_val = train_test_split(X, y, test_size = 0.10, random_state = 42)

# Non-machine learning baseline shows that 93.3% is non-default in training
# and 93.0% is non-default in validation set
# round(1 - np.sum(y_train) / y_train.shape[0], 3) * 100
# round(1 - np.sum(y_val) / y_val.shape[0], 3) * 100

# Basic XGBoost model without tuning shows accuracy of:
# training   = 93.8% +0.5% to baseline
# validation = 93.4% +0.4% to baseline
model = XGBRFClassifier(tree_method="exact", booster="gbtree", eval_metric="auc")
model.fit(X_train, y_train.values.ravel())
y_pred = model.predict(X_train)
print("Model accuracy = {}%".format(round(accuracy_score(y_train, y_pred) * 100, 1)))
y_val_test = model.predict(X_val)
print("Model accuracy = {}%".format(round(accuracy_score(y_val, y_val_test) * 100, 1)))
