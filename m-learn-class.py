#!/usr/bin/env python3
import pandas as pd
import pylab as pl
import numpy as np
import scipy.optimize as opt
from sklearn import preprocessing
import matplotlib.pyplot as plt
from sklearn.metrics import jaccard_score

from sklearn.metrics import classification_report, confusion_matrix
import itertools

# Algorithm 'KNN' or 'SVM'
algorithm='SVM'

# Subroutine for confusion matrix plotting
def plot_confusion_matrix(cm, classes,
                          normalize=False,
                          title='Confusion matrix',
                          cmap=plt.cm.Blues):
    """
    This function prints and plots the confusion matrix.
    Normalization can be applied by setting `normalize=True`.
    """
    if normalize:
        cm = cm.astype('float') / cm.sum(axis=1)[:, np.newaxis]
        print("Normalized confusion matrix")
    else:
        print('Confusion matrix, without normalization')

    print(cm)

    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)

    fmt = '.2f' if normalize else 'd'
    thresh = cm.max() / 2.
    for i, j in itertools.product(range(cm.shape[0]), range(cm.shape[1])):
        plt.text(j, i, format(cm[i, j], fmt),
                 horizontalalignment="center",
                 color="white" if cm[i, j] > thresh else "black")

    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')
    plt.show()


# Option to print out data frames
pd.set_option('display.max_rows', 1000)

# Loading data frames from csv files
qb_n_df = pd.read_csv("QBs-names.csv", index_col=0)
qb_df = pd.read_csv("QBs-train.csv", index_col=0)
qb_y_df = pd.read_csv("QBs-Y-train.csv", index_col=0)
qb_p_df = pd.read_csv("QBs-pred.csv", index_col=0)

# Following makes the QB names row indeces
names=[]
indeces=[]
for i in range(1,qb_n_df.shape[0]+1):
   names.append(qb_n_df.first_name[i]+" "+qb_n_df.last_name[i])
   indeces.append(i)

for j in range(1,qb_n_df.shape[0]+1):
   qb_df=qb_df.rename(index={indeces[j-1]:names[j-1]})
   qb_p_df=qb_p_df.rename(index={indeces[j-1]:names[j-1]})
# QB names are row indeces now


# Print out columns
print(qb_df.columns)

# Columns used in the analysis
cols=['G', 'GS', 'QBrec', 'Cmp', 'Att', 'Cmp%', 'Yds', 'TD', 'TD%', 'Int','Int%', 'Lng', 'Y/A', 'AY/A', 'Y/C', 'Y/G', 'Rate', 'Sk', 'Yds.1','NY/A', 'ANY/A', 'Sk%', 'AV']

# Career values of known careers (training set)
y = np.asarray(qb_y_df['x'])

# Next 6 lines guarantee that training and prediction sets are transformed with the same transormation
# We first remove NaNs from the data and replace them with 0
qb_df0 = qb_df[cols].fillna(0)
qb_p_df0 = qb_p_df[cols].fillna(0)
# Merging of training and prediction sets
pre_norm = pd.concat([qb_df0,qb_p_df0])

X_pre_norm = np.asarray(pre_norm)
# Transformation
X_after_norm = preprocessing.StandardScaler().fit(X_pre_norm).transform(X_pre_norm)

# Known careers QBs (training set)
X = X_after_norm[0:(qb_df0.shape[0])]
# Prediction set QBs (uknown careers)
X_p = X_after_norm[(qb_df0.shape[0]):(pre_norm.shape[0])]

# Print out dimensions of X and y
print ('X y shapes:', X.shape,  y.shape)

# Splitting X and y into (X,y)_train and (X,y)_test for evaluation
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split( X, y, test_size=0.3, random_state=4)
print ('Train set:', X_train.shape,  y_train.shape)
print ('Test set:', X_test.shape,  y_test.shape)

if algorithm=='KNN':
   # KNN classifier with k=9
   from sklearn.neighbors import KNeighborsClassifier
   k = 9
   # Fitting KNN to X_train and y_train
   LR = KNeighborsClassifier(n_neighbors = k).fit(X_train,y_train)
   # Fitting KNN to X and y
   LR_p = KNeighborsClassifier(n_neighbors = k).fit(X,y)

if algorithm=='SVM':
   # SVM classifier
   from sklearn import svm
   # 'linear', 'poly', 'rbf', 'sigmoid', 'precomputed'
   method='poly'
   # Fitting SVM to X_train and y_train
   LR = svm.SVC(kernel=method,C=0.25,probability=True).fit(X_train,y_train)
   # Fitting KNN to X and y
   LR_p = svm.SVC(kernel=method,C=0.25,probability=True).fit(X,y)
   # Calculating probabilities
   y_prob = LR_p.predict_proba(X_p)

# Prediction on X_test
yhat = LR.predict(X_test)
# Prediction on X_p (unknown careers)
y_pred = LR_p.predict(X_p)

# Printing unknown QBs with the prediction y_pred
print("Young QBs:")
if algorithm=='KNN':
   print(qb_p_df[cols].rename(columns={'Yds.1':'SkYds'}).assign(Career=y_pred))
if algorithm=='SVM':
   print((qb_p_df[cols].rename(columns={'Yds.1':'SkYds'}).assign(Career=y_pred)).assign(CareerProb=y_prob[:,1]))

print("\n",i)
# Jaccard score
print("\nJaccard score: ",jaccard_score(y_test, yhat))

# Classification report
print (classification_report(y_test, yhat))

# Compute confusion matrix
from sklearn.metrics import confusion_matrix
cnf_matrix = confusion_matrix(y_test, yhat, labels=[1,0])
np.set_printoptions(precision=2)

# Plot non-normalized confusion matrix
plt.figure()
plot_confusion_matrix(cnf_matrix, classes=['Career=1','Career=0'],normalize= False,  title='Confusion matrix')
