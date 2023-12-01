import pandas as pd
import numpy as np
import os
import os.path
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import MinMaxScaler
from mapie.classification import MapieClassifier
from mapie.metrics import classification_coverage_score
from mapie.metrics import classification_mean_width_score


# Function to fit multiple models.
def fit_models(X_train, y_train, random_seed):
    classifiers = []

    model_bayes = GaussianNB().fit(X_train, y_train)
    classifiers.append(("Naive Bayes",model_bayes))

    model_rf = RandomForestClassifier(n_estimators=50,random_state=random_seed).fit(X_train, y_train)
    classifiers.append(("RF",model_rf))
    
    model_svm = SVC(probability=True,random_state=random_seed).fit(X_train, y_train)
    classifiers.append(("SVM",model_svm))
    
    model_knn = KNeighborsClassifier(3).fit(X_train, y_train)
    classifiers.append(("KNN",model_knn))
    
    return classifiers



# Function that saves a results data frame in the given folder.
def save_df(df, dataset_path, model_type, filename):
    
    os.chdir(".")
    #print("current dir is: %s" % (os.getcwd()))
    
    tmpdir = dataset_path+"results_"+model_type+"/"
    
    if os.path.isdir(tmpdir) == False:
        os.mkdir(tmpdir)
    
    df.to_csv(tmpdir+"results.csv", index=False)
	

    
# Function that creates a folder to store general results.
def create_results_dir(dataset_path):
    
    os.chdir(".")
    
    tmpdir = dataset_path+"results_all/"
    
    if os.path.isdir(tmpdir) == False:
        os.mkdir(tmpdir)

# Compute p-values for each class as described in https://cml.rhul.ac.uk/cp.html
def compute_pvalues(calib_scores, test_scores):
    
    n = len(calib_scores)

    # Create array to store p-values
    pvalues = np.zeros(test_scores.shape)

    for r in range(test_scores.shape[0]):
        for c in range(test_scores.shape[1]):
            alpha_j = test_scores[r,c]
            pval = (sum(calib_scores >= alpha_j) + 1) / (n + 1)
            pvalues[r,c] = pval
    
    return(pvalues)

# Function that saves the classes order that correspond to the pvalues.
def save_classes_order(df, dataset_path):
    
    os.chdir(".")
    
    df.to_csv(dataset_path+"classes.csv", index=False, header=False)
