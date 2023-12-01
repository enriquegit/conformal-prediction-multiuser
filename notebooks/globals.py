# File that contains global variables.

DATASET_NAME = "wisdm" # Directory name where the data.csv file is located.

PCT_TRAIN = 0.6 # Percent of training data.

PCT_CALIBRATION = 0.50 # From the remaining data what percent is for calibration.

PCT_TARGET_TEST = 0.5

ITERATIONS = 20

ALPHA = 0.05 # Maximum error.

DATA_PATH = "../data/" # Root path where all datasets reside.

DATASET_PATH = DATA_PATH + DATASET_NAME + "/"

FILE_PATH = DATA_PATH + DATASET_NAME + "/data.csv"
