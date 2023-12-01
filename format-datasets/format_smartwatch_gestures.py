import os
import numpy as np
import pandas as pd
from scipy.stats import pearsonr
from sklearn.metrics import auc


def extract_features(x, y, z):
    features = {}
    features["meanX"] = np.mean(x)
    features["meanY"] = np.mean(y)
    features["meanZ"] = np.mean(z)
    features["sdX"] = np.std(x)
    features["sdY"] = np.std(y)
    features["sdZ"] = np.std(z)
    features["maxX"] = np.max(x)
    features["maxY"] = np.max(y)
    features["maxZ"] = np.max(z)
    features["corXY"] = pearsonr(x, y)[0]
    features["corXZ"] = pearsonr(x, z)[0]
    features["corYZ"] = pearsonr(y, z)[0]
    magnitude = np.sqrt(x**2 + y**2 + z**2)
    features["meanMagnitude"] = np.mean(magnitude)
    features["sdMagnitude"] = np.std(magnitude)
    features["auc"] = auc(range(len(magnitude)), magnitude)
    features["meanDif"] = np.mean(np.diff(magnitude))
    x_diff = np.diff(x)
    y_diff = np.diff(y)
    z_diff = np.diff(z)
    features["meanX2"] = np.mean(x_diff)
    features["meanY2"] = np.mean(y_diff)
    features["meanZ2"] = np.mean(z_diff)
    features["sdX2"] = np.std(x_diff)
    features["sdY2"] = np.std(y_diff)
    features["sdZ2"] = np.std(z_diff)
    features["maxX2"] = np.max(x_diff)
    features["maxY2"] = np.max(y_diff)
    features["maxZ2"] = np.max(z_diff)
    features["corXY2"] = pearsonr(x_diff, y_diff)[0]
    features["corXZ2"] = pearsonr(x_diff, z_diff)[0]
    features["corYZ2"] = pearsonr(y_diff, z_diff)[0]
    magnitude_diff = np.sqrt(x_diff**2 + y_diff**2 + z_diff**2)
    features["meanMagnitude2"] = np.mean(magnitude_diff)
    features["sdMagnitude2"] = np.std(magnitude_diff)
    features["auc2"] = auc(range(len(magnitude_diff)), magnitude_diff)
    features["meanDif2"] = np.mean(np.diff(magnitude_diff))
    return features


# Update the paths below
base_dir = "/gestures-dataset"
csv_file_path = (
    "../smartwatch_gestures/data.csv"
)

features_data = []
feature_columns = [
    "userid",
    "label",
    "meanX",
    "meanY",
    "meanZ",
    "sdX",
    "sdY",
    "sdZ",
    "maxX",
    "maxY",
    "maxZ",
    "corXY",
    "corXZ",
    "corYZ",
    "meanMagnitude",
    "sdMagnitude",
    "auc",
    "meanDif",
    "meanX2",
    "meanY2",
    "meanZ2",
    "sdX2",
    "sdY2",
    "sdZ2",
    "maxX2",
    "maxY2",
    "maxZ2",
    "corXY2",
    "corXZ2",
    "corYZ2",
    "meanMagnitude2",
    "sdMagnitude2",
    "auc2",
    "meanDif2",
]

for user in range(1, 9):
    user_dir = os.path.join(base_dir, f"U{user:02d}")
    for gesture in range(1, 21):
        gesture_dir = os.path.join(user_dir, f"{gesture:02d}")
        for rep in range(1, 21):
            file_path = os.path.join(gesture_dir, f"{rep:02d}.txt")
            df = pd.read_csv(file_path, delimiter=" ", header=None)
            x, y, z = df.iloc[:, -3], df.iloc[:, -2], df.iloc[:, -1]
            features = extract_features(x, y, z)
            features["userid"] = user
            features["label"] = "n" + str(gesture)
            features_data.append(features)

features_df = pd.DataFrame(features_data, columns=feature_columns)
features_df.to_csv(csv_file_path, index=False)
