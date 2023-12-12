# Conformal Prediction in Multi-User Settings: An Evaluation

This is the code repository to reproduce the results of our paper ["Conformal Prediction in Multi-User Settings: An Evaluation"](https://arxiv.org/abs/2312.05195).

 To run the experiments:

 1. Download the desired dataset (links to download the datasets can be found in the paper).
 2. Use the appropiate script to format the downloaded dataset. The scripts to format the datasets are in the ``format-datasets/`` directory. The formated dataset will be saved in a file named ``data.csv``.
 3. Copy the resulting dataset into the ``data/`` directory in the corresponding subfolder.
 4. Set the name of the dataset you want to process. This is defined in the ``DATASET_NAME`` variable in ``notebooks/globals.py``. For example ``DATASET_NAME = "wisdm"``. The dataset name must coincide with the name of its subdirectory in ``data/``. 
 5. Run the cells of the jupyter notebook ``notebooks/run_all.ipynb``. This will run all the experiments for the given dataset.
 6. To generate the tables and plots, run the ``format-results/run_format.R`` script.
 7. Enjoy!

