# Nonparametric Analysis of Clinical Indicators for Bloodstream Infection Detection
### Research Question
Is it possible to create an accurate prediction model for the presence of bacteremia using blood test results?
### Dataset
Bacteremia Dataset from Vienna General Hospital, Austria [https://zenodo.org/records/7554815#.ZF-dztLMK-Y]
### Authors
Giulia Elizabeth de Sanctis, Jacopo Lazzari, Beatrice Re

### Explanation of the code: 
Python: 
Autoencoder.py contains the architecture for the DSAE reproduced as faithfully as possible from the Massi et al 2021 paper
dsae.py runs the autoencoder+
dsae_config.yaml is the input file 
post_processing_dase.ipynb imports the results from the DSAE and selects features for a varying delta
R: 
fixing_NAs.R - cleans and imputes the dataste
model_comparisons.R - is the most important script, has all our classifiers and the code to run conformal prediction 
stratified_sampling.R - creates train / test / val datasets
Data:
dataset_NAs_fixed.csv is the csv of our complete dataset after NA computation, it is created with the fixing_NAs.R file
train_test_val_data contains 3 files:
test_data.csv, train_data.csv, val_data.csv contains the split of our dataset used for training, testing and validation of all our models, these are created in the file stratified_sampling.R
