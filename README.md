# Nonparametric Analysis of Clinical Indicators for Bloodstream Infection Detection
### Research Question
Is it possible to create an accurate prediction model for the presence of bacteremia using blood test results?
### Dataset
Bacteremia Dataset from Vienna General Hospital, Austria [https://zenodo.org/records/7554815#.ZF-dztLMK-Y]
### Authors
Giulia Elizabeth de Sanctis, Jacopo Lazzari, Beatrice Re <br>

### Explanation of the code: <br>
### Python: <br>
`Autoencoder.py` contains the architecture for the DSAE reproduced as faithfully as possible from the Massi et al. (2021) paper <br>
`dsae.py` runs the autoencoder <br>
`dsae_config.yaml` is the input file <br>
`post_processing_dase.ipynb` imports the results from the DSAE and selects features for a varying delta <br>
`other_models.ipynb` contains other attempted models such as random forest <br>
### R: <br>
`fixing_NAs.R` cleans and imputes the datastet <br>
`model_comparisons.R` is the most important script, has all our classifiers and the code to run conformal prediction <br>
`stratified_sampling.R` creates train / test / val datasets <br>
`rank_test.R` performs Mann-Whitney U-Test on the selected feature, also dividing by sex <br>
`rank_test_...` files repeat the rank tests with different NAs imputations <br>
### Data: <br>
`dataset_NAs_fixed.csv` is the csv of our complete dataset after NA computation, it is created with the `fixing_NAs.R` file <br>
The train_test_val_data folder contains 3 files:<br>
`test_data.csv`, `train_data.csv`, `val_data.csv` contains the split of our dataset used for training, testing and validation of all our models, these are created in the file `stratified_sampling.R`
