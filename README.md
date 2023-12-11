# full_shape_classification_SRVF_preserving_size
Step 1: Getting the data and cleaning it so it can be projected onto the tangnet space.  
./code/R/dataprep.R

Step 2: This outputs all the data projected onto the tangent space at the meam shape.    
./code/matlab/Data_prep_for_full_training_dataset.m


Step 2a: This generates features based on EFA.  
./code/R/elliptical_fourier_feature_generation.R

Step 2b: 
./code/R/create_folds.R


Step 3: Cross validation code. 
./code/R/random_forest_species_given_tribe.R
./code/R/svm_linear_kernel_species_given_tribe.R
./code/R/svm_radial_kernel_species_given_tribe.R

Step 4: Summarizing results 
./code/R/results_summary.R

Step 5: Data example with Gladysvale teeth
./code/R/dataprep_for_gladysvale.R
./code/matlab/dataprep_for_gladyvale_teeth_for_prediction.m
./code/R/Gladysvale_Actual_Fossil_Prediction.R ./code/R/Gladysvale_Actual_Fossil_Prediction_Overall_Mean.R


Other code: 
./code/R/deformations_EFA_for_paper.R
./code/R/PCA_plots_for_paper.R


