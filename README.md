# full_shape_classification_SRVF_preserving_size

Step 1:
dataprep.R

Step 2: 
Data_prep_for_full_training_dataset.m
This outputs all the data projected onto the tangent space.  

Step 2a: 
elliptical_fourier_feature_generation.R
This generates features based on EFA.  

Step 3: Cross validation.  
random_forest_species_given_tribe.R
svm_linear_kernel_species_given_tribe.R
svm_radial_kernel_species_given_tribe.R

Step 4: 
results_summary.R

Step 5: Data example with Gladysvale teeth
Gladysvale_Actual_Fossil_Prediction.R
Gladysvale_Actual_Fossil_Prediction_Overall_Mean.R





