# Auto_Loan_Default
Classification Model of Auto Loan Default

This project evaluates evaluates the predictive performance of four classification models on an auto loan default dataset.

There are three initial datasets used throughout the project: a data dictionary, a dataset that the models were trained on, and a final testing dataset that does not indicate whether a loan was defaulted on or not. This third dataset can be used to simulate a real life scenario where a company would like to analyze whether a loan should be offered to a potential client. Based on the model that was trained on a dataset of past defaults, predictions can be made whether potential new clients will default on their loans or not.

The data was first cleaned, and values were imputed for the NAs and missing values. Some exploratory data analysis was performed to understand any underlying trends in the data; these trends are visualized in the plots located in the repositroy. The primary dataset was then randomly divided into a 75%/25% train/test split. Logistic regression, lasso regression, ridge regression, and random forest models were fit onto the training data. These models were then tested on the holdout testing set to determine which model had the most predictive power. Three summary statistics were used to determine the best model for the data: area under the ROC curve (AUC), mean absolute error (MAE), and root mean squared error (RMSE).

For all three summary measures, the random forest model performed the best on the testing subset, predicting with roughly 93% accuracy. This model was then used to predict default on the final testing dataset. The model identified 712 individuals who are predicted to default on their auto loan. In a real life scenario, these individuals should not be offered a loan, as they would pose a greater financial risk to the lending company. 
