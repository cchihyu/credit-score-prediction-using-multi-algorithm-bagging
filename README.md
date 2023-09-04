# credit-score-prediction-using-multi-algorithm-bagging
This is a final project assigned by the course STAT505 Statistical Learning in National Tsing University.
The data we use is credit_score_classification_processed, which can be found on kaggle. 
When considering the correlation of base models in ensemble learning, some research adjust this issue by modifying sample scheme, aggregation step, and base model construction. Multi-algorithm bagging is the one that modifying the creation of base models to deal with the correlation. The reference and algorithm can be found in the pdf file.

## Overview
We use QDA, KNN, Logistic and tree model to construct base models. Originally, bagging with the same algorithm is performed. We further analyze the correlation and diversity between base predictors. To apply multi-algorithm bagging, we select same amount of base predictors from QDA, KNN, Logistic and tree model respectively and use major voting to produce the final results. 

## Outline
EDA of the data set.
Analyzing the diversity of the base model using the same algotirhm.
Aggregating multi-algorithm base models and analyzing its diversity and also accuracy.
Final results and conclusion.
