# Heart Disease Severity Analysis

An advanced statistical analysis of the Cleveland heart disease dataset using R.

## Project objective

This project examines the relationship between demographic, clinical, and exercise-related variables and the recorded heart disease severity outcome. It combines exploratory data analysis, an OLS baseline model, and an ordered logistic regression model.

## Dataset

- Source file: `data/heart_disease.csv`
- Observations in the uploaded file: 303
- Variables in the uploaded file: 14
- The source file uses comma-separated values and does not include a header row.
- Missing values are represented by `?` and are handled in the analysis script.

## Main variables

The analysis script assigns the standard Cleveland dataset variable names, including:

`age`, `sex`, `cp`, `trestbps`, `chol`, `fbs`, `restecg`, `thalach`, `exang`, `oldpeak`, `slope`, `ca`, `thal`, and `target`.

## Methods

1. Data import and cleaning
2. Descriptive statistics
3. Distribution plots and box plots
4. Correlation analysis
5. OLS regression as a baseline
6. Ordered logistic regression for the ordinal target
7. Model comparison using AIC
8. Multicollinearity and proportional-odds diagnostics where supported
9. Predicted probabilities
10. Interpretation and limitations

## How to run

Open R or RStudio and run:

```r
source("heart_disease_analysis.R")
```

The script creates figures in `figures/` and model outputs in `results/`.

## Important limitation

The target variable is ordinal in the Cleveland dataset. Therefore, OLS is treated as a baseline comparison rather than the ideal final model. The ordered logistic model is used as the principal specification, subject to its assumptions.

## Reproducibility

The project keeps the raw input file, analysis script, generated outputs, and documentation in separate folders so that the analysis can be reproduced and reviewed.
