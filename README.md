### Predictive Analytics on Health Care Data

### Technical Stack:

R programming language and associated packages such as tidyverse, ggplot2, usmap, imputeTS, zoo, kernlab, caret, cvms, tibble, e1071, and rpart.


### Overview

- This project aims to identify and predict factors influencing healthcare costs using a dataset from a Health Maintenance Organization (HMO). Our analysis is centered around discovering patterns in healthcare expenditure and providing insights to HMOs for better policy formulation.

### Objectives

- To determine key factors impacting healthcare costs.
- To predict customer segments likely to incur higher healthcare expenses.
- To offer actionable insights for cost optimization in health insurance policies.

### Methodology

- Utilizing statistical and machine learning techniques, the project conducts extensive data pre-processing, exploratory data analysis, and builds predictive models. Key procedures include:

- **Data Acquisition and Preprocessing:** Addressing missing values and ensuring data quality.
- **Descriptive Analysis:** Understanding data distribution and variable relationships.
- **Predictive Modelling:** Employing Linear Regression, Support Vector Machine, and Decision Tree models to forecast 'Expensive' insurance claimants.

### Significant Findings

- Age, BMI, smoking habits, exercise frequency, and family size are significant predictors of healthcare costs.
- Lifestyle and demographic factors such as marital status and place of residence contribute to the cost variation.
- Health-related habits, including exercise and annual physicals, correlate with healthcare expenses.

### Recommendations

- Focus on individuals aged 20-30 and those with a BMI range of 20-30 due to their lower claims.
- Target nonsmokers and customers from rural areas to optimize cost management.
- Encourage preventive healthcare practices among customers to reduce expensive claims.

### Model Insights

- The Support Vector Machine model achieved the highest prediction accuracy at 87.22%.
- The model efficiently identifies customers likely to fall into the 'Expensive' category for the coming year.

### Application: Shiny Web App

- Developed an interactive web application using Shiny that facilitates the prediction of healthcare costs and assesses the accuracy of the model through a user-friendly interface.

### Data Visualization

- Graphical representations illustrate key correlations between demographic characteristics, health-related habits, and healthcare costs, enabling intuitive understanding of the data.

