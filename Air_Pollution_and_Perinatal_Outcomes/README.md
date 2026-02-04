# Prenatal Exposure to Ambient Air Pollution and Adverse Birth Outcomes: Findings from the PERSIAN Birth Cohort – Isfahan

This repository contains Python code for analyzing the association between maternal air pollution exposure during pregnancy and Perinatal Outcomes.

### Data Source

The dataset is not included in this repository due to privacy and ethical restrictions.

### Project Structure

**1.	Data preparation**

* Define categorical and continuous variables

* Convert variables to appropriate data types

**2.	Normality testing**

* Shapiro–Wilk test for continuous variables

**3.	Categorical analysis**

* Chi-square tests vs gestational status

* Results exported to Excel

**4.	Continuous analysis**

* Summary statistics by gestational status

* Mann–Whitney U test

* Results exported to Excel

**5.	Air pollution exposure**

* Create binary indicators based on the 75th percentile

* Separate indicators for different pregnancy periods

**6.	Ordinal logistic regression**

* Outcome: birthweight category (and alternatives)

* Crude and adjusted models

* Odds ratios and 95% CIs reported

**7.	Season-specific analysis**

* Models repeated within each season

### Key Libraries

* pandas

* numpy

* scipy

* statsmodels

