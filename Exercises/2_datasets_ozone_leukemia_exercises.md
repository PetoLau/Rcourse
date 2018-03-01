# Exploratory analysis and feature engineering

## Ozone

 - The dataset from `faraway` package ([**`?ozone`**](https://rdrr.io/cran/faraway/man/ozone.html)),
 - 330 observations (rows) and 7 features (columns),
 - The target is to model ozone (**O3** column), so this is a regression task.

Features description:

 - `O3` Daily maximum one-hour-average ozone reading,
 - `wind` Wind speed in LA airport (LAX),
 - `humidity` Humidity at LAX,
 - `temp` Temperature,
 - `dpg` Pressure gradient (mm Hg) from LAX to Daggett, CA,
 - `vis` Visibility (miles) measured at LAX,
 - `doy` Day of Year.

#### Exercises - ozone

 - Correlation analysis (Pearson, or Spearman (`cor`)),
 - Create a pairs plot (a **matrix of scatter plots** (`ggpairs` in `GGally` package)) - assume target variable `O3`,
 - Try to find out linear and non-linear dependencies in data (smoothing, loess, GAM),
 - Try to find out interactions between data,
 - Transformations of data - power transformation of a target variable, polynomial trans. of independent variables etc.,
 - Iterate every change (idea), so do correlation analysis and visualisations.
 
## Genes Leukemia

 - Information about the dataset [**here**](https://www.kdnuggets.com/data_mining_course/data/genes-leukemia-description.txt),
 - The target is to model feature `CLASS`, so this is a classification task,
 - There are two classes - ALL (Acute Lymphoblastic leukemia) and AML (Acute Myelogenous Leukemia),
 - 72 observations (rows) and 51 features (columns),
 - Be careful with `"?"` observation values!
 - There are 9 categorical (plus target variable) features and 41 numeric features.
 
#### Exercises - leukemia

 - Data preprocessing for an analysis,
    - `?` -> `NA`
    - binning of mutlinomial or ordinal features
    - normalisation
 - Statistical hypotheses testing - find statistical significant features,
    - `CLASS` vs. categorical feature (Fisher exact test (`fisher.test`), Chi-square test (`chisq.test`))
    - `CLASS` vs. numerical feature (Normality testing (`shapiro.test`), t-test (`t.test`), Wilcoxon rank sum test (`wilcox.test`))
 - Use PCA (Principal Component Analysis) for numerical features and try to nicely visualise it (use `prcomp` and `ggplot` functions).
