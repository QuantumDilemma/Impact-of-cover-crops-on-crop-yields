```{R}
#import libraries
library(cobalt)
library(WeightIt)
library(lmtest)
library(sandwich)

farms_df <- read.csv("farms.csv")
head(farms_df)
```


```{R}
#balance plot for the average age
bal.plot(
  x = cover_10 ~ age_avg,
  data = farms_df,
  var.name = "age_avg"
)
```

```{R}
#balance plot for the different regions
bal.plot(
  x = cover_10 ~ region,
  data = farms_df,
  var.name = "region"
)
```

```{R}
#balance table to show SMD (Standardized Mean Differences) and Variance Ratios for all predictor variables according to the treatment group
bal.tab(
  x = cover_10 ~ age_avg + region,
  data = farms_df,
  binary = "std",
  disp.v.ratio = TRUE
)
```

The SMDs are outside of the recommended -0.1 and 0.1 range.

The variance ratio for age_avg is within the recommended range of 0.5 - 2.0


