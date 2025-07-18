#Libraries
library(dosresmeta)
library(dplyr)
df<-read.csv("cal.csv", header=TRUE)

#Calculate Continuous Exposure Values 
df <- df %>%
  mutate(range = ifelse(category %in% c(2, 3), max.BMI - min.BMI, NA))
range_mean <- df %>%
  filter(category %in% c(2, 3)) %>%
  summarize(range_mean = mean(range, na.rm = TRUE)) %>%
  pull(range_mean)
df <- df %>%
  mutate(
    mult = 2,
    min = ifelse(category == 1, max.BMI - mult * range_mean, min.BMI),
    max = ifelse(category == 4, min.BMI + mult * range_mean, max.BMI),
    dose = (min + max) / 2
  )
average0 <- df %>%
  filter(category == 1) %>%
  summarize(average0 = mean(dose, na.rm = TRUE)) %>%
  pull(average0)

#Prepare model Variables
df <- df %>%
  mutate(
    change = dose - average0,
    logOR = log(OR),
    loglb = log(LL.CI),
    logub = log(UL.CI),
    se = (logub - loglb) / (2 * qnorm(0.975))
  )

# Linear dose-response model
res <- dosresmeta(
  formula = logOR ~ change,
  id = ID,         
  type = "cc",          
  cases = case,
  n = n,
  se = se,
  data = df,
  method = "reml"
)

# Coefficient for 1-unit change
coef <- coef(res)["change"]
se_coef <- sqrt(vcov(res)["change", "change"])

# OR and 95% CI
OR <- exp(coef)
CI_lower <- exp(coef - 1.96 * se_coef)
CI_upper <- exp(coef + 1.96 * se_coef)
cat("OR for 1-unit increase in change:", round(OR, 3),
    " (95% CI:", round(CI_lower, 3), "-", round(CI_upper, 3), ")\n")
