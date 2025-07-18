#Libraries
library(dosresmeta)
library(dplyr)
df<-read.csv("cal.csv", header=TRUE)

# Use mean BMI directly
df <- df %>%
  rename(dose = min.BMI)

# Set the reference dose 
reference_dose <- df %>%
  filter(category == 2) %>%
  summarize(ref = mean(dose, na.rm = TRUE)) %>%
  pull(ref)

#Prepare model Variables
df <- df %>%
  mutate(
    change = dose - reference_dose,
    logrr = log(OR),
    loglb = log(LL.CI),
    logub = log(UL.CI),
    se = (logub - loglb) / (2 * qnorm(0.975))
  )

# Filter Data for Above-Reference Categories
df_above <- df %>%
  filter(category %in% c(2, 3, 4)) %>%
  mutate(
    change = dose - reference_dose,
    ID = 1
  )
# dose-response model
res_above <- dosresmeta(
  formula = logrr ~ change,
  id = ID,
  type = "cc",
  cases = case,
  n = n,
  se = se,
  data = df_above,
  method = "reml"
)

# Calculate and Display Results
coef_above <- coef(res_above)["change"]
se_above <- sqrt(vcov(res_above)["change", "change"])
OR_above <- exp(coef_above)
CI_above_lower <- exp(coef_above - 1.96 * se_above)
CI_above_upper <- exp(coef_above + 1.96 * se_above)

cat("OR for 1-unit BMI *increase* (above reference):", round(OR_above, 3),
    "(95% CI:", round(CI_above_lower, 3), "-", round(CI_above_upper, 3), ")\n")
