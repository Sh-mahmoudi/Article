# Libraries
library(epitools)
library(dplyr)

# Input the data
table_data <- data.frame(
  BMI = c("Underweight", "Normal", "Overweight", "Obese"),
  Myopia = c(15, 97, 51, 18),
  Total = c(21, 229, 119, 31)
)
table_data$NonMyopia <- table_data$Total - table_data$Myopia

# Set reference group
ref_group <- "Normal"
ref_row <- table_data %>% filter(BMI == ref_group)
ref_case <- ref_row$Myopia
ref_ctrl <- ref_row$NonMyopia

# Compute Odds Ratios and 95% Confidence Intervals for each BMI group relative to the reference
OR_results <- data.frame()
for (i in 1:nrow(table_data)) {
  bmi_group <- table_data$BMI[i]
  case <- table_data$Myopia[i]
  ctrl <- table_data$NonMyopia[i]
  tab <- matrix(c(case, ctrl, ref_case, ref_ctrl), nrow = 2, byrow = TRUE)
  OR_ci <- oddsratio(tab, rev = "neither", correction = FALSE)
  OR <- OR_ci$measure[2, 1]
  lower <- OR_ci$measure[2, 2]
  upper <- OR_ci$measure[2, 3]  
  OR_results <- bind_rows(
    OR_results,
    data.frame(
      BMI = bmi_group,
      Myopia = case,
      NonMyopia = ctrl,
      OR = round(OR, 2),
      CI_lower = round(lower, 2),
      CI_upper = round(upper, 2)
    )
  )
}

# Set OR = 1 for reference row
OR_results$OR[OR_results$BMI == "Normal"] <- 1
OR_results$CI_lower[OR_results$BMI == "Normal"] <- NA
OR_results$CI_upper[OR_results$BMI == "Normal"] <- NA

print(OR_results)

