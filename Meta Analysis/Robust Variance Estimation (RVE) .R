#Libraries
library(robumeta)
df <- read.csv("cal.csv", header=TRUE)

#Prepare model Variables
df$studyid <- as.factor(df$studyid)
df$logOR <- log(df$OR)
#df$logOR <- df$logOR + rnorm(nrow(df), mean = 0, sd = 1e-5)
df$se <- (log(df$UL.CI) - log(df$LL.CI)) / (2 * 1.96)
df$var <- df$se^2

# RVE meta-analysis
rve_model <- robu(
  formula = logOR ~ 1,          
  data = df,
  studynum = studyid,           
  var.eff.size = var,           
  modelweights = "HIER",        
  small = TRUE                  
)
summary(rve_model)

# Get coefficient and CI
est <- rve_model$reg_table$b.r
se <- rve_model$reg_table$SE
ci_lb <- est - 1.96 * se
ci_ub <- est + 1.96 * se

# Convert to Odds Ratio
OR <- exp(est)
OR_low <- exp(ci_lb)
OR_high <- exp(ci_ub)

cat("Combined OR from RVE:", round(OR, 3),
    "(95% CI:", round(OR_low, 3), "-", round(OR_high, 3), ")\n")
