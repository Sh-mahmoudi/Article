#Libraries
library(metafor)
library(meta)
df <- read.csv("testsdata.csv", header = TRUE)

#Prepare model Variables
df$LL.CI[df$LL.CI == df$UL.CI] <- df$LL.CI[df$LL.CI == df$UL.CI] * 0.99
df$UL.CI[df$LL.CI == df$UL.CI] <- df$UL.CI[df$LL.CI == df$UL.CI] * 1.01
df$logOR <- log(df$OR)
df$SE <- (log(df$UL.CI) - log(df$LL.CI)) / (2 * qnorm(0.975))

# Subgroups
df_bmi_myopia        <- subset(df, type == "BMI"    & Myopia.vs.High.Myopia == "Myopia")
df_weight_myopia     <- subset(df, type == "Weight" & Myopia.vs.High.Myopia == "Myopia")
df_height_myopia     <- subset(df, type == "Height" & Myopia.vs.High.Myopia == "Myopia")
df_bmi_highmyopia    <- subset(df, type == "BMI"    & Myopia.vs.High.Myopia == "High Myopia")
df_height_highmyopia <- subset(df, type == "Height" & Myopia.vs.High.Myopia == "High Myopia")

#Run meta-analysis diagnostics
run_meta_diagnostics <- function(data, subgroup_name) {
  data <- data[!is.na(data$logOR) & !is.na(data$SE) & data$SE > 0, ]
  if (nrow(data) < 3) {
    cat("\n\n========================\n")
    cat("Not enough studies for:", subgroup_name, "\n")
    cat("========================\n")
    return(NULL)
  }
  cat("\n\n========================\n")
  cat("Results for:", subgroup_name, "\n")
  cat("========================\n")
  
  # Two Level Meta analysis 
  res <- rma(yi = logOR, sei = SE, data = data, method = "REML")
  print(summary(res))
  
  # Funnel Plot
  pdf(paste0("Funnel_plot_", gsub(" ", "_", subgroup_name), ".pdf"), width = 5, height = 5)
  funnel(res, main = paste("Funnel Plot:", subgroup_name))
  dev.off()
  
  # Leave-One-Out Sensitivity Analysis
  cat("\nLeave-One-Out Sensitivity Analysis:\n")
  leave1out_res <- leave1out(res)
  print(leave1out_res)
  
  # Create a meta object for metabias()
  meta_obj <- metagen(TE = data$logOR,
                      seTE = data$SE,
                      studlab = data$ID,
                      sm = "OR",
                      method.tau = "REML")
  
  # Egger's Test
  cat("\nEgger's Test:\n")
  egger <- metabias(meta_obj, method.bias = "linreg", k.min = 1)
  print(egger)
  
  # Begg's Test
  cat("\nBegg's Test:\n")
  begg <- metabias(meta_obj, method.bias = "rank", k.min = 1)
  print(begg)
}

# Run for all five subgroups
run_meta_diagnostics(df_bmi_myopia, "BMI - Myopia")
run_meta_diagnostics(df_weight_myopia, "Weight - Myopia")
run_meta_diagnostics(df_height_myopia, "Height - Myopia")
run_meta_diagnostics(df_bmi_highmyopia, "BMI - High Myopia")
run_meta_diagnostics(df_height_highmyopia, "Height - High Myopia")
