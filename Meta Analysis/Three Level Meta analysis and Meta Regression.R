#Libraries
library(metafor)
df<-read.csv("totalfinal1.csv", header=TRUE)

#Prepare model Variables
df$y <- log(df$OR)
df$se <- (log(df$UL.CI) - log(df$LL.CI)) / (2 * 1.96)
df$v <- df$se^2
df$effect_id <- ave(df$y, df$ID, FUN = seq_along)
df$type <- factor(df$type)
df$outcome <- factor(df$Myopia.vs.High.Myopia)
df$group<-factor(df$group)
df$study<-factor(df$study)

#Three-level random-effects model
res_3level <- rma.mv(y, v, mods = ~ -1 + group, random = ~ 1 | ID/effect_id, data = df,method = "REML")
summary(res_3level)

# Define subgroups
df_bmi_myopia <- subset(df, type == "BMI" & Myopia.vs.High.Myopia == "Myopia")
df_weight_myopia <- subset(df, type == "Weight" & Myopia.vs.High.Myopia == "Myopia")
df_height_myopia <- subset(df, type == "Height" & Myopia.vs.High.Myopia == "Myopia")
df_bmi_highmyopia <- subset(df, type == "BMI" & Myopia.vs.High.Myopia == "High Myopia")
df_height_highmyopia <- subset(df, type == "Height" & Myopia.vs.High.Myopia == "High Myopia")

#Three-level Meta-Analysis on subset
res_bmi_myopia <- rma.mv(
  y, v,
  random = ~ 1 | ID/effect_id,
  data = df_bmi_myopia,
  method = "REML"
)
summary(res_bmi_myopia)

# Meta-Regression for subgroup analysis
res_meta_reg <- rma.mv(
  y, v,
  #mods = ~ mean.age,
  #mods = ~ study,
  #mods = ~  subgroup.sample.size,
  mods = ~  -1+subgroup.sample.size + study + mean.age ,
  #mods = ~  -1+subgroup.sample.size + study,
  #mods = ~  -1+subgroup.sample.size + mean.age,
  random = ~ 1 | ID/effect_id,
  data = df_bmi_myopia,
  method = "REML"
)
summary(res_meta_reg)
