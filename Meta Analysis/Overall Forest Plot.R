
#Libraries
library(metafor)
df<-read.csv("totalfinal1.csv", header=TRUE)

#Prepare forest plot Variables
df$y <- log(df$OR)
df$se <- (log(df$UL.CI) - log(df$LL.CI)) / (2 * 1.96)
df$v <- df$se^2
df$effect_id <- ave(df$y, df$ID, FUN = seq_along)
df$subgroup <- as.character(df$group)
df$group <- sapply(strsplit(df$subgroup, " \\- "), `[`, 1)
df$outcome <- sapply(strsplit(df$subgroup, " \\- "), `[`, 2)
df$year_only <- as.character(df$year)

# Prepare labels for a forest plot
study_col <- format(df$Author, width = 20, justify = "left")
year_col  <- format(df$year_only, width = 6, justify = "left")
group_col <- format(df$subgroup, width = 20, justify = "left")
df$labels_clean <- paste(study_col, year_col, group_col)
header_label <- paste(format("Study", width = 20, justify = "left"),
                      format("Year", width = 6, justify = "left"),
                      format("Group", width = 20, justify = "left"))

# Calculates plot row positions
df <- df[order(df$subgroup, df$ID), ]
unique_groups <- unique(df$subgroup)
df$plot_row <- NA
diamond_rows <- c()
gap <- 2
current_row <- 3  
for (group in unique_groups) {
  group_idx <- which(df$subgroup == group)
  n <- length(group_idx)
  group_rows <- seq(current_row + n - 1, current_row)
  df$plot_row[group_idx] <- group_rows
  diamond_rows <- c(diamond_rows, min(group_rows) - 1)
  current_row <- current_row + n + gap
}

# Fits a three-level meta-analytic model
res <- rma.mv(y, v, random = ~1 | ID/studyid, data = df)

# Generates a forest plot
pdf("forest_plot_total.pdf", width = 14, height = 20)  # size in inches
forest(res,
       transf = exp,
       refline = 1,
       xlab = "Odds Ratio (OR)",
       slab = df$labels_clean,
       rows = df$plot_row,
       alim = c(0.3, 3),
       header = header_label,
       cex = 0.8,
       ylim = c(0, max(df$plot_row) + 3))  # top & bottom space

# Adds diamonds for each subgroup
for (i in seq_along(unique_groups)) {
  group <- unique_groups[i]
  subset_data <- df[df$subgroup == group, ]
  res_sub <- rma.mv(y, v, random = ~1 | ID/studyid, data = subset_data)
  pred <- predict(res_sub)
  addpoly(pred$pred,
          ci.lb = pred$ci.lb,
          ci.ub = pred$ci.ub,
          transf = exp,
          mlab = paste("Combined effect for", group),
          rows = diamond_rows[i])
  text(-3.7, max(subset_data$plot_row) + 1.5, group, font = 2, pos = 4)
}

# Adds overall combined effect diamond at the bottom
bottom_row <- 1  # Below everything
overall <- predict(res)
addpoly(overall$pred,
        ci.lb = overall$ci.lb,
        ci.ub = overall$ci.ub,
        transf = exp,
        mlab = "Overall Combined Effect",
        rows = bottom_row)

dev.off()
