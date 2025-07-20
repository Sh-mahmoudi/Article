#Libraries
library(metafor)
df<-read.csv("totalfinal1.csv", header=TRUE)

#Prepare forest plot Variables
df$subgroup <- as.character(df$Myopia.vs.High.Myopia)
df$year_only <- as.character(df$year)
df$y <- log(df$OR)
df$se <- (log(df$UL.CI) - log(df$LL.CI)) / (2 * 1.96)
df$v <- df$se^2


# Prepare labels for  forest plot
Author_col <- format(df$Author, width = 20, justify = "left")
year_col  <- format(df$year_only, width = 6, justify = "left")
group_col <- format(df$subgroup, width = 20, justify = "left")
df$labels_clean <- paste(Author_col, year_col, group_col)
header_label <- paste(format("Author", width = 20, justify = "left"),
                      format("Year", width = 6, justify = "left"),
                      format("Group", width = 20, justify = "left"))

# forest plot for a given `type`
generate_forest_plot <- function(df_type, type_label, filename) {
  df_type <- df_type[order(df_type$subgroup, df_type$ID), ]
  unique_groups <- unique(df_type$subgroup)
  df_type$plot_row <- NA
  diamond_rows <- c()
  gap <- 2
  current_row <- 3
  
  for (group in unique_groups) {
    group_idx <- which(df_type$subgroup == group)
    n <- length(group_idx)
    group_rows <- seq(current_row + n - 1, current_row)
    df_type$plot_row[group_idx] <- group_rows
    diamond_rows <- c(diamond_rows, min(group_rows) - 1)
    current_row <- current_row + n + gap
  }
  
  res <- rma.mv(y, v, random = ~1 | ID/studyid, data = df_type)
  
  pdf(filename, width = 6, height = max(4, 0.3 * nrow(df_type)))
  
  forest(res,
         transf = exp,
         refline = 1,
         xlab = "Odds Ratio (OR)",
         slab = df_type$labels_clean,
         rows = df_type$plot_row,
         alim = c(0.3, 3),
         header = header_label,
         cex = 0.8,
         ylim = c(0, max(df_type$plot_row) + 3))
  
  for (i in seq_along(unique_groups)) {
    group <- unique_groups[i]
    subset_data <- df_type[df_type$subgroup == group, ]
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
  
  overall <- predict(res)
  addpoly(overall$pred,
          ci.lb = overall$ci.lb,
          ci.ub = overall$ci.ub,
          transf = exp,
          mlab = "Overall Combined Effect",
          rows = 1)
  
  dev.off()
}

# Generate forest plots by `type`
generate_forest_plot(subset(df, type == "BMI"), "BMI", "forest_plot_BMI.pdf")
generate_forest_plot(subset(df, type == "Weight"), "Weight", "forest_plot_Weight.pdf")
generate_forest_plot(subset(df, type == "Height"), "Height", "forest_plot_Height.pdf")
