library(ggplot2)
library(patchwork)

rm(list = ls())

#load data
data_path <- "C:\\Users\\jclaz\\OneDrive\\Documenti\\GitHub\\NonparametricProject\\Code\\Kernel_Density_Estimation\\bacteremia_no_NAs.rds"
data <- readRDS(data_path)

data_with_bacteremia <- subset(data, bacteremia == 1)
data_without_bacteremia <- subset(data, bacteremia == 0)

# Define the lambda for the gaussian kernel density estimation
lambda <- 1.3

#Function to calculate the Gaussian (default) KDE and create a dataframe
calculate_kde <- function(data, variable, group_var, bw) {
  do.call(rbind, lapply(split(data, data[[group_var]]), function(group) {
    kde <- density(group[[variable]], bw = bw)
    data.frame(x = kde$x, y = kde$y, group = unique(group[[group_var]]))
  }))
}

# Analyzed variables (all pertaining the wbc), I use the ratios to check if there are differences in the proportions rather than in the sheer numbers
variables <- c("wbc", "basor", "eosr", "lymr", "monor", "neur")

# Create plots and save them as PNGs
output_folder <- "C:/Users/jclaz/OneDrive/Documenti/GitHub/NonparametricProject/Code/Kernel_Density_Estimation/Plots"
dir.create(output_folder, showWarnings = FALSE)

for (var in variables) {
  kde_with <- calculate_kde(data_with_bacteremia, var, "sex", lambda)
  plot_with <- ggplot(kde_with, aes(x = x, y = y, color = group)) +
    geom_line(size = 1) +
    labs(
      title = paste(var, "(With Bacteremia, bw =", lambda, ")"),
      x = var,
      y = "Density",
      color = "Sex"
    ) +
    theme_minimal()
  
  kde_without <- calculate_kde(data_without_bacteremia, var, "sex", lambda)
  plot_without <- ggplot(kde_without, aes(x = x, y = y, color = group)) +
    geom_line(size = 1) +
    labs(
      title = paste(var, "(Without Bacteremia, bw =", lambda, ")"),
      x = var,
      y = "Density",
      color = "Sex"
    ) +
    theme_minimal()
  
  combined_plot <- plot_with + plot_without +
    plot_annotation(
      title = paste("Kernel Density Estimation for", var),
      theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    )
  
  file_path <- file.path(output_folder, paste0(var, "_kde.png"))
  ggsave(file_path, plot = combined_plot, width = 10, height = 6)
  message("Grafico salvato: ", file_path)
}
