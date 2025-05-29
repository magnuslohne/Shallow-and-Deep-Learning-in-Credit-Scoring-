# Function

create_facet_histogram <- function(data, x_var, filename, bin_count = 11) {
  x_vals <- data[[x_var]]

  p <- ggplot(data, aes(x = .data[[x_var]], fill = Group)) +
    geom_histogram(aes(y = after_stat(density)), bins = bin_count, color = "black") +
    facet_wrap(~ Group, nrow = 1) +
    scale_fill_manual(values = c("skyblue", "salmon", "salmon")) +
    theme_minimal(base_family = "") +
    theme(
      legend.position = "none",
      strip.text = element_text(colour=NA, face = "bold", size = 12),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    ) +
    labs(
      x = "",
      y = ""
    )
  
  ggsave(filename, plot = p, width = 6, height = 3, device = cairo_pdf)
}

# Example usage
df_age <- data.frame(
  Age = c(age_pos, age_neg, rand_age_neg),
  Group = factor(c(
    rep("All Defaults", length(age_pos)),
    rep("All No-Defaults", length(age_neg)),
    rep("Subset of No-Defaults", length(rand_age_neg))
  ), levels = c("All Defaults", "All No-Defaults", "Subset of No-Defaults"))
)

create_facet_histogram(df_age, "Age", "age_testset_plot.pdf")
