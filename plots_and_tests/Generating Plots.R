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
pos_df <- data.frame(test_set[which(test_set$Response == 1), ])
npos <- nrow(pos_df)

neg_df1 <- data.frame(test_set[which(test_set$Response == 0), ])
nneg <- nrow(neg_def1)

neg_df2 <- data.frame(test_set[sample(which(test_set$Response == 0), npos), ])


df_Feature1 <- data.frame(
  Feature = c(pos_df$Feature_1, neg_df1$Feature_1, neg_df2$Feature_1),
  Group = factor(c(
    rep("All Positives", npos),
    rep("All Negatives", nneg),
    rep("Subset of Negatives", npos)
  ), levels = c("All Positives", "All Negatives", "Subset of Negatives"))
)

create_facet_histogram(df_Feature1, "Feature 1", "Feature1_testset_plot.pdf")
