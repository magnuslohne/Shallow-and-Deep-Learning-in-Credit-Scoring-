# Function 
wilcox_compare_numfeatures <- function(pos_df, neg_list, num_vars) {
  results_g <- list()
  results_l <- list()

  for (var in num_vars) {
    p_vals_l <- c()
    p_vals_g <- c()

    for (neg_df in neg_list) {
      test_result <- wilcox.test(pos_df[[var]], neg_df[[var]], alternative = "greater")
      
      if (test_result$p.value < 0.5) {
        p_vals_g <- c(p_vals_g, test_result$p.value)
      } else {
        test_result <- wilcox.test(pos_df[[var]], neg_df[[var]], alternative = "less")
        p_vals_l <- c(p_vals_l, test_result$p.value)
      }
    }
    results_g[[var]] <- p_vals_g      
    results_l[[var]] <- p_vals_l
  }
  
  pval_df_l <- as.data.frame(results_l)
  pval_df_g <- as.data.frame(results_g)
  return(list(pvals_l = pval_df_l, pvals_g = pval_df_g))
}


# Example usage
pos_df <- data.frame(test_set[which(test_set$Response == 1), ])
npos <- nrow(pos_df)

neg_df1 <- data.frame(test_set[which(test_set$Response == 0), ])
neg_df2 <- data.frame(test_set[sample(which(test_set$Response == 0), npos), ])
neg_list <- list(
  neg_1 = neg_df1,
  neg_2 = neg_df2
)

numeric_feats <- c("Feature_1", "Feature_2", "Feature_3")

p_values_list <- wilcox_compare_numfeatures(pos_df, neg_list, numeric_feats)

