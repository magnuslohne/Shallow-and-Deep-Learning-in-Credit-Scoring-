# Function
chi_sq_compare_factors <- function(pos_df, neg_list, factor_vars) {
  results <- list()
  
  for (var in factor_vars) {
    p_vals <- sapply(neg_list, function(neg_df) {
      tbl <- table(
        group = c(rep("Default", nrow(pos_df)), rep("No-default", nrow(neg_df))),
        factor = c(pos_df[[var]], neg_df[[var]])
      )
      test_result <- chisq.test(tbl)
      if (sum(test_result$expected > 5) == ncol(test_result$expected)*nrow(test_result$expected)) {
        p_vals <- test_result$p.value
      } else {
        p_vals <- NA
      }
    })
    results[[var]] <- p_vals
  }
  
  pval_df <- as.data.frame(do.call(cbind, results))
  return(pval_df)
}


# Example usage

factor_feats <- c("Factor_1", "Factor_2")
