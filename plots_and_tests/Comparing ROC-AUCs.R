models <- list(
  XGB = roc(actual_class_test, test_prob_pred_XGB),
  NN = roc(actual_class_test, test_prob_pred_NN),
  RF = roc(actual_class_test, test_prob_pred_RF),
  LR = roc(actual_class_test, test_prob_pred_LR),
  SVM = roc(actual_class_test, test_prob_pred_SVM)
)

compare_auc(models) <- combn(names(models), 2, simplify = FALSE, FUN = function(pair) {
  delongtest <- roc.test(models[[pair[1]]], models[[pair[2]]], method = "delong", alternative = "greater")
  if (delongtest$p.value > 0.5) {
    delongtest <- roc.test(models[[pair[1]]], models[[pair[2]]], method = "delong", alternative = "less")
  }
  c(model1 = pair[1], model2 = pair[2], alternative_hypothesis = delongtest$alternative, p_value = delongtest$p.value)
})

rocauc_test_mat <- do.call(rbind, compare_auc)
