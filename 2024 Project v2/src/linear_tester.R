# Find the best weights
library(xtable)
library(kableExtra)

linearity_checks <- function(df) {
  # Check if the dataframe has exactly two columns
  if (ncol(df) != 2) {
    stop("The dataframe must contain exactly two columns.")
  }
  
  # Load necessary libraries
  required_packages <- c("ggplot2", "car", "lmtest", "nortest", "Metrics", "psych")
  new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages, dependencies = TRUE)
  lapply(required_packages, library, character.only = TRUE)
  
  # Extract variables
  x <- df[[1]]
  y <- df[[2]]
  var_names <- names(df)
  
  
  # 1. Visual Inspection: Scatter plot with regression lines
  # Plot both linear and quadratic regression lines
  print(
    ggplot(df, aes_string(x = var_names[1], y = var_names[2])) +
      geom_point(color = "darkblue") +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "red", linetype = "dashed") +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "green") +
      ggtitle("Scatter Plot with Linear (Red Dashed) and Quadratic (Green Solid) Regression Lines") +
      theme_minimal()
  )
  
  # 2. Pearson Correlation Coefficient
  cat("\n**Pearson Correlation Coefficient**\n")
  cat("Expected Value: Close to 1 or -1 indicates a strong linear relationship.\n")
  cat("Values close to 0 indicate a weak linear relationship.\n")
  pearson_cor <- cor(x, y, method = "pearson")
  cat("Pearson Correlation Coefficient:", round(pearson_cor, 4), "\n")
  
  # 3. Spearman's Rank Correlation Coefficient
  cat("\n**Spearman's Rank Correlation Coefficient**\n")
  cat("Expected Value: Close to 1 or -1 indicates a strong monotonic relationship.\n")
  cat("Useful if the relationship is monotonic but not strictly linear.\n")
  spearman_cor <- cor(x, y, method = "spearman")
  cat("Spearman's Rank Correlation Coefficient:", round(spearman_cor, 4), "\n")
  
  # 4. Kendall's Tau
  cat("\n**Kendall's Tau**\n")
  cat("Expected Value: Close to 1 or -1 indicates a strong ordinal association.\n")
  kendall_tau <- cor(x, y, method = "kendall")
  cat("Kendall's Tau:", round(kendall_tau, 4), "\n")
  
  # 5. Fit Linear Model
  lm_model <- lm(y ~ x, data = df)
  summary_lm <- summary(lm_model)
  cat("\n**Linear Model Summary**\n")
  cat("Expected: Significant coefficients (p-value < 0.05) and a high R-squared value indicate a good linear fit.\n")
  print(summary_lm)
  
  # 6. Error Metrics: MSE, RMSE, MAE
  cat("\n**Error Metrics**\n")
  cat("Expected: Lower error metrics (MSE, RMSE, MAE) indicate a better model fit.\n")
  mse_value <- mse(y, lm_model$fitted.values)
  rmse_value <- rmse(y, lm_model$fitted.values)
  mae_value <- mae(y, lm_model$fitted.values)
  cat("MSE:", round(mse_value, 4), "\n")
  cat("RMSE:", round(rmse_value, 4), "\n")
  cat("MAE:", round(mae_value, 4), "\n")
  
  # 7. Residual Analysis Plots
  cat("\n**Residual Analysis Plots**\n")
  cat("Expected: Residuals should be randomly scattered around zero with no discernible patterns.\n")
  par(mfrow = c(2, 2))
  plot(lm_model)
  par(mfrow = c(1, 1))  # Reset plotting area
  
  # 8. Normality Test of Residuals
  cat("\n**Shapiro-Wilk Normality Test for Residuals**\n")
  cat("Expected: p-value > 0.05 indicates that residuals are normally distributed.\n")
  shapiro_test <- shapiro.test(residuals(lm_model))
  print(shapiro_test)
  
  # 9. Breusch-Pagan Test for Heteroscedasticity
  cat("\n**Breusch-Pagan Test for Heteroscedasticity**\n")
  cat("Expected: p-value > 0.05 suggests homoscedasticity (constant variance of residuals).\n")
  bp_test <- bptest(lm_model)
  print(bp_test)
  
  # 10. Ramsey's RESET Test for Model Specification
  cat("\n**Ramsey's RESET Test for Model Specification**\n")
  cat("Expected: p-value > 0.05 implies no significant non-linear relationships missed by the model.\n")
  reset_test <- resettest(lm_model, power = 2:3, type = "fitted")
  print(reset_test)
  
  # 11. Fit Quadratic Model for Comparison
  lm_quad <- lm(y ~ x + I(x^2), data = df)
  summary_quad <- summary(lm_quad)
  cat("\n**Quadratic Model Summary**\n")
  cat("Expected: If the quadratic term is not significant (p-value > 0.05), the linear model may be sufficient.\n")
  print(summary_quad)
  
  # 12. Compare Models using AIC and BIC
  cat("\n**Model Comparison Metrics (AIC and BIC)**\n")
  cat("Expected: Lower AIC/BIC values indicate a better model considering model complexity.\n")
  aic_lm <- AIC(lm_model)
  bic_lm <- BIC(lm_model)
  aic_quad <- AIC(lm_quad)
  bic_quad <- BIC(lm_quad)
  cat("Linear Model AIC:", round(aic_lm, 4), ", BIC:", round(bic_lm, 4), "\n")
  cat("Quadratic Model AIC:", round(aic_quad, 4), ", BIC:", round(bic_quad, 4), "\n")
  
  # 13. ANOVA to Compare Models
  cat("\n**ANOVA to Compare Linear and Quadratic Models**\n")
  cat("Expected: p-value < 0.05 suggests the quadratic model provides a significantly better fit.\n")
  anova_models <- anova(lm_model, lm_quad)
  print(anova_models)
  
  # 14. Cook's Distance for Influential Observations
  cat("\n**Cook's Distance for Influential Observations**\n")
  cat("Expected: Observations with Cook's Distance > 4/(n - k - 1) may be influential and affect the model.\n")
  cooks_d <- cooks.distance(lm_model)
  plot(cooks_d, ylab = "Cook's Distance", type = "h", main = "Cook's Distance for Linear Model")
  abline(h = 4 / (length(y) - length(lm_model$coefficients)), col = "red", lty = 2)
  
  # Return a list of results
  results <- list(
    pearson_correlation = pearson_cor,
    spearman_correlation = spearman_cor,
    kendall_tau = kendall_tau,
    linear_model_summary = summary_lm,
    mse = mse_value,
    rmse = rmse_value,
    mae = mae_value,
    shapiro_test = shapiro_test,
    breusch_pagan_test = bp_test,
    reset_test = reset_test,
    quadratic_model_summary = summary_quad,
    aic_values = c(Linear_Model = aic_lm, Quadratic_Model = aic_quad),
    bic_values = c(Linear_Model = bic_lm, Quadratic_Model = bic_quad),
    anova_models = anova_models,
    cooks_distance = cooks_d
  )
  
  return(results)
}

