###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

Hurdle_Regularized_Regression <- function(datazl,LocalCase,family,alpha){
  #2006-2013: training set
  set.seed(123)
  X <- as.matrix(datazl[1:91,])
  y <- LocalCase[1:91]
  newx <- as.matrix(datazl)
  # 
  fit_zero_inflated <- cv.glmnet(X, as.factor(y == 0), family = "binomial", alpha = alpha, maxit = 1000000)
  
  # 
  fit_nonzero <- cv.glmnet(X[y > 0, ], y[y > 0], family = family, alpha = alpha, maxit = 1000000)
  
  # 
  best_lambda_zero_inflated <- fit_zero_inflated$lambda.min
  best_lambda_nonzero <- fit_nonzero$lambda.min
  
  # 
  predict_zero_inflated <- predict(fit_zero_inflated, newx =newx , s = best_lambda_zero_inflated, type = "response")
  predict_nonzero <- predict(fit_nonzero, newx = newx, s = best_lambda_nonzero, type = "response")
  
  final_predictions <- (1 - predict_zero_inflated) * predict_nonzero
  
  coef_zero_inflated <- coef(fit_zero_inflated, s = best_lambda_zero_inflated)
    coef_nonzero <- coef(fit_nonzero, s = best_lambda_nonzero)
  
  rmse_fit <- sqrt(mean((LocalCase[1:91] - final_predictions[1:91])^2))
  rmse_pred <- sqrt(mean((LocalCase[92:115] - final_predictions[92:115])^2))
  rmse <- sqrt(mean((LocalCase - final_predictions)^2))
  mae <- mean(abs(LocalCase - final_predictions))
  
  bootstrap_predict <- function(d, indices) {
    X_bootstrap <- d[indices,-1 ]
    y_bootstrap <- d[indices,1]  # 
    # 
    fit_zero_inflated_bootstrap <- glmnet(X_bootstrap, as.factor(y_bootstrap == 0), family = "binomial", alpha = alpha, maxit = 80000000)
    fit_nonzero_bootstrap <- glmnet(X_bootstrap[y_bootstrap > 0, ], y_bootstrap[y_bootstrap > 0], family = "gaussian", alpha = alpha, maxit = 80000000)
    
    # 
    predict_zero_inflated_bootstrap <- predict(fit_zero_inflated_bootstrap, newx = newx, s =  best_lambda_zero_inflated, type = "response")
    predict_nonzero_bootstrap <- predict(fit_nonzero_bootstrap, newx = newx, s = best_lambda_nonzero, type = "response")
    
    # 
    preds <- (1 - predict_zero_inflated_bootstrap) * predict_nonzero_bootstrap
    
    return(preds)
  }
  
  # 
  set.seed(123)
  results <- boot(cbind(y,X), bootstrap_predict, R = 1000)
  
  # 
  conf_interval <- boot.ci(results, type = "perc")
  
  # 
  predicted_df <- data.frame(
    Predicted = final_predictions,
    Lower = apply(results$t, 2, function(x) {
      q <- quantile(x, 0.025)
      return( q)  # 
    }),
    Upper = apply(results$t, 2, function(x) {
      q <- quantile(x, 0.975)
      return( q)  #
    })
  )
  return(list(predicted_df = predicted_df,
              coef_zero_inflated = coef_zero_inflated,
              coef_nonzero = coef_nonzero,
              rmse_fit = rmse_fit,
              rmse_pred =rmse_pred,
              rmse=rmse))
  
}