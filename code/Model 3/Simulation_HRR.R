###################################################################################################
##  All Code was Written by Yan Liu
## 
## Last updated: 2024.11.22
###################################################################################################

Simulation_HRR <- function(datasim,alpha,data) {

datazl <- data 

set.seed(123)
X <- as.matrix(datazl[1:91,])
y <- datzh$`ln(LocalCase)`[1:91]

fit_zero_inflated <- cv.glmnet(X, as.factor(y == 0), family = "binomial", alpha = alpha)
fit_nonzero <- cv.glmnet(X[y > 0, ], y[y > 0], family = "gaussian", alpha = alpha)

##
newdatasim <- as.matrix(datasim[1:92,])
for (i in 1:23) {
   predict_zero_inflated_sim <- predict(fit_zero_inflated, newx =newdatasim , s = fit_zero_inflated$lambda.min, type = "response")
   predict_nonzero_sim <- predict(fit_nonzero, newx = newdatasim, s = fit_nonzero$lambda.min, type = "response")
  
  final_predictions_sim <- (1 - predict_zero_inflated_sim) * predict_nonzero_sim
  newdatasim <- rbind(newdatasim,c((final_predictions_sim[92+i-1]-mean_case1_1)/sd_case1_1
                                   ,as.matrix(datasim[92+i,2:7])))
}
predict_zero_inflated_sim <- predict(fit_zero_inflated, newx =newdatasim , s = fit_zero_inflated$lambda.min, type = "response")

predict_nonzero_sim <- predict(fit_nonzero, newx = newdatasim, s = fit_nonzero$lambda.min, type = "response")

final_predictions_sim <- (1 - predict_zero_inflated_sim) * predict_nonzero_sim

return(final_predictions_sim)
}