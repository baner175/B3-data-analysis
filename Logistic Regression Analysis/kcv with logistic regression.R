# rm(list = ls())

library(pROC)
library(MLmetrics)
library(glmnet)
library(caret)
library(ggplot2)
source('Data Preprocessing.R')

100*table(val.data$y)/nrow(val.data)
100*table(train.data$y)/nrow(train.data)

# mod_cv <- cv.glmnet(x = as.matrix(train.data[,-1]),
#                     y = train.data$y,
#                     family = 'binomial', nfolds = 5)

opt_lam <- 0.0004331299 # mod_cv$lambda.min


# K-fold cv:
k <- 5
set.seed(1008)
fold_plus <- createFolds(1:nrow(train.plus), k=k)
fold_minus <- createFolds(1:nrow(train.minus), k=k)


specs <- function(thresh, probs, obs)
{
  pred.bin <- ifelse(probs>thresh, 1, 0)
  conf <- confusionMatrix(factor(pred.bin), factor(obs),
                          mode = 'prec_recall')
  res  <- conf$byClass[c(1,2,7)]
  return(res)
}



thresh <- seq(0.05, 0.95, 0.01)

f_mat <- NULL
spec_mat <- NULL
sens_mat <- NULL

ROC_logis <- list()
n_beta <- NULL
for(i in 1:k)
{
  ind_test_plus <- fold_plus[[i]]
  ind_test_minus <- fold_minus[[i]]
  
  test <- rbind(train.plus[ind_test_plus,],
                train.minus[ind_test_minus,])
  
  train <- rbind(train.plus[-ind_test_plus,],
                 train.minus[-ind_test_minus,])
  
  train$y <- ifelse(train$y == 'BBB+', 1, 0)
  test$y <- ifelse(test$y == 'BBB+', 1, 0)
  
  logi_mod = glmnet(y = train$y,
                    x = as.matrix(train[,-1]), 
                    family = 'binomial', 
                    alpha = 1, lambda = opt_lam)
  n_beta <- c(n_beta, which(abs(logi_mod$beta)>0)|> length())
  probs <- predict(logi_mod, as.matrix(test[,-1]),
                   type="response")
  res_mat <- sapply(thresh, specs, probs = probs, obs = test[,1])
  
  f_mat <- cbind(f_mat, res_mat[3,])
  spec_mat <- cbind(spec_mat, res_mat[2,])
  sens_mat <- cbind(sens_mat, res_mat[1,])
  
  ROC_logis[[i]] <- suppressMessages(roc(test$y, as.numeric(probs)))
  
  message(paste0('Iteration: ',i,'/',k, ' completed'))
}

f_vec <- apply(f_mat, 1, mean)

(f_vs_thresh <- ggplot(mapping = aes(x = thresh, y = f_vec)) + 
    geom_line() + scale_x_continuous(breaks = seq(0.1,0.9,0.05)) +
    labs(x = 'Threshold',
         y = 'F-score')
)


(ind <- which.max(f_vec))
thresh[ind]
f_vec[ind]

(opt.thresh <- thresh[ind])

(f_vs_thresh <- f_vs_thresh + 
    geom_vline(xintercept = opt.thresh, 
               col = 'red',
               linetype = 'dashed') +
    geom_hline(yintercept = f_vec[ind],
               col = 'red',
               linetype = 'dashed')
)  

for(i in 1:k)
{
  if(i ==1)
  {
    plot(ROC_logis[[i]], col = 'blue', main = 'ROC')
  }else{
    plot(ROC_logis[[i]], col = 'blue', add = TRUE)
  }
}
legend_text <- paste0('Logis Iteration: ', 1:5)
legend('bottomright', legend = legend_text, 
       lty = 1, col = 1:5, bty = 'n', lwd = 3) # updated upto this point

 
final_logis_mod <- glmnet(y = train.data$y, 
                          x = as.matrix(train.data[,-1]),
                          family = 'binomial',
                          lambda = opt_lam, alpha = 1)
val_probs <- predict(final_logis_mod, as.matrix(val.data[,-1]),
                     type = 'response')
val_preds <- ifelse(val_probs>opt.thresh, 1, 0)
val_y <- ifelse(val.data$y == 'BBB+',1,0)

cm <- confusionMatrix(factor(val_preds),
                      factor(val_y),
                      mode = 'prec_recall')
# Logistic:
cm
