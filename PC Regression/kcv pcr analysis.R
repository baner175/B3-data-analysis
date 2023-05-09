# rm(list = ls())

library(pROC)
library(MLmetrics)
library(caret)
library(ggplot2)
library(Compositional)

source('Data Preprocessing.R')

100*table(val.data$y)/nrow(val.data)
100*table(train.data$y)/nrow(train.data)

X <- as.matrix(train.data[,-1])
pc_res <- prcomp(X, center = TRUE, scale. = TRUE)
var_vals <- pc_res$sdev^2
prop_vars <- var_vals/sum(var_vals)
# plot(prop_vars, type = 'o')
plot(prop_vars[1:200], type = 'o', ylab = 'proportion of variance',
     xlab = 'number of components')

# n_comp <- 50
n_comp <- 100

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


thresh <- seq(0.05, 1, 0.01)

f_mat <- NULL
spec_mat <- NULL
sens_mat <- NULL

ROC_pcr <- list()

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
  X_train <- as.matrix(train[,-1])
  X_test <- as.matrix(test[,-1])
  logi_mod <- glm.pcr(y = train$y,
                      x = X_train,
                      k = n_comp,
                      xnew = X_test)
  probs <- logi_mod$est
  res_mat <- sapply(thresh, specs, probs = probs, obs = test[,1])
  
  f_mat <- cbind(f_mat, res_mat[3,])
  spec_mat <- cbind(spec_mat, res_mat[2,])
  sens_mat <- cbind(sens_mat, res_mat[1,])
  
  ROC_pcr[[i]] <- suppressMessages(roc(test$y, as.numeric(probs)))
  
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
    plot(ROC_pcr[[i]], col = 'green', main = 'ROC')
  }else{
    plot(ROC_pcr[[i]], col = 'green', add = TRUE)
  }
}

train_y <- ifelse(train.data$y == 'BBB+', 1, 0)
val_y <- ifelse(val.data$y == 'BBB+', 1, 0)
final_pcr_model <- glm.pcr(y = train_y,
                            x = as.matrix(train.data[,-1]),
                            k = n_comp,
                            xnew = as.matrix(val.data[,-1]))

val_probs <- final_pcr_model$est
val_preds <- ifelse(val_probs>opt.thresh, 1, 0)

val_conf <- confusionMatrix(factor(val_preds), factor(val_y),
                            mode = 'prec_recall')
# pcr:
val_conf
