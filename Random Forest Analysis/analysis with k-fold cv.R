# rm(list = ls())

# library(e1071)
library(pROC)
library(MLmetrics)
library(caret)
library(ranger)
library(doParallel)
library(ggplot2)
library(patchwork)
library(vip)
# library(randomForest)

source('Data Preprocessing.R')

100*table(val.data$y)/nrow(val.data)
100*table(train.data$y)/nrow(train.data)


# K-fold cv:
k <- 5
set.seed(1008)
fold_plus <- createFolds(1:nrow(train.plus), k=k)
fold_minus <- createFolds(1:nrow(train.minus), k=k)


specs <- function(thresh, probs, obs)
{
  pred.bin <- ifelse(probs>thresh, 'BBB+', 'BBB-')
  conf <- confusionMatrix(factor(pred.bin), obs,
                          mode = 'prec_recall')
  res  <- conf$byClass[c(1,2,7)]
  return(res)
}

thresh <- seq(0.05, 1, 0.01)

f_mat <- NULL
spec_mat <- NULL
sens_mat <- NULL

ROC_rf <- list()

for(i in 1:k)
{
  ind_test_plus <- fold_plus[[i]]
  ind_test_minus <- fold_minus[[i]]
  
  test <- rbind(train.plus[ind_test_plus,],
                train.minus[ind_test_minus,])
  
  train <- rbind(train.plus[-ind_test_plus,],
                 train.minus[-ind_test_minus,])

  rfmod = ranger(y~., data= train,
                 importance="impurity", probability = TRUE,
                 seed=083120181)
  probs <- predict(rfmod, test[,-1],
                   type="response")$predictions
  res_mat <- sapply(thresh, specs, probs = probs[,2], obs = test[,1])

  f_mat <- cbind(f_mat, res_mat[3,])
  spec_mat <- cbind(spec_mat, res_mat[2,])
  sens_mat <- cbind(sens_mat, res_mat[1,])
  
  ROC_rf[[i]] <- suppressMessages(roc(test$y, probs[,2]))
  
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
    plot(ROC_rf[[i]], col = i, main = 'ROC')
  }else{
    plot(ROC_rf[[i]], col = i, add = TRUE)
  }
}
legend_text <- paste0('RF Iteration: ', 1:5)
legend('bottomright', legend = legend_text, 
       lty = 1, col = 1:5, bty = 'n', lwd = 2)

rfmod = ranger(y~., data= train.data,
               importance="impurity", probability = TRUE,
               seed=083120181)

# Recognizing top 60 features
imps = sort(rfmod$variable.importance, decreasing = TRUE)
# round(imps[1:60],2)

vip(rfmod, num_features = 30, bar = F)

auc.f_sc <-  function(p, train, test)
{
  n_feat <- round(p*(ncol(tab.work2)-1))
  imp.feat <- names(round(imps[1:n_feat],2))
  train.temp <- train[,c('y', imp.feat)]
  rfmod = ranger(y~., data=train.temp,
                 importance="impurity",probability = TRUE,
                 seed=083120181)
  prob.mat <- predict(rfmod, test[,-1],
                      type="response")$predictions
  roc.out <- suppressMessages(roc(test$y,prob.mat[,2]))
  auc <- roc.out$auc[1]
  
  pred.bin <- ifelse(prob.mat[,2]>opt.thresh, 'BBB+', 'BBB-')
  cnf <- confusionMatrix(factor(pred.bin),
                         test$y,
                         mode = 'prec_recall')
  f.sc <- cnf$byClass[7]
  return(c(f.sc, auc))
}
set.seed(1008)
fold_plus <- createFolds(1:nrow(train.plus), k=k)
fold_minus <- createFolds(1:nrow(train.minus), k=k)

perc_feat <- seq(0.05, 0.6, 0.01)

f_mat2 <- NULL
auc_mat <- NULL

registerDoParallel(cores = 4)

for(i in 1:k)
{
  ind_test_plus <- fold_plus[[i]]
  ind_test_minus <- fold_minus[[i]]
  
  test <- rbind(train.plus[ind_test_plus,],
                train.minus[ind_test_minus,])
  
  train <- rbind(train.plus[-ind_test_plus,],
                 train.minus[-ind_test_minus,])
  
  result <- foreach(j = 1:length(perc_feat), 
                    .packages = c('ranger', 'pROC', 'MLmetrics'
                                  , 'caret', 'e1071'),
                    .combine = rbind)%do%
    {
      
      msg <- sprintf('Iteration: %d / %d', j, length(perc_feat))
      message(msg)
      # print(paste('iteration:', i, sep = ' '))
      l = auc.f_sc(p = perc_feat[j], train = train,test = test)
      l
    }
  f_mat2 <- cbind(f_mat2, result[,1])
  auc_mat <- cbind(auc_mat, result[,2])
  message(paste0('Test on Fold: ',i,'/',k, ' completed'))
}

auc_vec <- apply(auc_mat,1,mean)
f_vec <- apply(f_mat2,1,mean)


# Data frame containg the above output is stored into a .csv file:
perf.df <- data.frame('perc_feat' = perc_feat, 
                      'AUC' = result[,1],
                      'F_scores' = result[,2])
# write.csv(perf.df, 'performance-kcv.csv',
#           row.names = FALSE)
perf.df <- read.csv('performance-kcv.csv',
                    header = TRUE)
auc_perc <- ggplot(data = perf.df) +
  geom_line(mapping = aes(x = perc_feat, y = AUC)) +
  scale_x_continuous(breaks = seq(0.05, 0.6, 0.02)) +
  xlab('proportion of most important features used')
# ggsave('AUC.png')

fsc_perc <- ggplot(data = perf.df) +
  geom_line(mapping = aes(x = perc_feat, y = F_scores)) +
  scale_x_continuous(breaks = seq(0.05, 0.6, 0.02)) +
  xlab('proportion of most important features used')
#ggsave('F-score.png')

auc_perc/fsc_perc
n_feat <- round(0.23*(ncol(tab.work2)-1))
imp.feat <- names(round(imps[1:n_feat],2))
#-------------------------------------------------------------------------------
tab.top.feat <- train.data[,c('y', imp.feat)]
rfmod.top <-  ranger(y~., data= tab.top.feat,
                     importance="impurity", probability = TRUE,
                     seed=083120181)
prob_mat.test.top <- predict(rfmod.top, val.data[, imp.feat], 
                             type = 'response')$predictions
pred.test.top <- ifelse(prob_mat.test.top[,2]>opt.thresh, 'BBB+', 'BBB-')
conf.top <- confusionMatrix(factor(pred.test.top), val.data$y,
                            mode = 'prec_recall')
# Random forest:
conf.top

# n_feat <- round(0.1*(ncol(tab.work2)-1))
# imp.feat <- names(round(imps[1:n_feat],2))
# tab.top.feat <- rbind(train.data, val.data)
# tab.top.feat <- tab.top.feat[,c('y', imp.feat)]
# rfmod.top <-  ranger(y~., data= tab.top.feat,
#                      importance="impurity", probability = TRUE,
#                      seed=083120181)
# prob_mat.test.top <- predict(rfmod.top, test.data[, imp.feat], 
#                              type = 'response')$predictions
# pred.test.top <- ifelse(prob_mat.test.top[,1]>opt.thresh, 'BBB-', 'BBB+')
# conf.top <- confusionMatrix(factor(pred.test.top), test.data$y,
#                             mode = 'prec_recall')
# conf.top


# rf.slow <- randomForest(y~., data = tab.top.feat)
# tr <- getTree(rf.slow, k = 1, labelVar = TRUE)
# 
# reprtree::plot.getTree(rf.slow, depth = 5)
