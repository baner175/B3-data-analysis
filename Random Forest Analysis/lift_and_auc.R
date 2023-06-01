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

rf_full <- ranger(y~., data= train.data,
                  importance="impurity", probability = TRUE,
                  seed=083120181)
imps = sort(rf_full$variable.importance, decreasing = TRUE)

top_x <- function(p, lift)
{
  n_feat <- round(p*(ncol(tab.work2)-1))
  imp.feat <- names(round(imps[1:n_feat],2))
  train.temp <- train.data[,c('y', imp.feat)]
  rfmod = ranger(y~., data=train.temp,
                 importance="impurity",probability = TRUE,
                 seed=083120181)
  prob.mat <- predict(rfmod, 
                      val.data[,-1],
                      type = 'response')$predictions
  roc.out <- suppressMessages(roc(val.data$y,prob.mat[,2]))
  auc_val <- roc.out$auc[1]
  ordered_ind <- order(prob.mat[,2], decreasing = TRUE)
  top_ord <- ordered_ind[1:round(nrow(val.data)*lift)]
  positives <- val.data$y[top_ord] == 'BBB+'
  lift_perc <- 100 * sum(positives)/sum(val.data$y == 'BBB+')
  message(paste0('Calculatinng for p = ',100*p,'%'))
  return(list(lift = lift_perc, auc = auc_val))
}

perc_feat <- seq(0, 0.95, 0.01)
res <- sapply(perc_feat, top_x, lift = 0.2)

auc_plot <- ggplot() + 
  geom_line(mapping = aes(x = perc_feat,
                          y = as.numeric(res[2,]))) + 
  xlab('percentage of features') + ylab('AUC')

lift_plot <- ggplot() + 
  geom_line(mapping = aes(x = perc_feat,
                          y = as.numeric(res[1,]))) + 
  xlab('percentage of features') + ylab('top 20% lift')

auc_plot/lift_plot
