rm(list = ls())

library(e1071)
library(pROC)
library(ranger)
library(MLmetrics)
library(caret)
library(doParallel)
library(ggplot2)
library(vip)
library(randomForest)
# Reading the data:
tab <- read.csv('B3DB_classification_extended.tsv', sep = '\t',
                header = T)

tab.work <- tab[, c(7,13:ncol(tab))]
dim(tab.work)
# Removing features with too many NA values:
na.vals <- apply(tab.work[,2:ncol(tab.work)], 2,
                 function(x) sum(is.na(x)))


tab.work2 <- tab.work[,c(1,1+which(na.vals<=30))]
dim(tab.work2)

# Removing the rows with NA values:
tab.work2 <- tab.work2[complete.cases(tab.work2),]
dim(tab.work2)


names(tab.work2)[1] <- 'y'
View(tab.work2)

tab.work2$y <- as.factor(tab.work2$y)
View(tab.work2)

# Correlation heatmap of some of the predictors:
vec <- seq(1,50,3)
corr <- cor(tab.work2[,2+vec])
melted_corr <- reshape2::melt(corr)
(cor_plt1 <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile())

# Correlation heatmap of some of the predictors:
vec <- seq(1,50,3)
corr <- cor(tab.work2[,3+vec])
melted_corr <- reshape2::melt(corr)
(cor_plt2 <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile())

(cor_plt <- ggpubr::ggarrange(cor_plt1, cor_plt2, ncol = 2, nrow = 1))

write.csv(tab.work2, 'C:/Users/Aritra/OneDrive/Desktop/Fall 2022/RA work/R-codes/B3_working.csv',
          row.names = F)

split.fun <- function(data, tr.p, val.p)
{
  set.seed(123)
  n = nrow(data)
  vec <- 1:n
  n.tr <- floor(n*tr.p)
  n.val <- floor(n*val.p)
  n.ts <- n - n.tr - n.val
  tr.index <- sample(vec, size = n.tr, replace = F)
  val.index <- sample(vec[-tr.index], size = n.val, replace = F)
  data.train <- data[tr.index, ]
  data.val <- data[val.index, ]
  data.test <- data[-c(tr.index, val.index), ]
  l <- list(train.set = data.train, validation.set = data.val, 
            test.set = data.test)
  return(l)
}

tab.plus <- tab.work2[tab.work2$y == 'BBB+',]
tab.minus <- tab.work2[tab.work2$y == 'BBB-',]
split.plus <- split.fun(data = tab.plus, tr.p = 0.5, val.p = 0.1)
split.minus <- split.fun(data = tab.minus, tr.p = 0.5, val.p = 0.1)

dim(split.plus$train.set)
View(split.plus$train.set)

train.plus <- split.plus$train.set
train.minus <- split.minus$train.set
train.data <- rbind(train.plus,train.minus)
write.csv(train.data, 'C:/Users/Aritra/OneDrive/Desktop/Fall 2022/RA work/R-codes/B3_train.csv',
          row.names = F)
# View(train.data)
100*table(train.data$y)/nrow(train.data)

val.plus <- split.plus$validation.set
val.minus <- split.minus$validation.set
val.data <- rbind(val.plus, val.minus)
write.csv(val.data, 'C:/Users/Aritra/OneDrive/Desktop/Fall 2022/RA work/R-codes/B3_validation.csv',
          row.names = F)
100*table(val.data$y)/nrow(val.data)

test.plus <- split.plus$test.set
test.minus <- split.minus$test.set
test.data <- rbind(test.plus, test.minus)
write.csv(test.data, 'C:/Users/Aritra/OneDrive/Desktop/Fall 2022/RA work/R-codes/B3_test.csv',
          row.names = F)
100*table(test.data$y)/nrow(test.data)

# Fitting a random forest on the training set:
rfmod = ranger(y~., data= train.data,
               importance="impurity", probability = TRUE,
               seed=083120181)
preds = predict(rfmod, val.data[,-1],
                type="response")$predictions[,2]
prob.mat <- predict(rfmod, val.data[,-1],
                    type="response")$predictions
thresh <- seq(0.1, 0.9, 0.01)

f.score <- function(thresh)
{
  pred.bin <- ifelse(prob.mat[,1]>thresh, 'BBB-', 'BBB+')
  
  conf <- confusionMatrix(factor(pred.bin), val.data$y,
                          mode = 'prec_recall')
  f1.score  <- conf$byClass[7]
  return(f1.score)
}

f_vec <- sapply(thresh, f.score)
# plot(x = thresh, y = f_vec, type = 'o')
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

ggsave('F-scores vs threshold.png')

# The F-score on the test data with the optimal threshold:
probs.test.mat <- predict(rfmod, test.data[,-1],
                          type = 'response')$predictions
pred.test.bin <- ifelse(probs.test.mat[,1]>opt.thresh, 'BBB-', 'BBB+')
conf <- confusionMatrix(factor(pred.test.bin), test.data$y,
                        mode = 'prec_recall')
conf$byClass[7]

# ROC and AUC for the fitted random forest on validation set:
roc.out <- roc(val.data$y, preds)
plot(roc.out)
roc.out$auc
roc.out$auc[1]

# Recognizing top 60 features
imps = sort(rfmod$variable.importance, decreasing=TRUE)
round(imps[1:60],2)

vip(rfmod, num_features = 30, bar = F)

auc.f_sc <-  function(p)
{
  n_feat <- round(p*(ncol(tab.work2)-1))
  imp.feat <- names(round(imps[1:n_feat],2))
  # tab.temp <- tab.work2[-test,c('y',imp.feat)]
  tab.temp <- rbind(train.data, val.data)
  tab.temp <- tab.temp[,c('y', imp.feat)]
  set.seed(789)
  tr <- sample(1:nrow(tab.temp), size = nrow(tab.temp)*0.7, 
               replace = F)
  rfmod = ranger(y~., data=tab.temp[tr,],
                 importance="impurity",probability = TRUE,
                 seed=083120181)
  preds = predict(rfmod, tab.temp[-tr,],
                  type="response")$predictions[,2]
  roc.out <- roc(tab.temp$y[-tr],preds)
  auc <- roc.out$auc[1]
  prob.mat <- predict(rfmod, tab.temp[-tr,],
                      type="response")$predictions
  pred.bin <- ifelse(prob.mat[,1]>opt.thresh, 'BBB-', 'BBB+')
  cnf <- confusionMatrix(factor(pred.bin),
                         tab.temp$y[-tr],
                         mode = 'prec_recall')
  f.sc <- cnf$byClass[7]
  return(list('auc' =  auc, 'F-score' = f.sc))
}

# Plotting the AUC and F-scores against the percent of most 
# important features used
perc_feat <- seq(0.05, 0.6, 0.01)

registerDoParallel(cores = 4)
result <- foreach(i = 1:length(perc_feat), 
                  .packages = c('ranger', 'pROC', 'MLmetrics'
                                , 'caret', 'e1071'),
                  .combine = rbind)%do%
  {
    l = suppressMessages(auc.f_sc(p = perc_feat[i]))
    msg <- sprintf('Iteration: %d / %d', i, length(perc_feat))
    message(msg)
    # print(paste('iteration:', i, sep = ' '))
    vals <- c(l$auc, l$'F-score')
    vals
  }

# Data frame containg the above output is stored into a .csv file:
perf.df <- data.frame('perc_feat' = perc_feat, 
                      'AUC' = result[,1],
                      'F_scores' = result[,2])
write.csv(perf.df, "C:/Users/Aritra/OneDrive/Desktop/Fall 2022/RA work/R-codes/performance.csv",
          row.names = F)

ggplot(data = perf.df) +
  geom_line(mapping = aes(x = perc_feat, y = AUC)) +
  scale_x_continuous(breaks = seq(0.05, 0.6, 0.02)) 
ggsave('AUC.png')

ggplot(data = perf.df) +
  geom_line(mapping = aes(x = perc_feat, y = F_scores)) +
  scale_x_continuous(breaks = seq(0.05, 0.6, 0.02))
ggsave('F-score.png')

n_feat <- round(0.1*(ncol(tab.work2)-1))
imp.feat <- names(round(imps[1:n_feat],2))
tab.top.feat <- train.data[,c('y', imp.feat)]
rfmod.top <-  ranger(y~., data= tab.top.feat,
                      importance="impurity", probability = TRUE,
                      seed=083120181)
prob_mat.test.top <- predict(rfmod.top, test.data[, imp.feat], 
                              type = 'response')$predictions
pred.test.top <- ifelse(prob_mat.test.top[,1]>opt.thresh, 'BBB-', 'BBB+')
conf.top <- confusionMatrix(factor(pred.test.top), test.data$y,
                             mode = 'prec_recall')
conf.top

n_feat <- round(0.1*(ncol(tab.work2)-1))
imp.feat <- names(round(imps[1:n_feat],2))
tab.top.feat <- rbind(train.data, val.data)
tab.top.feat <- tab.top.feat[,c('y', imp.feat)]
rfmod.top <-  ranger(y~., data= tab.top.feat,
                     importance="impurity", probability = TRUE,
                     seed=083120181)
prob_mat.test.top <- predict(rfmod.top, test.data[, imp.feat], 
                             type = 'response')$predictions
pred.test.top <- ifelse(prob_mat.test.top[,1]>opt.thresh, 'BBB-', 'BBB+')
conf.top <- confusionMatrix(factor(pred.test.top), test.data$y,
                            mode = 'prec_recall')
conf.top


rf.slow <- randomForest(y~., data = tab.top.feat)
tr <- getTree(rf.slow, k = 1, labelVar = TRUE)

reprtree::plot.getTree(rf.slow, depth = 5)
