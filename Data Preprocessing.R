# rm(list = ls())

# Reading the data:
tab <- read.csv('B3DB_classification_extended.tsv', sep = '\t',
                header = T)

tab.work <- tab[, c(7,13:ncol(tab))]
# Removing features with too many NA values:
na.vals <- apply(tab.work[,2:ncol(tab.work)], 2,
                 function(x) sum(is.na(x)))


tab.work2 <- tab.work[,c(1,1+which(na.vals<=30))]

# Removing the rows with NA values:
tab.work2 <- tab.work2[complete.cases(tab.work2),]


names(tab.work2)[1] <- 'y'
tab.work2$y <- as.factor(tab.work2$y)

sd_vec <- apply(tab.work2[,-1], 2, sd)
const_vars <- which(sd_vec == 0) |> names()

tab.work2 <- tab.work2[,!(names(tab.work2) %in% const_vars)]
# View(tab.work2)

split.fun <- function(data, tr.p)
{
  set.seed(123)
  n = nrow(data)
  vec <- 1:n
  n.tr <- floor(n*tr.p)
  n.val <- n - n.tr
  tr.index <- sample(vec, size = n.tr, replace = F)
  data.train <- data[tr.index, ]
  data.val <- data[-tr.index, ]
  l <- list(train.set = data.train, validation.set = data.val)
  return(l)
}

tab.plus <- tab.work2[tab.work2$y == 'BBB+',]
tab.minus <- tab.work2[tab.work2$y == 'BBB-',]
split.plus <- split.fun(data = tab.plus, tr.p = 0.9)
split.minus <- split.fun(data = tab.minus, tr.p = 0.9)

# dim(split.plus$train.set)
# View(split.plus$train.set)

train.plus <- split.plus$train.set
train.minus <- split.minus$train.set
train.data <- rbind(train.plus,train.minus)


val.plus <- split.plus$validation.set
val.minus <- split.minus$validation.set
val.data <- rbind(val.plus, val.minus)

rm('tab', 'tab.work', 'val.plus', 'val.minus')

# write.csv(train.data, 'Training Data.csv', row.names = FALSE)
# write.csv(val.data, 'Validation Data.csv', row.names = FALSE)
