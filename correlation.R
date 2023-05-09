library(ggplot2)
library(ggcorrplot)
library(patchwork)
source('Data Preprocessing.R')

100*table(val.data$y)/nrow(val.data)
100*table(train.data$y)/nrow(train.data)

# Correlation heatmap of some of the predictors:
vec <- seq(1,50,3)
corr_pearson <- cor(tab.work2[,2+vec])
corr_spearman <- cor(tab.work2[,2+vec], method = 'spearman')
corr_kendall <- cor(tab.work2[,2+vec], method = 'kendall')

pearson_plt <- ggcorrplot(corr_pearson, hc.order = TRUE, type = "full",
                          lab = FALSE) + 
  ggtitle("Pearson's Correlation Coefficient")
spearman_plt <- ggcorrplot(corr_spearman, hc.order = TRUE, type = "full",
                          lab = FALSE) + 
  ggtitle("Spearman's Correlation Coefficient")
kendall_plt <- ggcorrplot(corr_kendall, hc.order = TRUE, type = "full",
                          lab = FALSE) + 
  ggtitle("Kendall's Correlation Coefficient")

(pearson_plt | kendall_plt )

# melted <- reshape2::melt(corr_pearson)
# cor_plt_pearson <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
#     geom_tile()
# melted <- reshape2::melt(corr_spearman)
# cor_plt_spearman <- ggplot(data = melted, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# melted <- reshape2::melt(corr_kendall)
# cor_plt_kendall <- ggplot(data = melted, aes(x=Var1, y=Var2, fill=value)) + 
#   geom_tile()
# 
# cor_plt_pearson | (cor_plt_spearman / cor_plt_kendall)


# # Correlation heatmap of some of the predictors:
# vec <- seq(1,50,3)
# corr <- cor(tab.work2[,3+vec])
# melted_corr <- reshape2::melt(corr)
# (cor_plt2 <- ggplot(data = melted_corr, aes(x=Var1, y=Var2, fill=value)) + 
#     geom_tile())

# (cor_plt <- ggpubr::ggarrange(cor_plt1, cor_plt2, ncol = 2, nrow = 1))
