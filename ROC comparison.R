for(i in 1:k)
{
  if(i ==1)
  {
    plot(ROC_rf[[i]], col = 'red', main = 'ROC')
  }else{
    plot(ROC_rf[[i]], col = 'red', add = TRUE)
  }
}

for(i in 1:k)
{
  if(i ==1)
  {
    plot(ROC_logis[[i]], col = 'blue', add = TRUE)
  }else{
    plot(ROC_logis[[i]], col = 'blue', add = TRUE)
  }
}

for(i in 1:k)
{
  if(i ==1)
  {
    plot(ROC_pcr[[i]], col = 'green', add = TRUE)
  }else{
    plot(ROC_pcr[[i]], col = 'green', add = TRUE)
  }
}

legend('bottomright', col = c('red', 'blue', 'green'),
       legend = c('Random Forest', 'Logistic LASSO', 'PC Logistic Regression'),
       lty = 1, bty = 'n', lwd = 2)
