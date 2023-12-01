# ROC function  -----------------------------------------------------------
ROC_function <- function(predictions_real_df) {
  
  # Curva ROC ---------------------------------------------------------------
  curva_ROC <- roc(predictions_real_df$real, predictions_real_df$predictions)
  
  #Coordenadas del "top-left" de la curva ROC 
  ROC_thresh <- coords(curva_ROC, x = "best", best.method = "closest.topleft")
  
  ROC_thresh
}

graph_roc <- function(predictions_real_df, ROC_thresh){
  # Curva ROC ---------------------------------------------------------------
  curva_ROC <- roc_curve(data = predictions_real_df, truth = real, predictions)
  
  predictions_real_df  <- predictions_real_df  %>% 
  mutate(class_ROC_Thresh = factor(ifelse(predictions > ROC_thresh$threshold, 1, 0)))
  FPR_op<-mean(predictions_real_df$class_ROC_Thresh[predictions_real_df$real == 0] == 0)
  TPR_op<-mean(predictions_real_df$class_ROC_Thresh[predictions_real_df$real == 1] == 1)
  #Gráfica 
  plot(curva_ROC, print.auc = TRUE, legacy.axes = TRUE)
  points(x= FPR_op, 
         y=TPR_op, 
         cex=4, pch=20, col='green')
  
}
