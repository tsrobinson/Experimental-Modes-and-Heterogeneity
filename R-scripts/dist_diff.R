## Banded bootstrapping function
# Author: Thomas Robinson (thomas.robinson@politics.ox.ac.uk)

# Function for assessing the covariate balance for n bins of a CATE distribution.
distdiff <- function(data,
                     cate_var,
                     covar,
                     value,
                     bins = 5,
                     ndraw = 1000) {
  
  require(ggplot2)

  effect_low <- range(data[,cate_var])[1]
  effect_high <- range(data[,cate_var])[2]
  

  data <- data[order(data[,cate_var]),]
  
  cate_n <- length(data[,cate_var])
  
  binwidth <- cate_n/bins
  
  bin_estimates <- matrix(, nrow = bins, ncol = 5)
  colnames(bin_estimates) <- c("Mean","Lower.CI","Upper.CI","Min.CATE","Max.CATE")
  for (i in 1:bins) {
    start <- (i-1)*binwidth + 1
    end <- i*binwidth
    df <- data[start:end,]
    estimates <- c()
    for (j in 1:ndraw) {
      bstp <- df[sample(nrow(df),binwidth, replace = TRUE),]
      estimates[length(estimates) + 1] <- sum(bstp[,covar] == value)/nrow(bstp)
    }
    estimates <- sort(estimates)
    bin_estimates[i,1] <- mean(estimates)
    bin_estimates[i,2] <- estimates[ndraw*0.025]
    bin_estimates[i,3] <- estimates[ndraw*0.975]
    bin_estimates[i,4] <- range(df[,cate_var])[1]
    bin_estimates[i,5] <- range(df[,cate_var])[2]
  }
  
  plot_df <- as.data.frame(bin_estimates)
  plot_df$Mid.CATE <- (plot_df$Min.CATE + plot_df$Max.CATE)/2
  
  plot <- ggplot(data = plot_df, x = Mean, y = Mid.CATE) + 
    geom_rect(aes(ymax = Max.CATE, ymin = Min.CATE, xmax = 1, xmin = 0, fill = factor(Min.CATE))) +
    geom_point(aes(x = Mean, y = Mid.CATE)) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_errorbarh(aes(xmax = Upper.CI, xmin = Lower.CI, y = Mid.CATE)) +
    xlim(0,1) +
    labs(x = "Proportion", y = "CATE Range", title = paste0("Estimated proportion ",value," within CATE ranges")) +
    guides(fill=FALSE) +
    # geom_text(x = 0.9, y = effect_high + 0.005, label = paste0("N per bin = ",round(binwidth))) +
    scale_fill_grey()
                  
  
  return(list(matrix = bin_estimates, graph = plot, bin_width = round(binwidth)))
}