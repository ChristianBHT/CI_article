
library(parallel)
library(pbapply)
library(dplyr)
library(ggplot2)
library(gridExtra)

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterEvalQ(cl, c(library('caret'),
                   library('ipred'),
                   library('xgboost'),
                   library('ggplot2'),
                   library('stats')
))

clusterExport(cl,  varlist=c('multinominal', 'simulateTrigData', 'simulateNonLinearData', 'NullGenerator', 'TestGenerator', 'multi_class_log_loss', 'get_pvalues'), envir=environment())

res1 <- pblapply(cl=cl,1:20, function(i){
  # Generate data
  data <- multinominal(500)
  data$Z1Sqr <- data$Z1^2
  data$Z2Sqr <- data$Z2^2
  data$Z1Cub <- data$Z1^3
  data$Z2Cub <- data$Z2^3
  data$X <- as.factor(data$X)
  data$Y <- as.integer(as.factor(data$Y))-1
  
  # Generate the null distribution
  output <- list()
  for (j in 1:500) {
    output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub , p = 0.80, objective = 'multi:softprob', num_class = 3)
    cat(sprintf("Sample: %d\r", j))
    flush.console()
  }
  NullDist <- data.frame(do.call(rbind, output))
  
  test <- list()
  for (j in 1:500) {
    test[[j]] <- TestGenerator(data = data, formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub , p = 0.80, objective = 'multi:softprob', num_class = 3)
    cat(sprintf("Sample: %d\r", j))
    flush.console()
  }
  TestDist <- data.frame(do.call(rbind, test))
  
  # Calculate p-values
  p_values <- list()
  for (j in 1:500) {
    test1 <- TestDist$Metric1[j]
    test2 <- TestDist$Metric2[j]
    p_values[[j]] <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
  }
  p_value1 <- sapply(p_values, function(x) x["p_value1"])
  p_value2 <- sapply(p_values, function(x) x["p_value2"])
  
  return( c(list(p_value1), list(p_value2)))
})


res2 <- pblapply(cl=cl,1:20, function(i){
  # Generate data
  data <- multinominal(500)
  data$Z1Sqr <- data$Z1^2
  data$Z2Sqr <- data$Z2^2
  data$Z1Cub <- data$Z1^3
  data$Z2Cub <- data$Z2^3
  
  data$X <- as.factor(data$X)
  data$Y <- as.integer(as.factor(data$Y))-1
  
  # Generate the null distribution
  output <- list()
  for (j in 1:500) {
    output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z2Sqr + Z2Cub , p = 0.80, objective = 'multi:softprob', num_class = 3)
    cat(sprintf("Sample: %d\r", j))
    flush.console()
  }
  NullDist <- data.frame(do.call(rbind, output))
  
  # Generate the test distribution
  test <- list()
  for (j in 1:500) {
    test[[j]] <- TestGenerator(data = data, formula = Y ~ X + Z2 + Z2Sqr + Z2Cub , p = 0.80, objective = 'multi:softprob', num_class = 3)
    cat(sprintf("Sample: %d\r", j))
    flush.console()
  }
  TestDist <- data.frame(do.call(rbind, test))
  
  # Calculate p-values
  p_values <- list()
  for (j in 1:500) {
    test1 <- TestDist$Metric1[j]
    test2 <- TestDist$Metric2[j]
    p_values[[j]] <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
  }
  return( c(p_values))
})

save(res1, file = "result_low_power_multinominal_true.RData")
save(res2, file = "result_low_power_multinominal_false.RData")

######### PLOTTING THE RESULTS #########
complete_data <- data.frame(unlist(res1))

log_loss_data <- data.frame(complete_data[1:10000, ])
log_loss_data$test <- factor(rep(1:20, each = 500))
colnames(log_loss_data) <- c('pvalue', 'test')

plot1 <- ggplot(log_loss_data, aes(sample = pvalue, color = test)) +
  geom_qq(distribution = stats::qunif, , size = 0.1)  +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = "Log loss")  +
  theme_minimal() +
  theme(text = element_text(size = 17), legend.position = 'none')
plot1

kappa_data <- data.frame(complete_data[10001:20000, ])
kappa_data$test <- factor(rep(1:20, each = 500))
colnames(kappa_data) <- c('pvalue', 'test')

plot2 <- ggplot(kappa_data, aes(sample = pvalue, color = test)) +
  geom_qq(distribution = stats::qunif, , size = 0.1)  +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", y = " ",
       title = "Kappa scores")  +
  theme_minimal()  +
  theme(text = element_text(size = 17), legend.position = 'none')
plot2

QQplots <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("low_power_multinominal_true.eps", plot = QQplots, width = 8 , height = 5)

complete_data <- data.frame(unlist(res2))

log_loss_data <- data.frame(complete_data[1:10000, ])
log_loss_data$test <- factor(rep(1:20, each = 500))
colnames(log_loss_data) <- c('pvalue', 'test')

plot1 <- ggplot(log_loss_data, aes(sample = pvalue, color = test)) +
  geom_qq(distribution = stats::qunif, , size = 0.1)  +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = "Log loss")  +
  theme_minimal() +
  theme(text = element_text(size = 17), legend.position = 'none')
plot1

kappa_data <- data.frame(complete_data[10001:20000, ])
kappa_data$test <- factor(rep(1:20, each = 500))
colnames(kappa_data) <- c('pvalue', 'test')

plot2 <- ggplot(kappa_data, aes(sample = pvalue, color = test)) +
  geom_qq(distribution = stats::qunif, , size = 0.1)  +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  labs(x = "Theoretical Quantiles", y = " ",
       title = "Kappa scores")  +
  theme_minimal()  +
  theme(text = element_text(size = 17), legend.position = 'none')
plot2

QQplots <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("low_power_multinominal_false.eps", plot = QQplots, width = 8 , height = 5)

