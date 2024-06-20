library(parallel)
library(pbapply)
library(dplyr)
library(xgboost)
library(caret)
library(Metrics)
library(diptest)
library(GeneralisedCovarianceMeasure)

################################################################################

cl <- makeCluster(detectCores() - 1)
for (N in c(500, 800, 2000)) {
  p <- 0.825
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator',
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'normal_data',
                                'non_lin_normal',
                                'uniform_noise',
                                'exponential_noise',
                                'poisson_noise',
                                'sinusoidal',
                                'N',
                                'R',
                                'p',
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    set.seed(i)
    
    # data <- normal_data(N)
    # 
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # 
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1,
    #                       p = p)
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # normal_data_pvalue1 <- p_values[1]
    # normal_data_pvalue2 <- p_values[2]
    # 
    # #######################################################################################
    # data <- hierarchical_data(N)
    # data$Group <- as.factor(data$Group)
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1 + Group,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1 + Group,
    #                       p = p)
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # hierarchical_pvalue1 <- p_values[1]
    # hierarchical_pvalue2 <- p_values[2]
    # 
    # 
    # ################################################################
    # data <- non_lin_normal(N)
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1,
    #                       p = p)
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # 
    # non_lin_pvalue1 <- p_values[1]
    # non_lin_pvalue2 <- p_values[2]
    # ################################################################
    # data <- uniform_noise(N)
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1,
    #                       p = p)
    # 
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # 
    # uniform_noise_pvalue1 <- p_values[1]
    # uniform_noise_pvalue2 <- p_values[2]
    # ################################################################
    # data <- exponential_noise(N)
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # 
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1,
    #                       p = p)
    # 
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # exponential_noise_pvalue1 <- p_values[1]
    # exponential_noise_pvalue2 <- p_values[2]
    # 
    # ################################################################
    # data <- poisson_noise(N)
    # 
    # output <- list()
    # for (j in 1:R) {
    #   output[[j]] <- NullGenerator(data = data,
    #                                formula =  Y ~ X + Z2 + Z1,
    #                                p = p)
    #   cat(sprintf("Sample: %d\r", j))
    #   flush.console()
    # }
    # NullDist <- data.frame(do.call(rbind, output))
    # 
    # test <- TestGenerator(data = data,
    #                       formula =  Y ~ X + Z2 + Z1,
    #                       p = p)
    # 
    # p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    # poisson_noise_pvalue1 <- p_values[1]
    # poisson_noise_pvalue2 <- p_values[2]
    
    ################################################################
    data <- sinusoidal(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z1,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z1,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    
    
    sinusoidal_pvalue1 <- p_values[1]
    sinusoidal_pvalue2 <- p_values[2]
    
    ################################################################
    
    return(data.frame(
      test_number = i,
      error = "Type I error",
      CI_statement = "Y _||_ X | Z2, Z1",
      # normal_data_pvalue1 = normal_data_pvalue1,
      # normal_data_pvalue2 = normal_data_pvalue2,
      # hierarchical_pvalue1 = hierarchical_pvalue1,
      # hierarchical_pvalue2 = hierarchical_pvalue2,
      # non_lin_pvalue1 = non_lin_pvalue1,
      # non_lin_pvalue2 = non_lin_pvalue2,
      # uniform_noise_pvalue1 = uniform_noise_pvalue1,
      # uniform_noise_pvalue2 = uniform_noise_pvalue2,
      # exponential_noise_pvalue1 = exponential_noise_pvalue1,
      # exponential_noise_pvalue2 = exponential_noise_pvalue2,
      # poisson_noise_pvalue1 = poisson_noise_pvalue1,
      # poisson_noise_pvalue2 = poisson_noise_pvalue2,
      sinus_pvalue1 = sinusoidal_pvalue1,
      sinus_pvalue2 = sinusoidal_pvalue2
    ))
  })
  # Save output
  filename <- paste0("sinusoidal_data_type_1_error_", N,".rds")
  saveRDS(results, filename)
}



for (N in c(500, 800, 2000)) {
  p <- 0.825
  R <- 1000
  no_tests <- 100
  
  clusterExport(cl, varlist = c('NullGenerator',
                                'TestGenerator',
                                'multi_class_log_loss',
                                'get_pvalues',
                                'normal_data',
                                'non_lin_normal',
                                'uniform_noise',
                                'exponential_noise',
                                'poisson_noise',
                                'sinusoidal',
                                'N',
                                'R',
                                'p',
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(caret)
    library(Metrics)
    library(dplyr)
    library(xgboost)
    library(diptest)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    set.seed(100 + i)
    data <- normal_data(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    normal_data_pvalue1 <- p_values[1]
    normal_data_pvalue2 <- p_values[2]
    
    #######################################################################################
    data <- hierarchical_data(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    hierarchical_pvalue1 <- p_values[1]
    hierarchical_pvalue2 <- p_values[2]
    
    
    ################################################################
    data <- non_lin_normal(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    non_lin_pvalue1 <- p_values[1]
    non_lin_pvalue2 <- p_values[2]
    
    ################################################################
    data <- uniform_noise(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    uniform_noise_pvalue1 <- p_values[1]
    uniform_noise_pvalue2 <- p_values[2]
    ################################################################
    data <- uniform_noise(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    uniform_noise_pvalue1 <- p_values[1]
    uniform_noise_pvalue2 <- p_values[2]
    ################################################################
    data <- exponential_noise(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    exponential_noise_pvalue1 <- p_values[1]
    exponential_noise_pvalue2 <- p_values[2]
    
    ################################################################
    data <- poisson_noise(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    poisson_noise_pvalue1 <- p_values[1]
    poisson_noise_pvalue2 <- p_values[2]
    
    ################################################################
    data <- sinusoidal(N)
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2,
                          p = p)
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    
    sinusoidal_pvalue1 <- p_values[1]
    sinusoidal_pvalue2 <- p_values[2]
    ################################################################
    
    return(data.frame(
      test_number = i,
      error = "1 - Type II error",
      CI_statement = "Y _||_ X | Z2",
      # normal_data_pvalue1 = normal_data_pvalue1,
      # normal_data_pvalue2 = normal_data_pvalue2,
      # hierarchical_pvalue1 = hierarchical_pvalue1,
      # hierarchical_pvalue2 = hierarchical_pvalue2,
      # non_lin_pvalue1 = non_lin_pvalue1,
      # non_lin_pvalue2 = non_lin_pvalue2,
      # uniform_noise_pvalue1 = uniform_noise_pvalue1,
      # uniform_noise_pvalue2 = uniform_noise_pvalue2,
      # exponential_noise_pvalue1 = exponential_noise_pvalue1,
      # exponential_noise_pvalue2 = exponential_noise_pvalue2,
      # poisson_noise_pvalue1 = poisson_noise_pvalue1,
      # poisson_noise_pvalue2 = poisson_noise_pvalue2,
      sinus_pvalue1 = sinusoidal_pvalue1,
      sinus = sinusoidal_pvalue2
    ))
  })
  # Save output
  filename <- paste0("sinusoidal_data_power_", N,".rds")
  saveRDS(results, filename)
}
stopCluster(cl)


cl <- makeCluster(detectCores() - 1)
for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('normal_data',
                                'non_lin_normal',
                                'uniform_noise',
                                'hierarchical_data',
                                'exponential_noise',
                                'poisson_noise',
                                'N',
                                'R',
                                'p',
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(GeneralisedCovarianceMeasure)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    set.seed(i)
    data <- normal_data(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    normal_data_pvalue <- gcmtest$p.value
    
    #######################################################################################
    set.seed(i + 1)
    data <- hierarchical_data(N)
    conditional <- data.frame(data$Z1, data$Z2, data$Group)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    hierarchical_data_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 2)
    data <- non_lin_normal(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    non_lin_data_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 3)
    data <- uniform_noise(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    uniform_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 4)
    data <- exponential_noise(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    exponential_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 5)
    data <- poisson_noise(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    poisson_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 6)
    data <- sinusoidal(N)
    conditional <- data.frame(data$Z1, data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    sinusoidal_pvalue <- gcmtest$p.value
    
    ################################################################
    
    return(data.frame(
      test_number = i,
      error = "Type I error",
      CI_statement = "Y _||_ X | Z2, Z1",
      normal_data_pvalue = normal_data_pvalue,
      hierarchical_data_pvalue = hierarchical_data_pvalue,
      non_lin_data_pvalue = non_lin_data_pvalue, 
      uniform_noise_pvalue = uniform_noise_pvalue,
      exponential_noise_pvalue = exponential_noise_pvalue,
      poisson_noise_pvalue = poisson_noise_pvalue,
      sinusoidal_pvalue = sinusoidal_pvalue
    ))
  })
  # Save output
  filename <- paste0("conti_data_type_1_error_", N,"_gcm.rds")
  saveRDS(results, filename)
}

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('normal_data',
                                'non_lin_normal',
                                'uniform_noise',
                                'hierarchical_data',
                                'exponential_noise',
                                'poisson_noise',
                                'N',
                                'R',
                                'p',
                                'no_tests'), envir = environment())
  
  clusterEvalQ(cl, {
    library(GeneralisedCovarianceMeasure)
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    set.seed(i)
    data <- normal_data(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    normal_data_pvalue <- gcmtest$p.value
    
    #######################################################################################
    set.seed(i + 10)
    data <- hierarchical_data(N)
    conditional <- data.frame(data$Z2, data$Group)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    hierarchical_data_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 20)
    data <- non_lin_normal(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    non_lin_data_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 30)
    data <- uniform_noise(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    uniform_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 40)
    data <- exponential_noise(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    exponential_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 50)
    data <- poisson_noise(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    poisson_noise_pvalue <- gcmtest$p.value
    
    ################################################################
    set.seed(i + 60)
    data <- sinusoidal(N)
    conditional <- data.frame(data$Z2)
    gcmtest <- gcm.test(X = data$X, Y = data$Y, Z = conditional)
    sinusoidal_pvalue <- gcmtest$p.value
    
    ################################################################
    return(data.frame(
      test_number = i,
      error = "1 - Type II error",
      CI_statement = "Y _||_ X | Z2",
      normal_data_pvalue = normal_data_pvalue,
      hierarchical_data_pvalue = hierarchical_data_pvalue,
      non_lin_data_pvalue = non_lin_data_pvalue, 
      uniform_noise_pvalue = uniform_noise_pvalue,
      exponential_noise_pvalue = exponential_noise_pvalue,
      poisson_noise_pvalue = poisson_noise_pvalue,
      sinusoidal_pvalue = sinusoidal_pvalue
    ))
  })
  # Save output
  filename <- paste0("conti_data_type_2_error_", N,"_gcm.rds")
  saveRDS(results, filename)
}

stopCluster(cl)



