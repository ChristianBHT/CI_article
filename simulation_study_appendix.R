library(parallel)
library(pbapply)
library(dplyr)
library(xgboost)
library(caret)
library(Metrics)
library(diptest)


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
                                'ContinousInteractionData',
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
    
    data <- normal_data(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3

    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))

    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    normal_data_pvalue1 <- p_values[1] 
    normal_data_pvalue2 <- p_values[2] 
    
    #######################################################################################
    data <- hierarchical_data(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
      
    output <- list()
    for (j in 1:R) {
        output[[j]] <- NullGenerator(data = data,
                                     formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                     p = p)
        cat(sprintf("Sample: %d\r", j))
        flush.console()
      }
    NullDist <- data.frame(do.call(rbind, output))
      
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p)
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    hierarchical_pvalue1 <- p_values[1] 
    hierarchical_pvalue2 <- p_values[2] 
    
    ################################################################
    
    data <- skewed_data(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    skewed_pvalue1 <- p_values[1] 
    skewed_pvalue2 <- p_values[2] 
    
    ################################################################
    
    data <- non_lin_normal(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    non_lin_pvalue1 <- p_values[1] 
    non_lin_pvalue2 <- p_values[2] 
    
    ################################################################
    data <- uniform_noise(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    uniform_noise_pvalue1 <- p_values[1] 
    uniform_noise_pvalue2 <- p_values[2] 
    ################################################################
    data <- uniform_noise(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    uniform_noise_pvalue1 <- p_values[1] 
    uniform_noise_pvalue2 <- p_values[2] 
    ################################################################
    data <- exponential_noise(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    exponential_noise_pvalue1 <- p_values[1] 
    exponential_noise_pvalue2 <- p_values[2] 
    
    ################################################################
    data <- poisson_noise(N)
    data$Z1Sqr <- data$Z1^2
    data$Z2Sqr <- data$Z2^2
    data$Z1Cub <- data$Z1^3
    data$Z2Cub <- data$Z2^3  
    
    output <- list()
    for (j in 1:R) {
      output[[j]] <- NullGenerator(data = data,
                                   formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                                   p = p)
      cat(sprintf("Sample: %d\r", j))
      flush.console()
    }
    NullDist <- data.frame(do.call(rbind, output))
    
    test <- TestGenerator(data = data,
                          formula =  Y ~ X + Z2 + Z2Sqr + Z2Cub + Z1 + Z1Sqr + Z1Cub,
                          p = p) 
    
    p_values <- get_pvalues(objective = 'reg:squarederror', NullDist = NullDist, test1_metric = test[1], test2_metric = test[2])
    poisson_noise_pvalue1 <- p_values[1] 
    poisson_noise_pvalue2 <- p_values[2] 
    
    ################################################################
    
    return(data.frame(
      test_number = i,
      error = "Type I error",
      CI_statement = "Y _||_ X | Z2, Z1",
      normal_data_pvalue1 = normal_data_pvalue1,
      normal_data_pvalue2 = normal_data_pvalue2,
      hierarchical_pvalue1 = hierarchical_pvalue1,
      hierarchical_pvalue2 = hierarchical_pvalue2,
      skewed_pvalue1 = skewed_pvalue1,
      skewed_pvalue2 = skewed_pvalue2,
      non_lin_pvalue1 = non_lin_pvalue1,
      non_lin_pvalue2 = non_lin_pvalue2,
      uniform_noise_pvalue1 = uniform_noise_pvalue1,
      uniform_noise_pvalue2 = uniform_noise_pvalue2,
      exponential_noise_pvalue1 = exponential_noise_pvalue1,
      exponential_noise_pvalue2 = exponential_noise_pvalue2,
      poisson_noise_pvalue1 = poisson_noise_pvalue1,
      poisson_noise_pvalue2 = poisson_noise_pvalue2
    ))
  })
  # Save output
  filename <- paste0("conti_data_type_1_error_", N,".rds")
  saveRDS(results, filename)
}
stopCluster(cl)
