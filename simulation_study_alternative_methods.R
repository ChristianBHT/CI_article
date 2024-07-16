library(dplyr)
library(parallel)
library(pbapply)
library(caret)
library(Metrics)
library(DescTools)
library(CondIndTests)
library(GeneralisedCovarianceMeasure)
library(stats)
library(nnet)
library(car)

#####################################################
cl <- makeCluster(detectCores() - 1)

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('CondIndTests')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    data <- CategorizeInteractiondData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    InteractionP_values_kci <- kci$pvalue
    ########################
    
    data <- simulateExpLogData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    ExpLogP_values_kci <- kci$pvalue
    
    ########################
    
    data <- simulateTrigData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X =  data.frame(data$Z2, data$Z1),  gammaApprox = F)
    TrigDataP_values_kci <- kci$pvalue
    
    ########################
    data <- simulatePolyData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    PolyDataP_values_kci <- kci$pvalue
    # ########################
    data <- simulateNonLinearData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    NonLinearP_values_kci <- kci$pvalue
    
    ########################
    data <- simulateComplexCategorization(N)
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    ComplexCategorizationP_values_kci <- kci$pvalue
    
    ########################
    data <- multinominal(N)
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2, data$Z1), GP = F, gammaApprox = F)
    multinominalP_values_kci <- kci$pvalue
    
    ########################
    return(data.frame(
      error = "Type I Error",
      CI_statement = "Y _||_ X | Z2, Z1",
      InteractionP_values_kci = InteractionP_values_kci,
      ExpLogP_values_kci = ExpLogP_values_kci,
      TrigDataP_values_kci = TrigDataP_values_kci,
      PolyDataP_values_kci = PolyDataP_values_kci,
      NonLinearP_values_kci = NonLinearP_values_kci,
      ComplexCategorizationP_values_kci = ComplexCategorizationP_values_kci,
      multinominalP_values_kci = multinominalP_values_kci
    ))
  })
  
  
  filename <- paste0("type_I_error_", N,"_kci.rds")
  saveRDS(results, filename)
}
stopCluster(cl)



for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('CondIndTests')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    data <- CategorizeInteractiondData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    InteractionP_values_kci <- kci$pvalue
    
    ########################
    data <- simulateExpLogData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    ExpLogP_values_kci <- kci$pvalue
    
    ########################
    data <- simulateTrigData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    TrigDataP_values_kci <- kci$pvalue
    
    ########################
    data <- simulatePolyData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    PolyDataP_values_kci <- kci$pvalue
    
    # ########################
    data <- simulateNonLinearData(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    NonLinearP_values_kci <- kci$pvalue
    
    ########################
    data <- simulateComplexCategorization(N)
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    ComplexCategorizationP_values_kci <- kci$pvalue
    
    ########################
    data <- multinominal(N)
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    
    kci <- KCI(Y = data$Y, E = data$X, X = data.frame(data$Z2), GP = F, gammaApprox = F)
    multinominalP_values_kci <- kci$pvalue
    
    ########################
    return(data.frame(
      error = "1 - type II error",
      CI_statement = "Y _||_ X | Z2",
      InteractionP_values_kci = InteractionP_values_kci,
      ExpLogP_values_kci = ExpLogP_values_kci,
      TrigDataP_values_kci = TrigDataP_values_kci,
      PolyDataP_values_kci = PolyDataP_values_kci,
      NonLinearP_values_kci = NonLinearP_values_kci,
      ComplexCategorizationP_values_kci = ComplexCategorizationP_values_kci,
      multinominalP_values_kci = multinominalP_values_kci
    ))
  })
  
  
  filename <- paste0("rejection_rate_", N,"_kci.rds")
  saveRDS(results, filename)
}
stopCluster(cl)


cl <- makeCluster(detectCores() - 1)

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('GeneralisedCovarianceMeasure')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    set.seed(i)
    data <- CategorizeInteractiondData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y,  Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    InteractionP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+1)
    data <- simulateExpLogData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    ExpLogP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+2)
    data <- simulateTrigData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    TrigDataP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+3)
    data <- simulatePolyData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    PolyDataP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+4)
    data <- simulateNonLinearData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y,  Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    NonLinearP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+5)
    data <- simulateComplexCategorization(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    ComplexCategorizationP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+6)
    data <- multinominal(N)
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2, data$Z1),  regr.method = "xgboost")
    multinominalP_values_gcm <- gcm$p.value
    
    ########################
    
    return(data.frame(
      error = "Type I Error",
      CI_statement = "Y _||_ X | Z2, Z1",
      InteractionP_values_gcm = InteractionP_values_gcm,
      ExpLogP_values_gcm = ExpLogP_values_gcm,
      TrigDataP_values_gcm = TrigDataP_values_gcm,
      PolyDataP_values_gcm = PolyDataP_values_gcm,
      NonLinearP_values_gcm = NonLinearP_values_gcm,
      ComplexCategorizationP_values_gcm = ComplexCategorizationP_values_gcm,
      multinominalP_values_gcm = multinominalP_values_gcm
    ))
  })
  
  
  filename <- paste0("type_I_error_", N,"_gcm.rds")
  saveRDS(results, filename)
}
stopCluster(cl)

cl <- makeCluster(detectCores() - 1)

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('GeneralisedCovarianceMeasure')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    set.seed(i)
    data <- CategorizeInteractiondData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y,  Z = data.frame(data$Z2),  regr.method = "xgboost")
    InteractionP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+1)
    data <- simulateExpLogData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2),  regr.method = "xgboost")
    ExpLogP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+3)
    data <- simulateTrigData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2),  regr.method = "xgboost")
    TrigDataP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+4)
    data <- simulatePolyData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2),  regr.method = "xgboost")
    PolyDataP_values_gcm <- gcm$p.value
    
    # ########################
    set.seed(i+5)
    data <- simulateNonLinearData(N)
    gcm <- gcm.test(X = data$X, Y = data$Y,  Z = data.frame(data$Z2),  regr.method = "xgboost")
    NonLinearP_values_gcm <- gcm$p.value
    
    
    ########################
    set.seed(i+6)
    data <- simulateComplexCategorization(N)
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2),  regr.method = "xgboost")
    ComplexCategorizationP_values_gcm <- gcm$p.value
    
    ########################
    set.seed(i+7)
    data <- multinominal(N)
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    
    gcm <- gcm.test(X = data$X, Y = data$Y, Z = data.frame(data$Z2),  regr.method = "xgboost")
    multinominalP_values_gcm <- gcm$p.value
    
    ########################
    
    return(data.frame(
      error = "1 - Type II error",
      CI_statement = "Y _||_ X | Z2",
      InteractionP_values_gcm = InteractionP_values_gcm,
      ExpLogP_values_gcm = ExpLogP_values_gcm,
      TrigDataP_values_gcm = TrigDataP_values_gcm,
      PolyDataP_values_gcm = PolyDataP_values_gcm,
      NonLinearP_values_gcm = NonLinearP_values_gcm,
      ComplexCategorizationP_values_gcm = ComplexCategorizationP_values_gcm,
      multinominalP_values_gcm = multinominalP_values_gcm
    ))
  })
  
  
  filename <- paste0("rejection_rate_", N,"_gcm.rds")
  saveRDS(results, filename)
}
stopCluster(cl)



cl <- makeCluster(detectCores() - 1)

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('stats')
    library('nnet')
    library('car')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    data <- CategorizeInteractiondData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    InteractionP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateExpLogData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    ExpLogP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateTrigData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    TrigDataP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulatePolyData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    PolyDataP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    # ########################
    data <- simulateNonLinearData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    NonLinearP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateComplexCategorization(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    ComplexCategorizationP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    
    ########################
    data <- multinominal(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub  + Z1 + Z1Sqr + Z1Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    multinominalP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    return(data.frame(
      error = "Type I Error",
      CI_statement = "Y _||_ X | Z2, Z1",
      InteractionP_values_mlog = InteractionP_values_mlog,
      ExpLogP_values_mlog = ExpLogP_values_mlog,
      TrigDataP_values_mlog = TrigDataP_values_mlog,
      PolyDataP_values_mlog = PolyDataP_values_mlog,
      NonLinearP_values_mlog = NonLinearP_values_mlog,
      ComplexCategorizationP_values_mlog = ComplexCategorizationP_values_mlog,
      multinominalP_values_mlog = multinominalP_values_mlog
    ))
  })
  
  
  filename <- paste0("type_I_error_", N,"_mlog.rds")
  saveRDS(results, filename)
}
stopCluster(cl)

cl <- makeCluster(detectCores() - 1)

for (N in c(500, 800, 2000)) {
  no_tests <- 100
  
  clusterExport(cl, varlist = c('CategorizeInteractiondData',
                                'simulateExpLogData',
                                'simulateTrigData',
                                'simulatePolyData',
                                'simulateNonLinearData',
                                'simulateNonLinearData',
                                'simulateComplexCategorization',
                                'multinominal',
                                'no_tests',
                                'N'), envir = environment())
  
  clusterEvalQ(cl, {
    library('stats')
    library('nnet')
    library('car')
  })
  
  results <- pblapply(cl=cl, 1:no_tests, function(i){
    ########################
    data <- CategorizeInteractiondData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    InteractionP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateExpLogData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    ExpLogP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateTrigData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    TrigDataP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulatePolyData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    PolyDataP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    # ########################
    data <- simulateNonLinearData(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    NonLinearP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    data <- simulateComplexCategorization(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    ComplexCategorizationP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    
    ########################
    data <- multinominal(N)
    data$Z2Sqr <- data$Z2^2
    data$Z2Cub <- data$Z2^3
    data$Y <- as.integer(as.factor(data$Y))-1
    data$X <- as.integer(as.factor(data$X))-1
    
    multinom_model <- multinom(Y ~ as.factor(X) + Z2 + Z2Sqr + Z2Cub, data = data, trace = FALSE)
    model_anova <- Anova(multinom_model, type = "III")
    multinominalP_values_mlog <- model_anova$'Pr(>Chisq)'[1]
    
    ########################
    return(data.frame(
      error = "1 - Type II Error",
      CI_statement = "Y _||_ X | Z2",
      InteractionP_values_mlog = InteractionP_values_mlog,
      ExpLogP_values_mlog = ExpLogP_values_mlog,
      TrigDataP_values_mlog = TrigDataP_values_mlog,
      PolyDataP_values_mlog = PolyDataP_values_mlog,
      NonLinearP_values_mlog = NonLinearP_values_mlog,
      ComplexCategorizationP_values_mlog = ComplexCategorizationP_values_mlog,
      multinominalP_values_mlog = multinominalP_values_mlog
    ))
  })
  
  
  filename <- paste0("rejection_rate_", N,"_mlog.rds")
  saveRDS(results, filename)
}
stopCluster(cl)
