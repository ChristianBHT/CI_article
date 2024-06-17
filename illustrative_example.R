library(xgboost)
library(ggplot2)
library(nortest)
library(goftest)
library(gridExtra)
set.seed(84)
trans_red <- rgb(1, 0, 0, alpha = 0.5)
trans_blue <- rgb(0, 0, 1, alpha = 0.5)

data <- CategorizeInteractiondData(800)

output <- list()

for (j in 1:1000) {
  output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2 + Z1, p = 0.85, objective = 'multi:softprob', num_class = 4)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}
NullDist <- data.frame(do.call(rbind, output))
Test <- TestGenerator(data = data, formula =  Y ~ X + Z2 + Z1, p = 0.85, objective = 'multi:softprob', num_class = 4)
test1 <- Test[1]
test2 <- Test[2]

get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)

output2 <- list()
for (j in 1:1000) {
  output2[[j]] <- TestGenerator(data = data, formula =  Y ~ X + Z2 + Z1 , p = 0.85, objective = 'multi:softprob', num_class = 4)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}
TestDist <- data.frame(do.call(rbind, output2))
combined_data <- rbind(data.frame(Metric = NullDist$Metric1, Type = "NullDist"))
#################################
plot1 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = trans_blue)) +
  xlim(min(c(NullDist$Metric1)), max(c(NullDist$Metric1))) +
  labs(title = "", x = "Log loss", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

combined_data <- rbind(data.frame(Metric = NullDist$Metric2, Type = "NullDist"))

plot2 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = trans_blue, TestDist = "red")) +
  xlim(min(c(NullDist$Metric2)), max(c(NullDist$Metric2))) +
  labs(title = "", x = "Kappa score", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

null_distributions <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("null_distributions.eps", plot = null_distributions, width = 8 , height = 5)

#################################
combined_data <- rbind(data.frame(Metric = NullDist$Metric1, Type = "NullDist"),
                       data.frame(Metric = TestDist$Metric1, Type = "TestDist")
)
plot1 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = trans_blue, TestDist = "red")) +
  xlim(min(c(TestDist$Metric1, NullDist$Metric1)), max(c(TestDist$Metric1, NullDist$Metric1))) +
  labs(title = "", x = "Log loss", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

combined_data <- rbind(data.frame(Metric = NullDist$Metric2, Type = "NullDist"),
                       data.frame(Metric = TestDist$Metric2, Type = "TestDist")
)

plot2 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = trans_blue, TestDist = "red")) +
  xlim(min(c(TestDist$Metric2, NullDist$Metric2)), max(c(TestDist$Metric2, NullDist$Metric2))) +
  labs(title = "", x = "Kappa score", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

null_distributions <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("overlay_distributions.eps", plot = null_distributions, width = 8 , height = 5)
##################################

p_values <- list()
for (i in 1:1000) {
  test1 <- TestDist$Metric1[i]
  test2 <- TestDist$Metric2[i]
  p_values[[i]] <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
  
}

pvalues <- data.frame(do.call(rbind, p_values))

plot1 <- ggplot(pvalues, aes(sample = p_value1)) +
  stat_qq(distribution = qunif) +
  stat_qq_line(distribution = qunif, col = "red") + 
  labs(x = "Theoretical", y = "Sample", title = "Log Loss") + 
  theme(text = element_text(size = 16)) 

plot2 <- ggplot(pvalues, aes(sample = p_value2)) +
  stat_qq(distribution = qunif) +
  stat_qq_line(distribution = qunif, col = "red") + 
  labs(x = "Theoretical", y = "", title = "Kappa Scores") + 
  theme(text = element_text(size = 16)) 


pvalues_under_null <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("pvalues_under_null.eps", plot = pvalues_under_null, width = 8 , height = 5)
#################### Untrue null distribution###############################

output <- list()

for (j in 1:1000) {
  output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = 0.85, objective = 'multi:softprob', num_class = 4)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}
NullDist <- data.frame(do.call(rbind, output))

output2 <- list()
for (j in 1:1000) {
  output2[[j]] <- TestGenerator(data = data, formula = Y ~ X + Z2 , p = 0.85, objective = 'multi:softprob', num_class = 4)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}

TestDist <- data.frame(do.call(rbind, output2))
test1 <- TestDist$Metric1[1]
test2 <- TestDist$Metric2[1]
get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)


combined_data <- rbind(data.frame(Metric = NullDist$Metric1, Type = "NullDist"),
                       data.frame(Metric = TestDist$Metric1, Type = "TestDist")
)
plot1 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = 'white', TestDist = "gray")) +
  xlim(min(c(TestDist$Metric1, NullDist$Metric1)), max(c(TestDist$Metric1, NullDist$Metric1))) +
  labs(title = "", x = "Log loss", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

combined_data <- rbind(data.frame(Metric = NullDist$Metric2, Type = "NullDist"),
                       data.frame(Metric = TestDist$Metric2, Type = "TestDist")
)

plot2 <- ggplot(combined_data, aes(x = Metric, fill = Type)) +
  geom_histogram(color = "black", position = "identity", alpha = 0.5, bins = 15) +
  scale_fill_manual(values = c(NullDist = 'white', TestDist = "gray")) +
  xlim(min(c(TestDist$Metric2, NullDist$Metric2)), max(c(TestDist$Metric2, NullDist$Metric2))) +
  labs(title = "", x = "Kappa score", y = "Frequency") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())

overlay_distributions_false_null <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("overlay_distributions_false_null.eps", plot = overlay_distributions_false_null, width = 8 , height = 5)


p_values <- list()
for (i in 1:1000) {
  test1 <- TestDist$Metric1[i]
  test2 <- TestDist$Metric2[i]
  p_values[[i]] <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
  
}


pvalues <- data.frame(do.call(rbind, p_values))



################# Simulation with low power ###################

data <- multinominal(800, zeta = 1)
data$X <- as.factor(data$X)
data$Y <- as.integer(as.factor(data$Y))-1


output <- list()

for (j in 1:1000) {
  output[[j]] <- NullGenerator(data = data, formula = Y ~ X + Z2, p = 0.85, objective = 'multi:softprob', num_class = 3)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}

NullDist <- data.frame(do.call(rbind, output))

test <- list()
for (j in 1:1000) {
  test[[j]] <- TestGenerator(data = data, formula =  Y ~ X + Z2, p = 0.85, objective = 'multi:softprob', num_class = 3)
  cat(sprintf("Sample: %d\r", j))
  flush.console()
}

TestDist <- data.frame(do.call(rbind, test))

pdf("overlay_distrutions_low_power.pdf", width = 6, height = 4)
par(mfrow = c(1, 2))
hist(NullDist$Metric1, col = trans_blue, ylim = range(0,300), xlim = c(min(c(TestDist$Metric1, NullDist$Metric1)),
                                                                       max(c(TestDist$Metric1, NullDist$Metric1))),
     main = "", xlab = "Log loss", ylab = "Frequency", breaks = 15)
hist(TestDist$Metric1, col = trans_red, add = T,breaks = 15)

hist(NullDist$Metric2, col = trans_blue, ylim = range(0,300), xlim = c(min(c(TestDist$Metric2, NullDist$Metric2)),
                                                                       max(c(TestDist$Metric2, NullDist$Metric2))),
     main = "", xlab = "Kappa scores", ylab = "Frequency", breaks = 15)
hist(TestDist$Metric2, col = trans_red, breaks = 15, add = TRUE)
par(mfrow = c(1, 1))
dev.off()

p_values <- list()
for (i in 1:1000) {
  test1 <- TestDist$Metric1[i]
  test2 <- TestDist$Metric2[i]
  p_values[[i]] <- get_pvalues(objective = 'multi:softprob', NullDist = NullDist, test1_metric = test1, test2_metric = test2)
  
}
pvalues <- data.frame(do.call(rbind, p_values))

pdf("qq_plots_low_power.pdf", width = 6, height = 4)
par(mfrow = c(1, 2))
qqplot(qunif(ppoints(pvalues$p_value1)), pvalues$p_value1, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "P-values from log loss")
abline(0, 1, col = "red")
qqplot(qunif(ppoints(pvalues$p_value2)), pvalues$p_value2, xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", main = "P-values from Kappa scores")
abline(0, 1, col = "red")
par(mfrow = c(1, 1))
dev.off()
