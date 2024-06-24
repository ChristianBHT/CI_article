

##### Plotting #####
library(ggplot2)
library(gridExtra)
library(directlabels)
library(ggthemes)
library(dplyr)

conti_data_type_1_error_500 <- readRDS("C:/CI_article/conti_data_type_1_error_500.rds")
conti_data_type_1_error_800 <- readRDS("C:/CI_article/conti_data_type_1_error_800.rds")
conti_data_type_1_error_2000 <- readRDS("C:/CI_article/conti_data_type_1_error_2000.rds")

sim_2000 <- do.call(rbind, conti_data_type_1_error_2000)
sim_800 <- do.call(rbind, conti_data_type_1_error_800)
sim_500 <- do.call(rbind, conti_data_type_1_error_500)

sim_res <- data.frame(simulation = NULL,
                      obs = NULL,
                      reject_rate_rmse = NULL,
                      reject_rate_mae = NULL)

### Normal ##
reject_rate_rmse <- sum(sim_2000$normal_data_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$normal_data_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Normal",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$normal_data_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$normal_data_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Normal",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$normal_data_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$normal_data_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Normal",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Non Linear ###
reject_rate_rmse <- sum(sim_2000$non_lin_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$non_lin_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$non_lin_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$non_lin_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$non_lin_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$non_lin_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Uniform Noise ###
reject_rate_rmse <- sum(sim_2000$uniform_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$uniform_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$uniform_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$uniform_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$uniform_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$uniform_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Exponential Noise ###
reject_rate_rmse <- sum(sim_2000$exponential_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$exponential_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$exponential_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$exponential_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$exponential_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$exponential_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Poisson Noise ###
reject_rate_rmse <- sum(sim_2000$poisson_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$poisson_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$poisson_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$poisson_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$poisson_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$poisson_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Sinusoidal ###
reject_rate_rmse <- sum(sim_2000$sinus_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$sinus_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$sinus_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$sinus_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$sinus_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$sinus_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### 
sim_res$sample_size <- factor(sim_res$obs, levels = c("500", "800", "2000"))
sim_res$rmse_text <- sprintf("%.2f", sim_res$reject_rate_rmse)
sim_res$mae_text <- sprintf("%.2f", sim_res$reject_rate_mae)

means_rmse_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate_rmse))

means_mae_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate_mae))

plot1 <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate_rmse)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = rmse_text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "RMSE", x = "Sample size", y = "Data generating function") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_text(angle = 30))
print(plot1)

plot2 <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate_mae)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = mae_text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "MAE", x = "Sample size", y = "") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())
print(plot2)


continous_type_I_error_computation <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("continous_type_I_error_computation.eps", plot = continous_type_I_error_computation, width = 8 , height = 5)

##############################################################################################################

conti_data_type_1_error_500_gcm <- readRDS("C:/CI_article/conti_data_type_1_error_500_gcm.rds")
conti_data_type_1_error_800_gcm <- readRDS("C:/CI_article/conti_data_type_1_error_800_gcm.rds")
conti_data_type_1_error_2000_gcm <- readRDS("C:/CI_article/conti_data_type_1_error_2000_gcm.rds")

sim_2000 <- do.call(rbind, conti_data_type_1_error_2000_gcm)
sim_800 <- do.call(rbind, conti_data_type_1_error_800_gcm)
sim_500 <- do.call(rbind, conti_data_type_1_error_500_gcm)

sim_res <- data.frame(simulation = NULL,
                      obs = NULL,
                      reject_rate = NULL)

### Normal ###
reject_rate <- sum(sim_2000$normal_data_pvalue  < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$normal_data_pvalue < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$normal_data_pvalue < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)


### Non Linear ###
reject_rate <- sum(sim_2000$non_lin_data_pvalue < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$non_lin_data_pvalue < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$non_lin_data_pvalue < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)


### Uniform Noise ###
reject_rate <- sum(sim_2000$uniform_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$uniform_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$uniform_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Exponential Noise ###
reject_rate <- sum(sim_2000$exponential_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$exponential_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$exponential_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Poisson Noise ###
reject_rate <- sum(sim_2000$poisson_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$poisson_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$poisson_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Sinusoidal ###
reject_rate <- sum(sim_2000$sinusoidal_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$sinusoidal_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$sinusoidal_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### 
sim_res$sample_size <- factor(sim_res$obs, levels = c("500", "800", "2000"))
sim_res$text <- sprintf("%.2f", sim_res$reject_rate)

means_gcm_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate))

plot <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "GCM", x = "Sample size", y = "Data generating function") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_text(angle = 30))
 
ggsave("continous_type_I_error_gcm.eps", plot = plot, width = 8 , height = 5)


##############################################################################################

conti_data_power_500 <- readRDS("C:/CI_article/conti_data_power_500.rds")
conti_data_power_800 <- readRDS("C:/CI_article/conti_data_power_800.rds")
conti_data_power_2000 <- readRDS("C:/CI_article/conti_data_power_2000.rds")

sim_2000 <- do.call(rbind, conti_data_power_2000)
sim_800 <- do.call(rbind, conti_data_power_800)
sim_500 <- do.call(rbind, conti_data_power_500)

sim_res <- data.frame(simulation = NULL,
                      obs = NULL,
                      reject_rate_rmse = NULL,
                      reject_rate_mae = NULL)

### Normal ##
reject_rate_rmse <- sum(sim_2000$normal_data_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$normal_data_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Normal",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$normal_data_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$normal_data_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Normal",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$normal_data_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$normal_data_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Normal",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Non Linear ###
reject_rate_rmse <- sum(sim_2000$non_lin_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$non_lin_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$non_lin_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$non_lin_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$non_lin_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$non_lin_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Uniform Noise ###
reject_rate_rmse <- sum(sim_2000$uniform_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$uniform_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$uniform_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$uniform_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$uniform_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$uniform_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Exponential Noise ###
reject_rate_rmse <- sum(sim_2000$exponential_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$exponential_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$exponential_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$exponential_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$exponential_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$exponential_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Poisson Noise ###
reject_rate_rmse <- sum(sim_2000$poisson_noise_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$poisson_noise_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$poisson_noise_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$poisson_noise_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$poisson_noise_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$poisson_noise_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### Sinusoidal ###
reject_rate_rmse <- sum(sim_2000$sinus_pvalue1 < 0.05)/nrow(sim_2000)
reject_rate_mae <- sum(sim_2000$sinus_pvalue2 < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 2000,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_800$sinus_pvalue1 < 0.05)/nrow(sim_800)
reject_rate_mae <- sum(sim_800$sinus_pvalue2 < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 800,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

reject_rate_rmse <- sum(sim_500$sinus_pvalue1 < 0.05)/nrow(sim_500)
reject_rate_mae <- sum(sim_500$sinus_pvalue2 < 0.05)/nrow(sim_500)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 500,
                      reject_rate_rmse = reject_rate_rmse,
                      reject_rate_mae = reject_rate_mae)
sim_res <- rbind(sim_res, new_row)

### 
sim_res$sample_size <- factor(sim_res$obs, levels = c("500", "800", "2000"))
sim_res$rmse_text <- sprintf("%.2f", sim_res$reject_rate_rmse)
sim_res$mae_text <- sprintf("%.2f", sim_res$reject_rate_mae)

means_rmse_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate_rmse))

means_mae_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate_mae))

plot1 <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate_rmse)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = rmse_text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "RMSE", x = "Sample size", y = "Data generating function") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_text(angle = 30))
print(plot1)

plot2 <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate_mae)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = mae_text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "MAE", x = "Sample size", y = "") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank())
print(plot2)


continous_power_computation <- grid.arrange(plot1, plot2, ncol = 2)

ggsave("continous_power_computation.eps", plot = continous_power_computation, width = 8 , height = 5)

##############################################################################################################

conti_data_power_500_gcm <- readRDS("C://CI_article/conti_data_type_2_error_500_gcm.rds")
conti_data_power_800_gcm <- readRDS("C://CI_article/conti_data_type_2_error_800_gcm.rds")
conti_data_power_2000_gcm <- readRDS("C://CI_article/conti_data_type_2_error_2000_gcm.rds")

sim_2000 <- do.call(rbind, conti_data_power_2000_gcm)
sim_800 <- do.call(rbind, conti_data_power_800_gcm)
sim_500 <- do.call(rbind, conti_data_power_500_gcm)

sim_res <- data.frame(simulation = NULL,
                      obs = NULL,
                      reject_rate = NULL)

### Normal ###
reject_rate <- sum(sim_2000$normal_data_pvalue  < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$normal_data_pvalue < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$normal_data_pvalue < 0.05)/100
new_row <- data.frame(simulation = "Normal",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Non Linear ###
reject_rate <- sum(sim_2000$non_lin_data_pvalue < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$non_lin_data_pvalue < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$non_lin_data_pvalue < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "NonLinear",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)


### Uniform Noise ###
reject_rate <- sum(sim_2000$uniform_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$uniform_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$uniform_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Uniform Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Exponential Noise ###
reject_rate <- sum(sim_2000$exponential_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$exponential_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$exponential_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Exponential Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Poisson Noise ###
reject_rate <- sum(sim_2000$poisson_noise_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$poisson_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$poisson_noise_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Poisson Noise",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### Sinusoidal ###
reject_rate <- sum(sim_2000$sinusoidal_pvalue  < 0.05)/nrow(sim_2000)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 2000,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_800$sinusoidal_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 800,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

reject_rate <- sum(sim_500$sinusoidal_pvalue  < 0.05)/nrow(sim_800)
new_row <- data.frame(simulation = "Sinusoidal",
                      obs = 500,
                      reject_rate = reject_rate)
sim_res <- rbind(sim_res, new_row)

### 
sim_res$sample_size <- factor(sim_res$obs, levels = c("500", "800", "2000"))
sim_res$text <- sprintf("%.2f", sim_res$reject_rate)

means_gcm_type1 <- sim_res %>%
  group_by(sample_size) %>%
  summarize(mean_value = mean(reject_rate))

plot <- ggplot(sim_res, aes(x = sample_size, y = simulation, fill = reject_rate)) +
  geom_tile() +
  scale_x_discrete(limits = c("500", "800", "2000")) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", midpoint = 0.5, limit = c(0, 1), space = "Lab", name = "Reject Rate") +
  geom_text(aes(label = text), color = "white", size = 4) +
  theme_minimal() +
  labs(title = "GCM", x = "Sample size", y = "Data generating function") +
  guides(fill = "none") +
  theme(text = element_text(size = 16)) +
  theme(axis.text.y = element_text(angle = 30))
plot

ggsave("continous_power_gcm.eps", plot = plot, width = 8 , height = 5)

