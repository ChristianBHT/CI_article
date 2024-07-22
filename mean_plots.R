library(ggplot2)
library(gridExtra)
library(directlabels)
library(ggthemes)
library(dplyr)

###### Log Loss #######
mean1 <- (0.06 + 0.06 + 0.1 + 0.05 + 0.04 + 0.09 + 0.05)/7
mean2 <- (0.09 + 0.1 + 0.05 + 0.06 + 0.1 + 0.05 + 0.04)/7
mean3 <- (0.11 + 0.07 + 0.05 + 0.05 + 0.08 + 0.01 + 0.07)/7
means_log_loss_type1 <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_log_loss_type1 <- data.frame(cbind(means_log_loss_type1, sample))

###### Kappa Score #######
mean1 <- (0.05 + 0.05 + 0.05 + 0.06 + 0.04 + 0.06 + 0.04)/7
mean2 <- (0.1 + 0.06 + 0.06 + 0.01 + 0.1 + 0.04 + 0.03)/7
mean3 <- (0.04 + 0.06 + 0.03 + 0.08 + 0.09 + 0.04 + 0.09)/7
means_kappa_type1 <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_kappa_type1 <- data.frame(cbind(means_kappa_type1, sample))

############ Multinominal Regression ###################
mean1 <- (0.6 + 0.04 + 0.06 + 0.18 + 0.00 + 0.27 + 0.00)/7
mean2 <- (0.84 + 0.05 + 0.12 + 0.31 + 0.00 + 0.22 + 0.00)/7
mean3 <- (1 + 0.15 + 0.1 + 0.7 + 0.03 + 0.52 + 0.03)/7
means_mlog_type1 <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_mlog_type1 <- data.frame(cbind(means_mlog_type1, sample))

############ GCM ##################
mean1 <- (0.2 + 0.1 + 0.07 + 0.02 + 0.06 + 0.08 + 0.05)/7
mean2 <- (0.2 + 0.03 + 0.06 + 0.03 + 0.10 + 0.03 + 0.10)/7
mean3 <- (0.3 + 0.11 + 0.05 + 0.07 + 0.06 + 0.14 + 0.06)/7
means_gcm_type1 <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_gcm_type1 <- data.frame(cbind(means_gcm_type1, sample))

######### KCI #########
mean1 <- (0.9 + 1 + 1 + 0.12 + 0.06 + 0.64 + 0.23)/7
mean2 <- (1 + 1 + 1 + 0.04 + 0.02 + 0.53 + 0.10)/7
mean3 <- (1 + 0.92 + 1 + 0.02 + 0.2 + 0.98 + 0.22)/7
means_kci_type1 <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_kci_type1 <- data.frame(cbind(means_kci_type1, sample))

######### Plotting ##########
means_log_loss_type1$method <- "Log Loss"
means_kappa_type1$method <- "Kappa Score"
means_mlog_type1$method <- "Mlog reg."
means_gcm_type1$method <- "GCM"
means_kci_type1$method <- "KCI"

mean_plot_data <- data.frame(rbind(means_log_loss_type1, means_kappa_type1, means_mlog_type1, means_gcm_type1, means_kci_type1))
colnames(mean_plot_data) <- c('value', 'sample', 'method')
mean_plot_data$sample <- as.factor(mean_plot_data$sample)

p <- ggplot(mean_plot_data, aes(x = sample, y = value, color = method, shape = method)) +
  geom_point(size = 2) +
  geom_line(aes(group = method)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "black") +
  scale_y_log10() +
  theme_light() +
  labs(title = " ",
       x = "Sample Size",
       y = "Type I Error (Log Scale)") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  geom_dl(aes(label = method), method = list(dl.trans(x=x-0.2), "first.qp")) +
  theme(text = element_text(size = 16))
p

ggsave("means_type_I_error.eps", plot = p, width = 8 , height = 5)

############### Rejection Rates ################

###### CCI test #######
mean1 <- (0.34 + 0.74 + 0.33 + 0.19 + 0.99 + 0.41 + 0.99)/7
mean2 <- (0.54 + 0.92 + 0.52 + 0.21 + 1 + 0.67 + 1)/7
mean3 <- (0.99 + 1 + 0.95 + 0.61 + 1 + 0.91 + 1)/7
means_log_loss_power <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_log_loss_power <- data.frame(cbind(means_log_loss_power, sample))

###### Kappa Score #######
mean1 <- (0.32 + 0.56 + 0.26 + 0.37 + 0.99 + 0.54 + 1)/7
mean2 <- (0.42 + 0.75 + 0.24 + 0.48 + 1 + 0.76 + 1)/7
mean3 <- (0.66 + 0.94 + 0.43 + 0.93 + 1 + 0.94 + 1)/7
means_kappa_power <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_kappa_power <- data.frame(cbind(means_kappa_power, sample))

############ Multinominal Regression ###################
mean1 <- (0.23 + 1 + 0.99 + 1 + 1 + 1 + 1)/7
mean2 <- (0.36 + 1 + 1 + 1 + 1 + 1 + 1)/7
mean3 <- (0.8 + 1 + 1 + 1 + 1 + 1 + 1)/7
means_mlog_power <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_mlog_power <- data.frame(cbind(means_mlog_power, sample))

############ GCM ##################
mean1 <- (0.07 + 1 + 0.2 + 0.64 + 1 + 0.63 + 1)/7
mean2 <- (0.11 + 1 + 0.35 + 0.83 + 1 + 0.73 + 1)/7
mean3 <- (0.16 + 1 + 0.77 + 1 + 1 + 1 + 1)/7
means_gcm_power <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_gcm_power <- data.frame(cbind(means_gcm_power, sample))

######### KCI #########
mean1 <- (0.94+ 1 + 0.99 + 1 + 1 + 1 + 1)/7
mean2 <- (1 + 1 + 1 + 1 + 1 + 1 + 1)/7
mean3 <- (1 + 1 + 1 + 1 + 1 + 1 + 1)/7
means_kci_power <- rbind(mean1, mean2, mean3)
sample <- rbind(500, 800, 2000)

means_kci_power <- data.frame(cbind(means_kci_power, sample))

######### Plotting ##########
means_log_loss_power$method <- "Log Loss"
means_kappa_power$method <- "Kappa Score"
means_mlog_power$method <- "Mlog reg."
means_gcm_power$method <- "GCM"
means_kci_power$method <- "KCI"

mean_plot_data <- data.frame(rbind(means_log_loss_power, means_kappa_power, means_mlog_power, means_gcm_power, means_kci_power))
colnames(mean_plot_data) <- c('value', 'sample', 'method')
mean_plot_data$sample <- as.factor(mean_plot_data$sample)


means_rejection_rate <- ggplot(mean_plot_data, aes(x = sample, y = value, color = method, shape = method)) +
  geom_point(size = 3) +
  geom_line(aes(group = method)) +
  scale_y_log10(limits = c(0.5,1)) +
  theme_light() +
  labs(title = " ",
       x = "Sample Size",
       y = "Power") +
  theme(legend.position = "none") +
  geom_dl(aes(label = method), method = list(dl.trans(x=x-0.2), "first.qp")) +
  theme(text = element_text(size = 17))

print(means_rejection_rate)
ggsave("means_rejection_rate.eps", plot = means_rejection_rate, width = 8 , height = 5)

