library(ggplot2)
library(dplyr)
rm(list=ls())

library(meta)

#serial interval

study <- c("Guo (2022)", "Madewell (2022)", "Miura (2022)", "Ward (2023)", "UKHSA (2022)")
n <- c(21,57,34,79,34) 
mean <- c(5.6,8.5,10.1,9.5,9.8)
sd <- c(1.5,5,6.1,10.9,19.9)

df <- data.frame(study,n,mean,sd)
# Compute confidence intervals
df <- df %>% 
  rowwise() %>% 
  mutate(
    lower = mean - qnorm(0.975) * (sd / sqrt(n)),
    upper = mean + qnorm(0.975) * (sd / sqrt(n))
  )

# Compute pooled mean and its confidence interval
pooled_weight <- sum(n) / sum(n^2/sd^2)
pooled_mean <- sum(n * mean / sd^2) / sum(n / sd^2)
pooled_sd <- sqrt(1 / sum(n / sd^2))
pooled_lower <- pooled_mean - qnorm(0.975) * pooled_sd
pooled_upper <- pooled_mean + qnorm(0.975) * pooled_sd

# Plot
ggplot(df, aes(x = mean, y = study)) +
  geom_point(size = 2.5, color = "blue") +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, color = "black") +
  geom_segment(aes(x = pooled_lower, xend = pooled_upper, y = -1, yend = -1), size = 1.5, color = "blue") +
  geom_point(aes(x = pooled_mean, y = -1), shape = 23, fill = "blue", size = 10, color = "black") +
  labs(title = "Serial interval", x = "Serial interval", y = NULL) +
  theme_minimal()

