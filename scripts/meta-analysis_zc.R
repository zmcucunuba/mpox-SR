
library(tidyverse)
library(meta)
library(metafor)
rm(list = ls())

#serial interval

study <- c("Guo (2022)", "Madewell (2022)", "Miura (2022)", "Ward (2023)", "UKHSA (2022)")
n <- c(21,57,34,79,34) 
mean <- c(5.6,8.5,10.1,9.5,9.8)
sd <- c(1.5,5,6.1,10.9,19.9)
df <- data.frame(study,n,mean,sd)

m1 <- metamean(n = n,
               mean = mean,
               sd = sd,
               studlab = study,
               data = df,
               sm = 'MLN',
               random = T,
               common = F,
               warn = F,
               prediction = F,
               title = "Serial interval")

str(m1)

f1 <- forest(m1, col.diamond = "blue", col.diamond.lines = "black", xlab = "Serial interval")

str(f1)

# Additional data from your meta-analysis object
random_effect <- m1$TE.random
random_effect_ci_lower <- m1$lower.random
random_effect_ci_upper <- m1$upper.random
I2 <- m1$I2
Q <- m1$Q
pval_Q <- m1$pval.Q

# Prepare the main data frame for the plot
plot_data <- data.frame(study, mean, ci_lower, ci_upper, w = m1$w.random)

# Create the base plot
p <- ggplot(plot_data, aes(x = mean, y = study, size = w)) +
  geom_point() +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
  xlab("Mean Effect Size") +
  theme_minimal()

# Add random effects model as a point with a confidence interval
p <- p + geom_point(aes(x = random_effect, y = "Random Effects Model"), color = "red") +
  geom_errorbarh(aes(xmin = random_effect_ci_lower, xmax = random_effect_ci_upper, y = "Random Effects Model"), height = 0.2, color = "red")

# Add annotations for heterogeneity statistics
p <- p + annotate("text", x = max(plot_data$ci_upper) * 0.5, y = -1, label = paste("I-squared:", round(I2, 2), "%; Q =", round(Q, 2), "p =", round(pval_Q, 3)), hjust = 0, vjust = 0, size = 3)

# Display the plot
print(p)



