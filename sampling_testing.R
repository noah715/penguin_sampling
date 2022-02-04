setwd("~/Desktop/penguin_sampling/")
source("sampling_code.R")

# Finds the global trend
total_sum <- colSums(samples[-1], na.rm = T)/nrow(samples)
df <- data.frame(counts = total_sum[], year = 1970:2020)
mod <- lm(counts ~ year, df)
total_trend <- mod$coefficients[2]

n <- 1
iter <- 500
options_coeff <- matrix(nrow = 6, ncol = iter)

# Samples the trend coefficients for each sampling option
for (i in 1:iter){
  new_samples <- option_1(n, samples)
  sample_sum <- colSums(new_samples[-1], na.rm = T)/(n)
  df <- data.frame(counts = sample_sum[], year = 1970:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[1,i] <- mod$coefficients[2]
  
  
  
  new_samples <- option_2(n, samples)
  sample_sum <- colSums(new_samples[-1], na.rm = T)/(n)
  df <- data.frame(counts = sample_sum[], year = 1970:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[2,i] <- mod$coefficients[2]
  
  
  
  new_samples <- option_3(n, samples, 5)
  sample_sum <- colSums(new_samples[-1], na.rm = T)/(n)
  df <- data.frame(counts = sample_sum[], year = 1970:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[3,i] <- mod$coefficients[2]
  
  
  
  new_samples <- option_4(n, samples, 5, 1)
  sample_sum <- colSums(new_samples[-1], na.rm = T)/(n + 1)
  df <- data.frame(counts = sample_sum[], year = 1970:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[4,i] <- mod$coefficients[2]
  
  
  new_samples <- option_5(n, samples, 5, 1)
  sample_sum <- colSums(new_samples[-c(1,2)], na.rm = T)/(n + 1)
  df <- data.frame(counts = sample_sum[], year = 1971:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[5,i] <- mod$coefficients[2]
  
  
  new_samples <- option_6(n, samples)
  sample_sum <- colSums(new_samples[-1], na.rm = T)/(n)
  df <- data.frame(counts = sample_sum[], year = 1970:2020)
  mod <- lm(counts ~ year, df)
  options_coeff[6,i] <- mod$coefficients[2]
}

write.csv(as.data.frame(options_coeff), "options_coeff.csv")

# Plots histograms of the trend coefficients
for (i in 1:6){
  hist(options_coeff[i,], main = paste("Option", i))
  abline(v = total_trend, col = 'red', lwd = 2)
  dev.copy(png, paste("option", i, ".png", sep = ""))
  dev.off()
}

