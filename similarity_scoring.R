setwd("~/Desktop/penguin_sampling/")
source("sampling_code.R")

samples$X
site_list <- xlsx::read.xlsx("SiteLocations.xlsx", 1) %>% 
  filter(ccamlr_id == 48.1)


site_list
samples_48.1 <- samples %>% filter(X %in% site_list$site_id)

# Finds the global trend
total_sum <- colMeans(samples_48.1[-1], na.rm = T)
df <- data.frame(counts = total_sum[], year = 1970:2020)
mod <- lm(counts ~ year, df)
total_trend <- mod$coefficients[2]

trend_coeff = numeric(dim(samples_48.1)[1])
ts_cor <- numeric(dim(samples_48.1)[1])
for (i in 1:dim(samples_48.1)[1]){
  model <- lm(as.numeric(samples_48.1[i, -1]) ~ seq(1970, 2020, by = 1))
  trend_coeff[i] <- model$coefficients[2]
  
  ts_cor[i] <- cor(as.numeric(samples_48.1[i, -1]), df[,1], use = 'complete.obs')
}

samples_48.1$trend <- abs(trend_coeff - total_trend)
samples_48.1$ts_cor <- ts_cor
dif_global_trend <- samples_48.1[order(samples_48.1$trend),]
dif_global_trend$X
global_ts_cor <- samples_48.1[order(samples_48.1$ts_cor, decreasing = T), ]
global_ts_cor$X

rank_orders <- data.frame(trend_order = dif_global_trend$X, ts_order = global_ts_cor$X)
write_csv(rank_orders, "rank_orders.csv")

site_list %>% filter((site_id %in% dif_global_trend$X[1:10]) &
                       (site_id %in% global_ts_cor$X[1:10]))
