setwd('/media/ola/EEAABE9FAABE642D/metylacja')
source('/media/ola/EEAABE9FAABE642D/metylacja/metR/Rcodes/function_preprocessing.R')
source('/media/ola/EEAABE9FAABE642D/metylacja/metR/Rcodes/libraries.R')


data <- readRDS('dane/to_analysis.rds')
data %>% mutate(met = numCs/(numTs + numCs)) -> data


options(scipen=10000)

data <- create_tiles(data, 100) 
data %>% group_by(tiles, group) %>% summarise(mean_met = mean(met), n_sqrt3 = n()^(-1/3), n = n()) %>%
  filter(n <= 30) %>% spread(group, mean_met) %>% mutate(diff_met = abs(EK - EU)) -> data_mets

q <- seq(0.1,0.9, by = 0.1)
for(i in q){
  fit1s <- rq(diff_met~n_sqrt3,tau=i, data = data_mets)
  summ1s <- summary(fit1s)
  saveRDS(summ1s, paste0('p1_', i, '.rds'))
}


data$tiles = data$poz %/% 1000

data %>% group_by(tiles, group, chr) %>% summarise(mean_met = mean(met), n_sqrt3 = n()^(-1/3), n = n()) %>% 
  filter(n <= 30) %>% spread(group, mean_met) %>% mutate(diff_met = abs(EK - EU)) -> data_met2

for(i in q){
  fit1s <- rq(diff_met~n_sqrt3,tau=i, data = data_met2)
  summ1s <- summary(fit1s)
  saveRDS(summ1s, paste0('t1_', i, '.rds'))
}


