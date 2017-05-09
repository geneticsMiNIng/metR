
setwd('/media/ola/EEAABE9FAABE642D/metylacja')
source('/media/ola/EEAABE9FAABE642D/metylacja/metR/Rcodes/function_preprocessing.R')
source('/media/ola/EEAABE9FAABE642D/metylacja/metR/Rcodes/function_visualization.R')
source('/media/ola/EEAABE9FAABE642D/metylacja/metR/Rcodes/libraries.R')


data <- readRDS('dane/to_analysis.rds') %>% filter(chr == 'chr7')
acf <- readRDS('wyniki/acfy_EK_min.rds') %>% filter(chr == 'mean')
acf <- acf$acf[-1]
n_probs <- 1

data$tiles = data$poz %/% 1000
data$tiles <- as.factor(data$tiles)
data <- data %>% filter(poz < 150000)

probs <- list()
for (i in 1:n_probs){
data %>% mutate(coverage = numCs + numTs,
                met_fixed = numCs/(numCs + numTs)) -> probs[[i]]

probs[[i]]$numCs <- mapply(function(a,b){rbinom(1,a, b)}, probs[[i]]$coverage, probs[[i]]$met_fixed)
    
probs[[i]] %>%  mutate(numTs = coverage - numCs,
         met = numCs/(numCs + numTs),
         no_prob = n_probs) -> probs[[i]]
}
                
probs <- do.call('rbind', probs)

all_models <- function(data, acf){
  
  data %>% arrange(group) -> data
  n <- nrow(data)/2
  
  ttest <- tryCatch({
  t.test(data$met[1:n], y = data$met[(n+1):(2*n)],
                    alternative ="two.sided",mu = 0, paired = TRUE, var.equal = FALSE)$p.value 
  }, error = function(cond){return(NA)})
  
  wilc <- tryCatch({
    wilcox.test(x = data$met[1:n],y =  data$met[(n+1):(2*n)],
                         alternative = 'two.sided', paired = T)$p.value
  }, error = function(cond){return(NA)})
  
  logistic <- tryCatch({
    mod_temp <- glm(cbind(numCs, numTs) ~ group + as.factor(poz), 
                    data = data, family = 'binomial')
    c(summary(mod_temp)$coefficients[2,4], summary(mod_temp)$coefficients[2,1])
  }, error = function(cond){
    return(c(NA,NA))})
  
  mixed_model <- tryCatch({
  mod_temp <- glmer(cbind(numCs, numTs) ~ group + (1|poz), 
                      data = data, family = binomial)
  c(summary(mod_temp)$coefficients[2,4], summary(mod_temp)$coefficients[2,1])
    }, error = function(cond){
    return(c(NA, NA))})
  
  mixed_corr <- tryCatch({
    n <- nrow(data)/2
    poz <- data$poz[1:n]
    
    m1 <- matrix(poz, nrow = length(poz), ncol = length(poz))
    m2 <- t(m1)
    m3 <- m1 - m2
    m3 <- m3[m3>0]
    cor <- acf[m3]
    
    data$poz2 <- c(1:n, 1:n)
    X <- diag(1, nrow = n) 
    X[lower.tri(X)] <- cor 
    X[upper.tri(X)] <- cor 
    mat_corr <- nearPD(X, corr= TRUE)$mat
    cs <- Initialize(corSymm(value =  mat_corr[lower.tri(mat_corr)], form= ~ poz2, fixed = TRUE), data=data[1:n,]) 
    
    mod_temp <- glmmPQL(cbind(numCs, numTs) ~ group , random = ~1|factor(poz), 
                        data = data, family = binomial, correlation = cs, verbose = FALSE)
    c(summary(mod_temp)$tTable[2,5], summary(mod_temp)$tTable[2,1])
  }, error = function(cond){
    return(c(NA,NA))
  })
  return(c(ttest, wilc, logistic, mixed_model, mixed_corr))
}

probs %>% group_by(tiles, no_prob) %>%
  do(models = all_models(data = ., acf = acf)) -> result

models <- data.frame(do.call("rbind", result$models))
models$tiles <- result$tiles
models$no_prob <- result$no_prob
colnames(models)[1:8] <- c("ttest", "wilc", "pValue_log", "beta_log", "pValue_mixed", "beta_mixed",
                           "pValue_corr", "beta_corr")

top30 <- list()

models %>% arrange(ttest) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
              "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[1]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                    "min_met", "max_met", "range", "st_met", "mean_cov")) 

models %>% arrange(wilc) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                          "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[2]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                           "min_met", "max_met", "range", "st_met", "mean_cov")) 

models %>% arrange(pValue_log) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                                                                   "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[3]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                           "min_met", "max_met", "range", "st_met", "mean_cov")) 

models %>% filter(pValue_log < 0.01) %>% arrange(desc(beta_log)) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
         "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[4]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                              "min_met", "max_met", "range", "st_met", "mean_cov")) 

models %>% arrange(pValue_mixed) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                                                                         "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[5]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                              "min_met", "max_met", "range", "st_met", "mean_cov")) 


models %>% filter(pValue_mixed < 0.01) %>% arrange(desc(beta_mixed)) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                                                                                                           "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[6]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                              "min_met", "max_met", "range", "st_met", "mean_cov")) 


models %>% arrange(pValue_corr) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                                                                           "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[7]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                              "min_met", "max_met", "range", "st_met", "mean_cov")) 


models %>% filter(pValue_corr < 0.01) %>% arrange(desc(beta_corr)) %>% top_n(30) %>% left_join(probs, on = c("tiles" = "tiles",
                                                                                                               "no_prob" = "no_prob")) %>% group_by(tiles, no_prob, group) %>% 
  summarise(nObs = n(), mean_met = mean(met), min_met = min(met), max_met = max(met), 
            range = max(met) - min(met), st_met = sd(met), mean_cov = mean(coverage)) -> temp

top30[[8]] <- dcast(setDT(temp), tiles ~ group, value.var = c("nObs", "mean_met", 
                                                              "min_met", "max_met", "range", "st_met", "mean_cov")) 

