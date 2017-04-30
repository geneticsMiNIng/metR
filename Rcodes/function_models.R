

Wilcoxon_test <- function(data){
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  
  data %>% group_by(chr, tiles) %>%
    summarize(p_value = wilcox.test(x = data$met[1:n],y =  data$met[(n+1):(2*n)],
              alternative = 'two.sided', paired = T)$p.value) -> wilc
  
  wilc %>%  arrange(desc(p_value))
  }



T_test <- function(data){
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  
  data %>% group_by(chr, tiles) %>%
    summarize(p_value = t.test(data$met[1:n], y = data$met[(n+1):(2*n)],
         alternative ="two.sided",mu = 0, paired = TRUE, var.equal = FALSE)$p.value -> ttest
  
ttest$start <- 1000 *as.numeric(levels(ttest$tiles ))[ttest$tiles ]
ttest$end <- 1000 * as.numeric(levels(ttest$tiles ))[ttest$tiles ]+ 999
ttest %>% arrange(desc(p_value))       
}              
  



Log_reg <- function(data){
  
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  data$tiles <- as.factor(data$tiles)
  
  logic <- function(data){
    tryCatch({
      mod_temp <- glm(cbind(numCs, numTs) ~ group + as.factor(poz), 
                      data = data, family = 'binomial')
      logistic <- summary(mod_temp)$coefficients[2,4]
      beta_logistic <- summary(mod_temp)$coefficients[2,1]
   return(c(logistic, beta_logistic)) 
       }, error = function(cond){
         return(c(NA, NA))  }
  }
  
  system.time(data %>%
                group_by(chr, tiles) %>%
                do(mod = logic(data =.)) -> models
  )
  
  mod <- do.call('rbind', models$mod)
  models$beta <- mod[,2]
  models$p_value <- mod[,1]
  models$mod <- NULL
  
  models$start <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]
  models$end <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]+ 999
  models
}


Random_reg <- function(data){
  
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  data$tiles <- as.factor(data$tiles)
  
  random <- function(data){
    tryCatch({
      mod_temp <- glmer(cbind(numCs, numTs) ~ group + (1|poz), 
                        data = data, family = binomial)
      rand <- summary(mod_temp)$coefficients[2,4]
      beta_random <- summary(mod_temp)$coefficients[2,1]
    return(rand, beta_random)
      }, error = function(cond){
      return(NA, NA)})
  }
  
  system.time(data %>%
                group_by(chr, tiles) %>%
                random(mod = random(data =.)) -> models)
  
  mod <- do.call('rbind', models$mod)
  models$beta <- mod[,2]
  models$p_value <- mod[,1]
  models$mod <- NULL
  
  models$start <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]
  models$end <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]+ 999
  
  models
}



Mixed_reg <- function(data, acf){
  
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  data$tiles <- as.factor(data$tiles)
  
  mixed <- function(data, acf){
    tryCatch({
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
      return(summary(mod_temp)$tTable[2,])
    }, error = function(cond){
      return(NA)
    }, 
    warning = function(cond){
      return(NA) 
    })  
  }
  
  system.time(data %>%
                group_by(chr, tiles) %>%
                random(mod = mixed(data =.)) -> models)
  
  mod <- do.call('rbind', models$mod)
  models$beta <- mod[,2]
  models$p_value <- mod[,1]
  models$mod <- NULL
  
  models$start <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]
  models$end <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]+ 999
  
  models
}






Log_reg_lasso <- function(data){
  
  data$met <- data$numCs/(data$numCs + data$numTs)
  
  data$tiles = data$start %/% 1000
  data$tiles <- as.factor(data$tiles)
  
  lasso <- function(data){
    tryCatch({
      data_list <- list()
      
      for(i in 1:nrow(data)){
        data_list[[i]] <- data.frame(target = c(rep(1, data$numCs[i]), rep(0, data$numTs[i])),
                                     poz = c(rep(data$poz[i], data$numCs[i] + data$numTs[i])),
                                     group = c(rep(data$group[i], data$numCs[i] + data$numTs[i])))
        
      }
      data_new <- do.call('rbind', data_list)
      
      x1 <- as.factor(data_new[,2])
      x2 <- as.factor(data_new[,3])
      y <- as.factor(data_new[,1])
      
      x <- model.matrix(y ~ x1 + x2)[,-1]
      
      
      mod_temp <- cv.glmnet(x, y, alpha = 1 , family = 'binomial', nfold = 5)
      
      co<-coef(mod_temp,s = "lambda.min")
      beta_coef <- co[row.names(co) == 'x2EU']
      
      return(beta_coef)
    }, error = function(cond){
      return(NA)
    }, 
    warning = function(cond){
      return(NA) 
    })  
  }
  
  system.time(data %>%
                group_by(chr, tiles) %>%
                random(mod = lasso(data =.)) -> models)
  
  mod <- do.call('rbind', models$mod)
  models$beta <- mod[,1]
  models$mod <- NULL
  
  models$start <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]
  models$end <- 1000 * as.numeric(levels(models$tiles ))[models$tiles ]+ 999
  
  models
}






