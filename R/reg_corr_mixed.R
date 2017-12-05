#' Logistic regression with random effects and given correlation matrix results for methylation data
#'
#' Get p.value and beta coefficient from grouping variable from logistic regression with random effects and given correlation matrix based on number of methylated and unmethylated citozines in two probes.
#' This function doesn't respect tiles or tiles.common column. This function is using in find.DMR and can be used sepparately.
#' @param data There are two options:  1. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @return vecotor with p.value and beta coef. from grouping variable from logistic regression with random effects and given correlation matrix or two-elemented vector of na values if something goes wrong
#' @export
#' @examples
#' data('schizophrenia')
#' control <- schizophrenia %>% filter(category == 'control') %>%
#' dplyr::select(-category)
#'
#' disease <- schizophrenia %>% filter(category == 'disease') %>%
#'  dplyr::select(-category)
#'
#' data <- preprocessing(control, disease)
#' data.tiles <- create_tiles_max_gap(data, gaps.length = 100)
#'
#' # we must also read acf vector which was previously estimated
#' data('mean.acf.chr')
#' acf <- mean.acf.chr[-1]
#' data.test <- data.tiles %>% filter(tiles == 1)
#' reg_corr_mixed(data.test, acf)
#' # or by some self-defined regions:
#' data.test.2 <- data.tiles %>% filter(chr == 'chr1', poz > 80000, poz < 82000)
#' reg_corr_mixed(data.test.2, acf)


reg_corr_mixed <- function(data, acf){

  data %<>% arrange(prob, poz)

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

    mod.temp <- glmmPQL(cbind(meth, unmeth) ~ prob , random = ~1|factor(poz),
                        data = data, family = binomial, correlation = cs, verbose = FALSE)
    p.value <- summary(mod.temp)$tTable[2,5]
    beta.coef <- summary(mod.temp)$tTable[2,1]
    return(data.frame(p.value = p.value, beta.coef = beta.coef))
  }, error = function(cond){
    return(data.frame(p.value = NA, beta.coef = NA))})
}
