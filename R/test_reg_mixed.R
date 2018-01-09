#' Logistic regression with random effects results for methylation data
#'
#' Get p.value and beta coefficient from grouping variable from logistic regression with random effects based on number of methylated and unmethylated citozines in two probes.
#' This function doesn't respect tiles or tiles.common column. This function is using in find.DMR and can be used sepparately.
#' @param data There are two options:  1. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @return vector with p.value and beta coef. from grouping variable from logistic regression with random effects or two-elemented vector of na values if something goes wrong
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
#' data.test <- data.tiles %>% filter(tiles == 10)
#' test_reg_mixed(data.test)
#' # or by some self-defined regions:
#' data.test.2 <- data.tiles %>% filter(chr == 'chr1', poz > 80000, poz < 100000)
#' test_reg_mixed(data.test.2)


test_reg_mixed <- function(data){
  tryCatch({
    mod.temp <- glmer(cbind(meth, unmeth) ~ prob + (1|poz),
                      data = data, family = binomial)
    p.value <- summary(mod.temp)$coefficients[2,4]
    beta.coef<- summary(mod.temp)$coefficients[2,1]
    return(data.frame(p.value = p.value, beta.coef = beta.coef))
  }, error = function(cond){
    return(data.frame(p.value = NA, beta.coef = NA))})
}
