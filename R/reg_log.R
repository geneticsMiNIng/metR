#' Logistic regression results for methylation data
#'
#' Get p.value and beta coefficient from grouping variable from logistic regression based on number of methylated and unmethylated citozines in two probes.
#' This function doesn't respect tiles or tiles.common column. This function is using in find.DMR and can be used sepparately.
#' @param data There are two options:  1. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @return vecotor with p.value and beta coef. from grouping variable from logistic regression or two-elemented vector of na values if something goes wrong
#' @export
#' @examples
#' data('sample.1')
#' data('sample.2')
#' data <- preprocessing(sample.1, sample.2)
#' data.tiles <- create.tiles.max.gap(data, gaps.lenth = 100)
#' data.test <- data.tiles %>% filter(tiles == 1)
#' reg.log(data.test)
#' # or by some self-defined regions:
#' data.test.2 <- data.tiles %>% filter(chr == 'chr2', start > 171573000, poz < 171574000)
#' reg.log(data.test.2)


reg.log <- function(data){
  tryCatch({
    mod.temp <- glm(cbind(meth, unmeth) ~ prob + as.factor(poz),
                    data = data, family = 'binomial')
    p.value <- summary(mod.temp)$coefficients[2,4]
    beta.coef <- summary(mod.temp)$coefficients[2,1]
    return(data.frame(p.value = p.value, beta.coef = beta.coef))
  }, error = function(cond){
    return(data.frame(p.value = NA, beta.coef = NA))})
}
