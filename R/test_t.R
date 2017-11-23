#' t-Stundent test for methylation data
#'
#' Get p.value from t-Student with unequal variances based on methylation rate in two probes.
#' This function doesn't respect tiles or tiles.common column. This function is using in find.DMR and can be used sepparately.
#' @param data There are two options:  1. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @return p.value from t-Student test or na values if something goes wrong
#' @export
#' @examples
#' data('sample.1')
#' data('sample.2')
#' data <- preprocessing(sample.1, sample.2)
#' data.tiles <- create.tiles.max.gap(data, gaps.lenth = 100)
#' data.test <- data.tiles %>% filter(tiles == 1)
#' test.t(data.test)
#' # or by some self-defined regions:
#' data.test.2 <- data.tiles %>% filter(chr == 'chr2', start > 171573000, poz < 171574000)
#' test.t(data.test.2)

test.t <- function(data){
  data %<>% arrange(prob, poz)
  n = nrow(data)/2
  tryCatch({p.value = t.test(x = data$meth.rate[1:n], y = data$meth.rate[(n+1):(2*n)],
                             alternative ="two.sided",mu = 0, paired = TRUE, var.equal = FALSE)$p.value
  return(data.frame(p.value = p.value))}
  ,error=function(cond) {return(data.frame(p.value = NA))})
}
