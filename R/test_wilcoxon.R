#' Wilcoxon test for methylation data
#'
#' Get p.value from Wilcoxon test based on methylation rate in two probes.
#' This function doesn't respect tiles or tiles.common column. This function is using in find_DMR and can be used sepparately.
#' @param data There are two options:  1. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create_tiles_min_gap or
#' create_tiles_fixed_length.
#' @return p.value from Wilcoxon test or na values if something goes wrong
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
#' test_wilcoxon(data.test)
#' # or by some self-defined regions:
#' data.test.2 <- data.tiles %>% filter(chr == 'chr1', poz > 80000, poz < 100000)
#' test_wilcoxon(data.test.2)


test_wilcoxon <- function(data){
  data %<>% arrange(prob, poz)
  n = nrow(data)/2
  tryCatch({p.value = wilcox.test(x = data$meth.rate[1:n],y =  data$meth.rate[(n+1):(2*n)],
                                  alternative = 'two.sided', paired = T)$p.value
  return(data.frame(p.value = p.value))}
  ,error=function(cond){return(data.frame(p.value = NA))})
}
