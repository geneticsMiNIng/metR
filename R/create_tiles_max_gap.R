#' Create regions for testing methylation difference
#'
#' The same regions are observations that maximum difference position is gaps.length argument
#' @param data dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate. This dataframe is result of function preprocessing.
#' @param gaps.length integer number that specifies maximum difference position between the same methylation regions
#' @return data.frame from parameter data with extra column tiles that is region id number within chromosomes
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
#' head(create_tiles_max_gap(data, gaps.length = 100))

create_tiles_max_gap <- function(data, gaps.length){

  check_data_without_tiles(data)

  check_args_create_tiles_max_gap(gaps.length)

  data <- split(data, f = data$chr)
  data <- lapply(data, create_tiles_on_chr, gaps.length)
  do.call(rbind, data)
}



