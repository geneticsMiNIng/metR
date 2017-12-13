#' Preprocessing data
#'
#' Preprocessing data for methylation analyses
#' @param sample.1 a data frame with methylation data
#' @param sample.2 a data frame with methylation data
#' @return data.frame with methylation values on common positions and chromosomes from sample.1 and sample.2
#' @export
#' @examples
#' # data from package
#' data('schizophrenia')
#' control <- schizophrenia %>% filter(category == 'control') %>%
#' dplyr::select(-category)
#'
#' disease <- schizophrenia %>% filter(category == 'disease') %>%
#'  dplyr::select(-category)
#'
#' data <- preprocessing(control, disease)
#' head(data)


preprocessing <- function(sample.1, sample.2){
  check_if_sample_is_correct(sample.1, 'sample.1')
  check_if_sample_is_correct(sample.2, 'sample.2')
  main_preprocessing(sample.1, sample.2)
  }


