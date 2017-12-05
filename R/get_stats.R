#' Summarize regions
#'
#' Summarize regions by minimum, maximum, mean, standard deviation of methylation rate in two probes and methylation diff rate withib two probes and estimated quantile basen od methylation diff rate
#' @param data dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create_tiles_min_gap or
#' create_tiles_fixed_length
#' @return data.frame which is summing-up regions specifed by tiles and tiles.common columns in data
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
#' head(get_stats(data.tiles))
#' @export

get_stats <- function(data){

  check_data_without_tiles(data[,1:7])
  check_tiles_in_data(data)
  data <- group_data(data, prob = T)
  main_get_stats(data, map)

  }


