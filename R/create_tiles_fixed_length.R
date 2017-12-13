#' Create regions for testing methylation difference
#'
#' The same regions are observations where maximum difference between minimum and maximum position is tiles.length argument.
#' @param data dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate. This dataframe is result of function preprocessing.
#' @param tiles.length integer number that specifies maximum difference between minimum and maximum position in the same methylation regions.
#' k-region in chromosome are observations for which position is between [k * tiles.length;(k + 1)* tiles.length -1]
#' @param common logi value. If TRUE this function creates second regions group that k-region in chromosome are observations for which position is between [ k * tiles.length + tiles.length/2 ; (k + 1)* tiles.length -1 + tiles.length/2]
#' @return data.frame from parameter data with extra column tiles that is region id number within chromosomes and extra column
#' tiles.common if argument tiles.common is not null
#' @export
#'
#' @examples
#' data('schizophrenia')
#' control <- schizophrenia %>% filter(category == 'control') %>%
#' dplyr::select(-category)
#'
#' disease <- schizophrenia %>% filter(category == 'disease') %>%
#'  dplyr::select(-category)
#'
#' data <- preprocessing(control, disease)
#' head(create_tiles_fixed_length(data, tiles.length = 1000, common = FALSE))
#' head(create_tiles_fixed_length(data, tiles.length = 1000, common = TRUE))


create_tiles_fixed_length <- function(data, tiles.length, common = FALSE){

  check_data_without_tiles(data)

  check_args_create_tiles_fixed_length(tiles.length, common)

  main_create_tiles_fixed_length(data, tiles.length, common)

}
