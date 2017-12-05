#' Create regions for testing methylation difference
#'
#' The same regions are observations that maximum difference position is gaps.length argument
#' @param data dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate. This dataframe is result of function preprocessing.
#' @param tiles.length integer number that specifes maximum difference between minimum and maximum position in the same methylation regions
#' @param common logi value. If TRUE this function creates second regions group that are min postion is (min postion + max position)/2 of k-region and
#' max position is (min position + max position) of k+1 region.
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
