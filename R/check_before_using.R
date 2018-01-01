#' Check datasets and functions in metR
#'
#' PBI: this function should be converted as a test with test_that
#' PBI: read https://pbiecek.gitbooks.io/przewodnik/content/Programowanie/pakiety/nowe_testy.html for further instructions
#'
#' Check if all necessary datasets and functions are in metR
#' @return If all datasets and function are in metR then functions returns message:
#' 'OK! All required functions and datasets are available in the metR package'. Otherwise function is stopped with message why.
#' @export
#' @examples
#' check_before_using()
#'
check_before_using <- function(){
metR_functions <- c(
  'check_if_sample_is_correct',
  'check_data_without_tiles',
  'check_args_create_tiles_fixed_length',
  'check_args_create_tiles_max_gap',
  'check_tiles_in_data',
  'check_args_draw_metylation',
  'create_tiles_on_chr',
  'create_tiles_fixed_length',
  'create_tiles_max_gap',
  'draw_methylation',
  'find_DMR',
  'find_DMR_Wilcoxon',
  'find_DMR_Ttest',
  'find_DMR_KS',
  'find_DMR_Reg_Log',
  'find_DMR_Reg_Mixed',
  'find_DMR_Reg_Corr_Mixed',
  'find_DMR_given_methods',
  'group_data',
  'group_tiles',
  'get_quantile',
  'get_stats',
  'main_preprocessing',
  'main_create_tiles_fixed_length',
  'main_get_stats',
  'main_prep_data_draw_metylation',
  'main_draw_metylation',
  'preprocessing',
  'reg_corr_mixed',
  'reg_log',
  'reg_mixed',
  'test_ks',
  'test_t',
  'test_wilcoxon'
)

metR_datasets <- c(
  'schizophrenia',
  'mean.acf.chr',
  'quantile.function',
  'find.DMR.results'
)

if(!all(sapply(metR_functions, exists, mode='function')) == TRUE)
  stop("Error: Some functions are lacking in metR package!")

if(!all(sapply(metR_datasets, exists, mode='any')) == TRUE)
  stop("Error: Some datasets are lacking in metR package!")

return('OK! All required functions and datasets are available in the metR package')
}

