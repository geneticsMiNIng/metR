#' Get n top region
#'
#' Get n top region from result of find_DMR function.
#' @param data dataframe from output of find_DMR function with given method
#' @param n number of regions to return
#' @param stats data.frame with basic statisics about region. This is result of running get_stats function
#' @return data.frame with n top region with their basic statistics
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
#' data.tiles.small <- data.tiles %>% filter(tiles < 30)
#' stats <- get_stats(data.tiles.small)
#' dmr <- find_DMR(data.tiles.small, c('Wilcoxon', 'Ttest', 'KS', 'Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'))
#' get_top(find_DMR$Wilcoxon, 3, stats)
#' get_top(find_DMR$Reg.Log, 3, stats)
#'
#' @export

  get_top <- function(data, n, stats){

    check_args_get_top(n)
    check_result_data(data)
    check_stats(stats)

    data %>% slice(1:n) %>%
      left_join(stats, by = c('chr', 'start', 'end')) %>%
      dplyr::select(chr, start, end, meth.cov, meth.max_x, meth.max_y
                    ,meth.mean_x, meth.mean_y, meth.min_x,
                    meth.min_y, meth.sd_x, meth.sd_y, meth.diff,quantile)
  }



