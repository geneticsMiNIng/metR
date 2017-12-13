#' Finding DMR
#'
#' Finding DMR by Wilcoxon, t-Student, Kolmogorov-Smirnow tests or logistic regression, logistic regression with mixed models,
#' logistic regression with mixed models with correlation matrix
#' @param data There are two options:  1. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @param methods vectors with given methods. Possible values are: 'Wilcoxon', 'Ttest', 'KS', 'Reg.Log', 'Reg.Mixed',
#' 'Reg.Corr.Mixed'.
#' 'Wilcoxon' - Wilcoxon signed test;
#' 'Ttest' - t-Student test with unequal variance;
#' 'KS' - Kolmogorov-Smirnov test;
#' 'Reg.Log' - Wald test of grouping variable from logistic regression;
#' 'Reg.Mixed' - Wald test of grouping variable from logistic regression with mixed effects;
#' 'Reg.Corr.Mixed' - Wald test of grouping variable from logistic regression with mixed effect and estimated previous correlation matrix
#' @param p.value.log.reg if not NULL regions with p.value of prob variable smaller than p.value.log.reg are returned and  decreasingly ordered by absolute value of beta coefficient
#' of prob variable otherwise regions ale increasingly ordered by p.value
#' @param p.value.reg.mixed if not NULL regions with p.value of prob variable smaller than p.value.log.reg are returned and  decreasingly ordered by absolute value of beta coefficient
#' of prob variable otherwise regions ale increasingly ordered by p.value
#' @param p.value.reg.corr.mixed if not NULL regions with p.value of prob variable smaller than p.value.log.reg are returned and  decreasingly ordered by absolute value of beta coefficient
#' of prob variable otherwise regions ale increasingly ordered by p.value
#' @param beta.coef.max only results which have absolute value of beta.coef less than this parameter are returned from Log.Reg, Reg.Mixed, Reg.Corr.Mixed. This prevent cases when algorithm
#' did not convergence well
#' @return list object. Elements of list are results of given methods. The most interesting regions are on the top
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
#' data.tiles.small <- data.tiles %>% filter(tiles < 30)
#'
#'  #finding DMR by all methods with sorting on p.values
#' find_DMR(data.tiles.small, c('Wilcoxon', 'Ttest', 'KS', 'Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'))
#'
#' #finding DMR by 'Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'  with sorting on beta values
#' find_DMR(data.tiles.small, c('Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'), p.value.log.reg = 0.01, p.value.reg.mixed = 0.02, p.value.reg.corr.mixed=0.03)
#'
#' #finding DMR only by 'Reg.Log' with sorting on beta values and 'Wilcoxon' with sorting on p.values
#' find_DMR(data.tiles.small, c('Wilcoxon', 'Reg.Log'), p.value.log.reg = 0.001)



find_DMR <- function(data, methods, p.value.log.reg = NULL,
                     p.value.reg.mixed= NULL, p.value.reg.corr.mixed= NULL,
                     beta.coef.max = 30){


  check_data_without_tiles(data[,1:7])
  check_tiles_in_data(data)
  data <- group_data(data, prob = F)

find_DMR_given_methods(data, methods, p.value.log.reg,p.value.reg.mixed, p.value.reg.corr.mixed,
                       beta.coef.max)
}

