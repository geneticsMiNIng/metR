#' Plot DMR region
#'
#' Visualizing DMR region by plotting methylation rate within two probes
#' @param data There are two options:  1. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate.
#' This dataframe is result of function preprocessing.
#' 2. dataframe with specyfic columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create.tiles.min.gap or
#' create.tiles.fixed.length.
#' @param chr chromosom name of region that are being plotted
#' @param start minimum position of region that are being plotted
#' @param end maximum position of region that are being plotted
#' @param bind.probes if TRUE methylation rates on the same position will be binded by vertical lines
#' @param smooth.methylation if TRUE methylation rates in two probes will be smoothed
#' @return ggplot object with visualization of regions
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
#' draw_methylation(data, chr = 'chr1', start = 80000, end = 100000)
#'
#' #without binding lines and smoothing
#' draw_methylation(data, chr = 'chr1', start = 80000, end = 100000,
#' bind.probes = F, smooth.methylation = F)
#'
#' # with changing some ggplot2 arguments:
#' draw_methylation(data, chr = 'chr1', start = 80000, end = 100000, legend.title = 20,
#' legend.position = 'bottom', plot.title = 28, size.x.dot = 10, size.y.dot = 4)


draw_methylation <- function(data, chromosom, start, end, bind.probes = T,
                             smooth.methylation = T,
                             size.x.dot = 15, size.y.dot = 9,
                             plot.title = 26, axis.title.x  = 23,
                             axis.title.y = 23, legend.position = 'right', axis.text.x = 20, axis.text.y = 20,
                             legend.text = 16, legend.title = 18){

  check_data_without_tiles(data[,1:7])
  check_args_draw_metylation(start, end, chromosom, bind.probes,
                              smooth.methylation)

  DT <- main_prep_data_draw_metylation(data, chromosom, start, end)
  main_draw_metylation(DT, chromosom, start, end, bind.probes,
                                   smooth.methylation,
                                   size.x.dot, size.y.dot,
                                   plot.title, axis.title.x,
                                   axis.title.y, legend.position, axis.text.x, axis.text.y,
                                   legend.text, legend.title)

}


