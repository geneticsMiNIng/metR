% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stats.R
\name{get_stats}
\alias{get_stats}
\title{Summarize regions}
\usage{
get_stats(data)
}
\arguments{
\item{data}{dataframe with specific columns: chr, poz, prob, no, meth, unmeth, meth.rate, tiles and possible tiles.common columns. This dataframe is result of function create_tiles_min_gap or
create_tiles_fixed_length}
}
\value{
data.frame which is summing-up regions specifed by tiles and tiles.common columns in data
}
\description{
Summarize regions by minimum, maximum, mean, standard deviation of methylation rate in two probes and methylation diff rate withib two probes and estimated quantile basen od methylation diff rate
}
\examples{
data('schizophrenia')
control <- schizophrenia \%>\% filter(category == 'control') \%>\%
dplyr::select(-category)

disease <- schizophrenia \%>\% filter(category == 'disease') \%>\%
 dplyr::select(-category)

data <- preprocessing(control, disease)
data.tiles <- create_tiles_max_gap(data, gaps.length = 100)
head(get_stats(data.tiles))
}
