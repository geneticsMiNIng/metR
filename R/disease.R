#' Methylation data in chromosome 1.
#'
#' A dataset containing the methylation data in chromosome 1.
#' Data for this probe are downloaded from site: http://www.neuroepigenomics.org/methylomedb/download.html. We used 4 disease samples:
#' SCZ1 AC, SCZ2 AC, SCZ3 AC, SCZ4 AC. After downloading we summed all results on the same position and chromosome separately
#' from control and disease samples. The variables are as follows:
#'
#' \itemize{
#'   \item chr. name of chromosome where methylation data was collected (only chromosome 1.)
#'   \item poz. position of chromosome where methylation data was collected
#'   \item no. number of sequences
#'   \item meth. number of sequences where cytosine was methylated
#'   \item unmeth. number of sequences where cytosine was unmethylated
#'   \item meth.rate. methylation.rate that is ratio of meth and no variables
#' }
#'
#' @docType data
#' @keywords datasets
#' @name disease
#' @usage data(disease)
#' @format A data frame with 2 109 751 rows and 6 variables
NULL
