#' Preprocessing data
#'
#' Preprocessing data for methylation analyses
#' @param sample.1 a data frame with methylation data
#' @param sample.2 a data frame with methylation data
#' @return data.frame with methylation values on common positions and chromosomes from sample.1 and sample.2
#' @export
#' @examples
#' # data from package
#' data('sample.1')
#' data('sample.2')
#' preprocessing(sample.1, sample.2)


preprocessing <- function(sample.1, sample.2){

  if(!is.data.frame(sample.1))
     stop("Error: Argument sample.1 or is incorrect. This must be a data.frame")

  if(!is.data.frame(sample.2))
    stop("Error: Argument sample.2 or is incorrect. This must be a data.frame")

  sample.colnames <- c("chr", "poz", "no", "meth","unmeth" ,"meth.rate")

  if(!all.equal(colnames(sample.1), sample.colnames))
    stop("Error: Incorrect colnames in sample.1")

  if(!all.equal(colnames(sample.2), sample.colnames))
    stop("Error: Incorrect colnames in sample.2")

  sample.types <- c("character","integer","integer","integer","integer", "double")
  names(sample.types) <-sample.colnames

  if(!all.equal(sapply(sample.1, typeof), sample.types))
    stop("Error: Incorrect datatypes in sample.1")

  if(!all.equal(sapply(sample.2, typeof), sample.types))
    stop("Error: Incorrect datatypes in sample.2")

  if (!all(sample.1$poz > 0) & all(sample.1$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) &
      all(sample.1$meth + sample.1$unmeth == sample.1$no) &
      all(round(sample.1$meth.rate,4) == round(sample.1$meth/sample.1$no,4)))
      stop("Error: Incorrect data in sample.1")

  if (!all(sample.2$poz > 0) & all(sample.2$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) &
      all(sample.2$meth + sample.2$unmeth == sample.2$no) &
      all(round(sample.2$meth.rate,4) == round(sample.2$meth/sample.2$no,4)))
      stop("Error: Incorrect data in sample.2")


    sample.1 %>% inner_join(sample.2, by = c("chr"="chr", "poz"="poz")) %>%
      gather(key, value, -chr, -poz) %>%
      tidyr::extract(key, c('type', "prob"), "(.*)\\.(.)") %>%
      spread(type, value) %>% dplyr::select(chr, poz, prob, no, meth, unmeth, meth.rate) %>%
      mutate(no = as.integer(no), meth = as.integer(meth), unmeth = as.integer(unmeth)) -> data

  rm(sample.1, sample.2, sample.types, sample.colnames)
  data
  }


