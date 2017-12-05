check_if_sample_is_correct <- function(sample, sample.name){

if(!is.data.frame(sample))
  stop(paste0("Error: Argument ", sample.name," is incorrect. This must be a data.frame "))

sample.colnames <- c("chr", "poz", "no", "meth","unmeth" ,"meth.rate")

if(!all.equal(colnames(sample), sample.colnames))
  stop(paste0("Error: Incorrect colnames in ", sample.name))

sample.types <- c("character","integer","integer","integer","integer", "double")
names(sample.types) <-sample.colnames

if(!all.equal(sapply(sample, typeof), sample.types))
  stop(paste0("Error: Incorrect datatypes in ", sample.name))

if (!all(sample$poz > 0) & all(sample$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) &
    all(sample$meth + sample$unmeth == sample$no) &
    all(round(sample$meth.rate,4) == round(sample$meth/sample$no,4)))
  stop(paste0("Error: Incorrect data in ", sample.name))
}



check_data_without_tiles <- function(data){
  data.colnames <- c('chr', 'poz', 'prob', 'no', 'meth', 'unmeth', 'meth.rate')

  if(!all.equal(colnames(data), data.colnames))
    stop("Error: Incorrect colnames in data")

  data.types <- c("character","integer","character","integer","integer", "integer", 'double')
  names(data.types) <- data.colnames

  if(!all.equal(sapply(data, typeof), data.types))
    stop("Error: Incorrect datatypes in data")

  if (!all(data$poz > 0) | !all(data$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) |
      !all(data$meth + data$unmeth == data$no) | !all(round(data$meth /data$no,3) == round(data$meth.rate,3)))
    stop("Error: Incorrect values in data")
}


check_args_create_tiles_fixed_length <- function(tiles.length, common){

  if(!zapsmall(tiles.length, 15) == round(tiles.length) | tiles.length <= 0)
  stop("Error: Incorrect tiles.length argument")

    if(!is.logical(common))
      stop("Error: Incorrect common argument")

}



check_args_create_tiles_max_gap <- function(gaps.length){

  if(!zapsmall(gaps.length, 15) == round(gaps.length) | gaps.length <= 0)
    stop("Error: Incorrect gaps.length argument")
}


check_tiles_in_data <- function(data){

  if(!'tiles' %in% colnames(data))
    stop("Error: Incorrect colnames in data")

  if (!all(data$tiles >= 0) | !all(zapsmall(data$tiles, 15) == round(data$tiles)))
    stop("Error: Incorrect values in data")

  if ('tiles.common' %in% colnames(data)){
    if (!all(data$tiles.common >= 0) | !all(zapsmall(data$tiles.common, 15) == round(data$tiles.common)))
      stop("Error: Incorrect values in data")
  }
}


check_args_draw_metylation <- function(start, end, chromosom, bind.probes,
                                       smooth.methylation){

  if(!zapsmall(start, 15) == round(start) | start <= 0)
    stop("Error: Incorrect start argument")

  if(!zapsmall(end, 15) == round(end) | end <= 0)
    stop("Error: Incorrect end argument")

  if(!chromosom %in% paste0('chr', c(1:22, "X", "Y", "M")))
    stop("Error: Incorrect chr argument")

  if(!is.logical(bind.probes))
    stop("Error: Incorrect bind.probes argument")

  if(!is.logical(smooth.methylation))
    stop("Error: Incorrect smooth.methylation argument")

}
