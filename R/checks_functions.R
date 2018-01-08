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

if (!(all(sample$poz > 0) & all(sample$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) &
    all(sample$meth + sample$unmeth == sample$no) &
    all(round(sample$meth.rate,4) == round(sample$meth/sample$no,4))))
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

check_args_find_DMR <- function(methods, p.value.log.reg,
                                p.value.reg.mixed, p.value.reg.corr.mixed,
                                beta.coef.max){

  all.methods <- c("Wilcoxon" ,"Ttest" ,"KS" ,"Reg.Log" ,"Reg.Mixed" ,"Reg.Corr.Mixed")

  if(!is.character(methods) |
     !(is.numeric(p.value.log.reg) | is.null(p.value.log.reg)) |
     !(is.numeric(p.value.reg.mixed) | is.null(p.value.reg.mixed)) |
     !(is.numeric(p.value.reg.corr.mixed) | is.null(p.value.reg.corr.mixed)) |
     !is.numeric(beta.coef.max))
    stop("Error: Incorrect arguments in find_DMR")


  if(!all(methods %in% all.methods))
    stop("Error: Incorrect arguments in find_DMR")

  if(!is.null(p.value.log.reg)){
    if(p.value.log.reg < 0 | p.value.log.reg > 1)
      stop("Error: Incorrect arguments in find_DMR")
  }

  if(!is.null(p.value.reg.mixed)){
    if(p.value.reg.mixed< 0 | p.value.reg.mixed > 1)
      stop("Error: Incorrect arguments in find_DMR")
  }

  if(!is.null(p.value.reg.corr.mixed)){
    if(p.value.reg.corr.mixed < 0 | p.value.reg.corr.mixed > 1)
      stop("Error: Incorrect arguments in find_DMR")
  }

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



check_args_get_top <- function(n){

  if(!zapsmall(n, 15) == round(n) | n <= 0)
    stop("Error: Incorrect n argument")
}


check_result_data <- function(data){

  data.colnames <- c('chr', 'start', 'end', 'p.value')
  if(!all.equal(colnames(data[,1:4]), data.colnames))
    stop("Error: Incorrect colnames in data")

  data.types <- c("character","integer","integer","double")
  names(data.types) <- data.colnames

  if(!all.equal(sapply(data[,1:4], typeof), data.types))
    stop("Error: Incorrect datatypes in data")

  if (!all(data$start > 0) | !all(data$end > 0) | !all(data$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) |
      !all(data$p.value[is.finite(data$p.value)] >= 0) | !all(data$p.value[is.finite(data$p.value)] <= 1))
    stop("Error: Incorrect values in data")

}


check_stats <- function(stats){

  stats.colnames <- c("chr", "start", "end", "meth.cov", "meth.max_x",
                        "meth.max_y", "meth.mean_x", "meth.mean_y", "meth.min_x",  "meth.min_y",
                        "meth.sd_x",   "meth.sd_y",   "meth.diff",   "quantile")

    if(!all.equal(colnames(stats), stats.colnames))
      stop("Error: Incorrect colnames in data")

    stats.types <- c("character",    "double" ,   "double" ,   "double",    "double",    "double",
                    "double",    "double",    "double",    "double",    "double",    "double" ,
                    "double",    "double")

    names(stats.types) <- stats.colnames

    if(!all.equal(sapply(stats, typeof), stats.types))
      stop("Error: Incorrect datatypes in data")

    if (!all(stats$start > 0) | !all(stats$end > 0) | !all(stats$chr %in% paste0('chr', c(1:22, "X", "Y", "M"))) |
        !all(zapsmall(stats$meth.cov, 15) == round(stats$meth.cov)) | !all(stats$meth.cov >= 0) |
        !all(stats[,c("meth.max_x", "meth.max_y", "meth.mean_x", "meth.mean_y", "meth.min_x",  "meth.min_y",
                     "meth.diff",   "quantile")] >= 0) |
       !all(stats[,c("meth.max_x", "meth.max_y", "meth.mean_x", "meth.mean_y", "meth.min_x",  "meth.min_y",
                     "meth.diff",   "quantile")] <= 1))
      stop("Error: Incorrect values in data")

  }

