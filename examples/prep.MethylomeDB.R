library(dplyr)
library(magrittr)
library(readr)
library(data.table)

prep.data <- function(name){
  data <- list()
  for (i in 1:4){
    data[[i]] <- name[i]
  }
  
  data <- lapply(data, FUN = function(x){
    read_delim(x, '\t', escape_double = F, trim_ws = T) %>% 
      filter(chr == 'chr1') %>%
      mutate(meth = round(methylation * coverage)) %>% 
      mutate(unmeth = coverage - meth) %>% select(meth, unmeth, chr, position)
  }) 
  
  data[[1]] %>% inner_join(data[[2]],by=c("chr", "position")) %>%
    inner_join(data[[3]],by=c("chr", "position")) %>% 
    inner_join(data[[4]],by=c("chr", "position")) -> data
  
  setDT(data)
  data[ ,meth := rowSums(.SD), .SDcols = substr(names(data), 1, 4) == 'meth']
  data[ ,unmeth := rowSums(.SD), .SDcols = substr(names(data), 1, 6) == 'unmeth']
  
  data %>% mutate(
    meth.rate = as.double(meth/(unmeth + meth)),
    no = as.integer(unmeth + meth),
    meth = as.integer(meth),
    unmeth = as.integer(unmeth),
    poz = as.integer(position)) %>%
    select(chr, poz, no, meth, unmeth, meth.rate) -> d2
}

control_name <- c('~/Pulpit/metylacja/Control1_AC', '~/Pulpit/metylacja/Control2_AC', 
                  '~/Pulpit/metylacja/Control3_AC', '~/Pulpit/metylacja/Control4_AC')

control <- prep.data(control_name)
saveRDS(control, 'control.rds')

disease_name <- c('~/Pulpit/metylacja/SCZ1_AC', '~/Pulpit/metylacja/SCZ2_AC', 
                  '~/Pulpit/metylacja/SCZ3_AC', '~/Pulpit/metylacja/SCZ4_AC')

disease <- prep.data(disease_name)
saveRDS(disease, 'disease.rds')
