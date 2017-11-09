sample.name.1 <- 'Sample_40092EU.bismark.CG.tab'
sample.name.2 <- 'Sample_40092EK.bismark.CG.tab'

library(readr)
library(dplyr)

sample.1 <- suppressMessages(read_delim(sample.name.1, delim = "\t"))
sample.2 <- suppressMessages(read_delim(sample.name.2, delim = "\t"))

prep.WUM <- function(sample){
  sample %>%
    rename(chr = `_CHROM`, poz = POS, no = DP) %>%
    mutate(meth.rate = as.double(1 - AF),
           no = as.integer(no), meth = as.integer(meth),
           unmeth = as.integer(unmeth)) %>%
    dplyr::select(chr, poz, no, meth, unmeth, meth.rate)
  
}
  

sample.1 <- prep.WUM(sample.1)
sample.2 <- prep.WUM(sample.2)

library(metR)
data <- preprocessing(sample.1, sample.2)
head(data)
