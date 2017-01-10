# Calculate and plot ACF for methylation profile
library(data.table)
library(dplyr)

EK_EU <- readr::read_delim("~/Dropbox/ProjektyBadawcze/Metylacja/SampleBismarkCG/Sample_40092EK.bismark.CG.tab", delim = "\t")

# only chr 2
EK <- EK_EU %>% filter(`_CHROM`=="chr2")
EK$m <- log((EK$meth+1)/(EK$unmeth+1))

wek <- numeric(max(EK$POS))
wek[EK$POS] = EK$m
wek[wek==0] = NA

acfG <- acf(wek, lag.max=5000, na.action = na.pass)
library(forecast)
autoplot(acfG) + ggtitle("ACF w przedziale 0 - 5000 kbp; chr X") + 
  geom_vline(xintercept = 1000, color="red")+ 
  geom_vline(xintercept = 2000, color="red")+ 
  geom_vline(xintercept = 3000, color="red")+ 
  geom_vline(xintercept = 4000, color="grey80")+ 
  geom_vline(xintercept = 5000, color="red") + xlim(0,5000)

