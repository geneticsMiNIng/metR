

main_preprocessing <- function(sample.1, sample.2){
  sample.1 %>% inner_join(sample.2, by = c("chr"="chr", "poz"="poz")) %>%
    gather(key, value, -chr, -poz) %>%
    tidyr::extract(key, c('type', "prob"), "(.*)\\.(.)") %>%
    spread(type, value) %>% dplyr::select(chr, poz, prob, no, meth, unmeth, meth.rate) %>%
    mutate(no = as.integer(no), meth = as.integer(meth), unmeth = as.integer(unmeth))
}


main_create_tiles_fixed_length <- function(data, tiles.length, common){
  data %<>% mutate(tiles = as.integer(poz %/% tiles.length))
  if (common == T){
    common.length = tiles.length %/% 2
    data %<>% mutate(tiles.common = as.integer((poz + common.length) %/% tiles.length))
  }
data
}


main_get_stats <- function(data, map){
  data %<>% mutate(poz.new = poz)
  setDT(data)

  data <- data[map, on=.(poz.new >= start, poz.new <= end, chr = chr), nomatch = 0,
               .(chr, poz, prob, no, meth, unmeth, meth.rate, start, end), allow.cartesian = T] %>%
    group_by(chr, start, end, prob) %>%
    summarise(meth.mean = mean(meth.rate), meth.cov = n(), meth.sd = sd(meth.rate),
              meth.min = min(meth.rate), meth.max = max(meth.rate))  %>%
    gather(Var, val, starts_with("meth.")) %>%
    unite(Var1,Var, prob) %>%
    spread(Var1, val) %>% dplyr::select(-meth.cov_x) %>% rename(meth.cov = meth.cov_y) %>%
    mutate(meth.diff = abs(meth.mean_x - meth.mean_y)) -> data

  data$quantile <- mapply(get_quantile, x = data$meth.diff, n = data$meth.cov)
  data
}

main_prep_data_draw_metylation <- function(data, chromosom, start, end){
  DT <- data %>% filter(chr == chromosom, poz >=start, poz <= end)

  DT1 <- DT %>% mutate(cov = round(log(no),1)) %>%  dplyr::select(poz, chr, prob, cov) %>% spread(prob, cov) %>%
    rename(cov_I = x, cov_II = y)

  DT  %>% dplyr::select(poz, chr, meth.rate, prob) %>% spread(prob, meth.rate) %>%
    rename(met_I = x, met_II = y) %>% left_join(DT1, by = c("poz", "chr"))

}


main_draw_metylation <- function(DT, chromosom, start, end, bind.probes,
                                 smooth.methylation,
                                 size.x.dot, size.y.dot,
                                 plot.title, axis.title.x,
                                 axis.title.y, legend.position, axis.text.x, axis.text.y,
                                 legend.text, legend.title){
  p <- ggplot(DT) + ylim(0,1) +
    scale_x_continuous(breaks = c(start, end),labels = c(start, end), limits = c(start, end)) +
    geom_point(DT,map=aes(x=poz, y=met_I, colour=cov_I), size = size.x.dot , shape = 20) +
    scale_color_gradient(low="pink1", high="red", name = "readsX") +
    geom_point(DT,map=aes(x=poz, y=met_II, fill=cov_II), size = size.y.dot, shape = 21) +
    scale_fill_gradient(low="cadetblue1", high="royalblue4", name = "readsY")

  x = as.matrix(DT[,1])
  y1 = as.matrix(DT[,'met_I'])
  y2 = as.matrix(DT[,'met_II'])


  p <- p + ggtitle(paste0("Methylation rate in ", chromosom)) +
    xlab("DNA position") + ylab("Methylation rate [%]") + theme_minimal() +
    theme(
      plot.title = element_text(size=plot.title),
      axis.title.x = element_text(size=axis.title.x),
      axis.title.y = element_text(size=axis.title.y),
      legend.position=legend.position,
      axis.text.x = element_text(size=axis.text.x),
      axis.text.y = element_text(size=axis.text.y),
      legend.text = element_text(size = legend.text),
      legend.title = element_text(size = legend.title),
      plot.margin = unit(c(1,1,1,1), "cm"))


  if (bind.probes){
    p <- p + geom_segment(aes(x = x, y = y1, xend = x, yend = y2) )
  }

  if (smooth.methylation){
    p <-  p + geom_smooth(aes(x = poz,y = met_I), colour = 'red', se = FALSE)  +
      geom_smooth(aes(x = poz,y = met_II), colour = "royalblue2", se = FALSE)
  }
  suppressWarnings(suppressMessages(print(p)))
}
