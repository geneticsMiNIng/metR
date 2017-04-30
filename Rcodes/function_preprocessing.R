

create_tiles <- function(data, gaps_length){
  data %>% distinct(chr, poz) %>% arrange(chr, poz) %>%
    mutate(diff = poz - lag(poz), id = seq_along(poz))  -> poz
  
  s1 <- c(1, which(poz$diff >= gaps_length))
  s2 <- c(s1[-1] - 1, nrow(poz))
  tiles <- 1:length(s1)
  to_bind <- data.table(s1,s2,tiles)
  setDT(poz)
  poz <- poz[to_bind, on=.(id >= s1, id <= s2), nomatch = 0, .(poz, chr, tiles)]
  
  data %>% left_join(poz, on = c("poz"= "poz", "chr" = "chr")) 
  
}
