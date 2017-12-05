create_tiles_on_chr <- function(data.chr, gaps.length){

  data.chr %>% distinct(poz) %>% arrange(poz) %>%
    mutate(diff = poz - lag(poz), id = seq_along(poz))  -> poz

  s1 <- c(1, which(poz$diff >= gaps.length))
  s2 <- c(s1[-1] - 1, nrow(poz))
  tiles <- 1:length(s1)

  id <- mapply(s1, FUN = function(s1,s2, tiles){
    data.frame(id = s1:s2, tiles = tiles)
  }, s2, tiles, SIMPLIFY = F)
  id <- do.call('rbind', id)

  data.chr %>% left_join(poz, by = c("poz"= "poz")) %>%
    left_join(id, c("id" = "id")) %>% dplyr::select(-c(diff, id))
}
