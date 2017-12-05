create_map <- function(data){

  data %>% group_by(chr, tiles) %>%
    summarize(start = min(poz), end = max(poz)) %>%
    dplyr::select(c(chr, start, end)) -> map

  if ('tiles.common' %in% colnames(data)) {
    data %>% group_by(chr, tiles.common) %>%
      summarize(start = min(poz), end = max(poz)) %>%
      dplyr::select(c(chr, start, end)) -> map.common

    rbind(map, map.common) %>% distinct() -> map
    rm(map.common)
  }

  setDT(map)
  map
}
