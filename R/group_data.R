
group_tiles <- function(data, tiles_name){

    col_vector <- c('chr', tiles_name, 'start', 'end')

    data %>% group_by_('chr', tiles_name) %>%
    summarize(start = min(poz), end = max(poz)) %>%
    dplyr::select_(.dots = col_vector) -> map
    data %>% left_join(map, by = c('chr', tiles_name)) %>%
    select_('-contains("tiles")')
}

group_data <- function(data, prob){

  data.new <- group_tiles(data, 'tiles')

  if ('tiles.common' %in% colnames(data)){
    data.common.new <- grouping_tiles(data, 'tiles.common')
    rbind(data.new, data.common.new) %>% distinct() -> data.new
    rm(data.common.new)
  }
  if (prob == T){ data.new %>% group_by(chr, start, end, prob)
  } else{
    data.new %>% group_by(chr, start, end)}

  }

