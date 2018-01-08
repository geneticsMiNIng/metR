data('schizophrenia')
control <- schizophrenia %>% filter(category == 'control') %>%
dplyr::select(-category)

disease <- schizophrenia %>% filter(category == 'disease') %>%
 dplyr::select(-category)

data <- preprocessing(control, disease)
data.tiles <- create_tiles_max_gap(data, gaps.length = 100)

test_that("Results from get_stat function are OK",{
  stats <- get_stats(data.tiles)
  expect_true(all(round(stats[11,2:14],3) == c(84100, 84338, 7,0.8,  0.955,  0.8,
                                  0.955, 0.8,  0.955,  0,  0, 0.155, 0.922)))
})


test_that("Incorrect arguments in get_stats",{
  expect_error(get_stats())
  colnames(data.tiles) <- 1:8
  expect_error(get_stats(data.tiles))

})
