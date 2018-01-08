data('schizophrenia')
control <- schizophrenia %>% filter(category == 'control') %>%
 dplyr::select(-category)

disease <- schizophrenia %>% filter(category == 'disease') %>%
  dplyr::select(-category)
data <- preprocessing(control, disease)
data.tiles <- create_tiles_max_gap(data, gaps.length = 100)
data.tiles.small <- data.tiles %>% filter(tiles == 11)
DMR <- find_DMR(data.tiles.small, c('Wilcoxon', 'Ttest', 'KS', 'Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'))

test_that("Results from find_DMR function are OK",{
  expect_true(is.list(DMR))
  expect_true(all(names(DMR) == c("Wilcoxon" ,"Ttest" ,"KS" ,"Reg.Log" ,"Reg.Mixed" ,"Reg.Corr.Mixed")))
  expect_true(is.data.frame(DMR$Wilcoxon))
  })


test_that("Incorrect arguments in find_DMR",{
  expect_error(find_DMR())
  expect_error(find_DMR(data.tiles.small, c('Reg.Log', 'Reg.Mixed', 'Reg.Corr.Mixed'),
                        p.value.log.reg = '0.2'))

  expect_error(find_DMR(data.tiles.small, c('New.method')))

  expect_error(find_DMR(data.tiles.small))

  colnames(data.tiles.small) <- 1:8
  expect_error(find_DMR(data.tiles.small,  c('Wilcoxon')))


})
