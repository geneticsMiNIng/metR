data('schizophrenia')
control <- schizophrenia %>% filter(category == 'control') %>%
dplyr::select(-category)

disease <- schizophrenia %>% filter(category == 'disease') %>%
dplyr::select(-category)

data <- preprocessing(control, disease)
data.colnames <- c("chr", "poz", "prob", "no", "meth","unmeth", "meth.rate")

test_that("Results from preprocessing function are OK",{
  expect_true(is.data.frame(data))
  expect_true(all(data.colnames == colnames(data)))
  })


test_that("Incorrect arguments in preprocessing",{
  expect_error(preprocessing(control))
  expect_error(preprocessing())

  colnames(disease) <- 1:6
  expect_error(preprocessing(control, disease))

  colnames(disease) <- colnames(control)
  disease$meth.rate <- disease$meth.rate + 1
  expect_error(preprocessing(control, disease))

})


test_that("Incorrect arguments in create_tiles_fixed_length",{
  expect_error(create_tiles_fixed_length(data, tiles.length = '1000', common = FALSE))
  expect_error(create_tiles_fixed_length(data, tiles.length = 1000, common = 'FALSE'))
  expect_error(create_tiles_fixed_length(data = data.frame(), tiles.length = 1000, common = FALSE))
  colnames(data) <- 1:7
  expect_error(create_tiles_fixed_length(data = data, tiles.length = 1000, common = FALSE))
  colnames(data) <- data.colnames
    })


test_that("Results from create_tiles_fixed_length function are OK",{
  tiles.1 <- create_tiles_fixed_length(data, tiles.length = 1000, common = FALSE)
  expect_true(all(tiles.1$tiles[1:10] == rep(81,10)))

  tiles.11 <- create_tiles_fixed_length(data, tiles.length = 1000, common = TRUE)
  expect_true(all(tiles.11$tiles.common[1:10] == c(81, 81, 81, 81, 82, 82, 82, 82, 82, 82)))
})


test_that("Incorrect arguments in create_tiles_max_gap",{
  expect_error(create_tiles_max_gap(data, gaps.length = '100'))
  expect_error(create_tiles_max_gap(data = data.frame(), gaps.length = 10))
  colnames(data) <- 1:7
  expect_error(create_tiles_max_gap(data = data, tiles.length = 1000, common = FALSE))
})


test_that("Results from preprocessing function are OK",{
  tiles.2 <- create_tiles_max_gap(data, gaps.length = 100)
  expect_true(all(tiles.2$tiles[1:10] == c(1,1,1,1,2,2,3,3,3,3)))
})

