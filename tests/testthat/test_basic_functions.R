data('schizophrenia')
control <- schizophrenia %>% filter(category == 'control') %>%
dplyr::select(-category)

disease <- schizophrenia %>% filter(category == 'disease') %>%
dplyr::select(-category)

data <- preprocessing(control, disease)
data.tiles <- create_tiles_max_gap(data, gaps.length = 100)
data.test <- data.tiles %>% filter(tiles == 10)
data.test.2 <- data.tiles %>% filter(chr == 'chr1', poz > 80000, poz < 100000)

test_t(data.test)
test_t(data.test.2)

test_that("Results from basic functions are OK",{
expect_true(all(round(test_reg_log(data.test.2), 3) == c(0,  0.982)))
expect_true(all(round(test_reg_log(data.test), 3) == c(0.018,  1.652)))

expect_true(round(test_ks(data.test), 3) == 0.1)
expect_true(round(test_ks(data.test.2), 3) == 0)

expect_true(round(test_t(data.test), 3) == 0.001)
expect_true(round(test_t(data.test.2), 3) == 0)

expect_true(round(test_wilcoxon(data.test), 3) == 0.174)
expect_true(round(test_wilcoxon(data.test.2), 3) == 0)
})

