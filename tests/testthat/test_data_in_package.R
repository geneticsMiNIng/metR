data('schizophrenia')
test_that("Load_schizophrenia",{
  expect_true(is.data.frame(schizophrenia))
})


data('quantile.function')
test_that("Load_quantile.function",{
  expect_true(is.list(quantile.function))
})

data('mean.acf.chr')
test_that("Load_mean.acf.chr",{
  expect_true(is.numeric(mean.acf.chr))
})

data('find.DMR.results')
test_that("Load_find.DMR.results",{
  expect_true(is.list(find.DMR.results))
})
