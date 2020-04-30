test_that("Round2 Returns appropriate value", {
  expect_equal(round2(1.45,1),1.5)
  expect_equal(round2(1.44,1),1.4)
  expect_equal(round2(1.11),1.1)
})

test_that("Indicator_min returns the minimum - Alabama", {
  expect_equal(indicator_min(estimates,"AL","CHL"),1.27)
  expect_equal(indicator_min(estimates,"AL","SECCHI"),0.525)
  expect_equal(indicator_min(estimates,"AL","PTL"),12)
  expect_equal(indicator_min(estimates,"AL","NTL"),248)
})

test_that("Indicator_min returns the minimum - Region_1", {
  expect_equal(indicator_min(estimates,"Region_1","CHL"),0.328)
  expect_equal(indicator_min(estimates,"Region_1","SECCHI"),0.625)
  expect_equal(indicator_min(estimates,"Region_1","PTL"),4)
  expect_equal(indicator_min(estimates,"Region_1","NTL"),100)
})

test_that("Indicator_min returns the minimum - all_sites", {
  expect_equal(indicator_min(estimates,"All_Sites","CHL"),0)
  expect_equal(indicator_min(estimates,"All_Sites","SECCHI"),0.0195)
  expect_equal(indicator_min(estimates,"All_Sites","PTL"),4)
  expect_equal(indicator_min(estimates,"All_Sites","NTL"),14)
})

test_that("Indicator Max returns the max - Alabama", {
  expect_equal(indicator_max(estimates,"AL","CHL"),764.64)
  expect_equal(indicator_max(estimates,"AL","SECCHI"),28)
  expect_equal(indicator_max(estimates,"AL","PTL"),3636)
  expect_equal(indicator_max(estimates,"AL","NTL"),54000)
})

test_that("Indicator Max returns the max - Region_1", {
  expect_equal(indicator_max(estimates,"Region_1","CHL"),764.64)
  expect_equal(indicator_max(estimates,"Region_1","SECCHI"),28)
  expect_equal(indicator_max(estimates,"Region_1","PTL"),3636)
  expect_equal(indicator_max(estimates,"Region_1","NTL"),54000)
})

test_that("Indicator Max returns the max - all_sites", {
  expect_equal(indicator_max(estimates,"All_Sites","CHL"),764.64)
  expect_equal(indicator_max(estimates,"All_Sites","SECCHI"),28)
  expect_equal(indicator_max(estimates,"All_Sites","PTL"),3636)
  expect_equal(indicator_max(estimates,"All_Sites","NTL"),54000)
})


