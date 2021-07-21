test_that("Round2 Returns appropriate value", {
  expect_equal(round2(1.45,1),1.5)
  expect_equal(round2(1.44,1),1.4)
  expect_equal(round2(1.11),1.1)
  expect_equal(round2(1.5767, 0), 2)
  expect_equal(round2(1.45, 0), 1)
  expect_equal(round2(1.49999, 0), 1)
  expect_equal(round2(1.51199, 0), 2)
  expect_equal(round2(0.4449, 0), 0)
  expect_equal(round2(0.4449, 1), 0.4)
  expect_equal(round2(0.5449, 0), 1)
  expect_equal(round2(0.5449, 1), 0.5)
})

# Margin of Error Calculator

test_that("Margin_calculator returns a non-zero margin of error when compared value is at lower and upper ends of data - Alabama - NTL", {
  expect_equal(round2(margin_calculator(dplyr::filter(estimates, year == 2012), "AL", "NTL", 10), 1),  14.3)
  expect_equal(round2(margin_calculator(dplyr::filter(estimates, year == 2012), "AL", "NTL", 17), 1),  14.3)
  expect_equal(round2(margin_calculator(dplyr::filter(estimates, year == 2012), "AL", "NTL", 18000), 1),  14.3)
  expect_equal(round2(margin_calculator(dplyr::filter(estimates, year == 2012), "AL", "NTL", 55000), 1),  14.3)
})

# 2012 Estimates

test_that("Indicator_min returns the minimum - Alabama", {
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"AL","CHL"),1.27)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"AL","SECCHI"),0.525)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"AL","PTL"),12)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"AL","NTL"),248)
})

test_that("Indicator_min returns the minimum - Region_1", {
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"Region_1","CHL"),0.328)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"Region_1","SECCHI"),0.625)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"Region_1","PTL"),4)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"Region_1","NTL"),100)
})

test_that("Indicator_min returns the minimum - all_sites", {
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"All_Sites","CHL"),0)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"All_Sites","SECCHI"),0.0195)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"All_Sites","PTL"),4)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2012),"All_Sites","NTL"),14)
})

test_that("Indicator Max returns the max - Alabama", {
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"AL","CHL"),764.64)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"AL","SECCHI"),28)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"AL","PTL"),3636)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"AL","NTL"),54000)
})

test_that("Indicator Max returns the max - Region_1", {
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"Region_1","CHL"),764.64)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"Region_1","SECCHI"),28)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"Region_1","PTL"),3636)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"Region_1","NTL"),54000)
})

test_that("Indicator Max returns the max - all_sites", {
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"All_Sites","CHL"),764.64)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"All_Sites","SECCHI"),28)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"All_Sites","PTL"),3636)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2012),"All_Sites","NTL"),54000)
})

# 2017 Estimates

test_that("Indicator_min returns the minimum - Alabama", {
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2017),"AL","CHL"),9.2)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2017),"AL","SECCHI"),0.45)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2017),"AL","PTL"),14.78625)
  expect_equal(indicator_min(dplyr::filter(estimates, year == 2017),"AL","NTL"),321)
})

test_that("Indicator Max returns the max - Alabama", {
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2017),"AL","CHL"),3299.18)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2017),"AL","SECCHI"),23.6)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2017),"AL","PTL"),11238.8)
  expect_equal(indicator_max(dplyr::filter(estimates, year == 2017),"AL","NTL"),36375)
})


