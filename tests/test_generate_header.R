test_that("Generate Header Works",{
  expect_equal(generate_header("All_Sites","NTL",165.6148048,"test_lake","Nationally"),"Nationally, test_lake is in the 4th percentile.")
})

test_that("Small Values are caught",{
  expect_equal(generate_header("All_Sites","NTL",2.2,"test_lake","Nationally"),"Nationally, test_lake's value is at or below the lowest NLA result.")
})

test_that("Large Values are caught",{
  expect_equal(generate_header("All_Sites","NTL",2000000,"test_lake","Nationally"),"Nationally, test_lake's value is above the highest NLA result.")
})