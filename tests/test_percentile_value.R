test_that("Percentile Values work for All Sites - CHL", {
  expect_equal(percentile_value(estimates,"All_Sites","CHL",0),0)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",1.272780988),5)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",8.710652276),50)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",24.9244048),75)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",96),94)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",105.2232224),95)
  expect_equal(percentile_value(estimates,"All_Sites","CHL",2000),100)
})

test_that("Percentile Values work for All Sites - NTL", {
  expect_equal(percentile_value(estimates,"All_Sites","NTL",0),-Inf)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",165.6148048),4)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",691.0659319),50)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",1074.247088),75)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",2925.288587),95)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",158),4)
  expect_equal(percentile_value(estimates,"All_Sites","NTL",7500),100)
})

test_that("Percentile Values work for All Sites - PTL", {
  expect_equal(percentile_value(estimates,"All_Sites","PTL",0),-Inf)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",9.161193182),5)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",37.10675882),50)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",87.78372891),75)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",391.406515),95)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",158),89)
  expect_equal(percentile_value(estimates,"All_Sites","PTL",7500),100)
})

test_that("Percentile Values work for All Sites - SECCHI", {
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",0),-Inf)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",0.279354944),5)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",1.437529308),48)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",3.00727161),75)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",5.695188612),94)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",5),93)
  expect_equal(percentile_value(estimates,"All_Sites","SECCHI",100),100)
})

#### Region_1 ####

test_that("Percentile Values work for Region_1 - NTL", {
  expect_equal(percentile_value(estimates,"Region_1","NTL",0),-Inf)
  expect_equal(percentile_value(estimates,"Region_1","NTL",142.1295169),4)
  expect_equal(percentile_value(estimates,"Region_1","NTL",299.2637381),46)
  expect_equal(percentile_value(estimates,"Region_1","NTL",399.7152194),71)
  expect_equal(percentile_value(estimates,"Region_1","NTL",678.6753737),94)
  expect_equal(percentile_value(estimates,"Region_1","NTL",400),77)
  expect_equal(percentile_value(estimates,"Region_1","NTL",7500),100)
})

 test_that("Percentile Values work for Region_1 - SECCHI", {
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",0),-Inf)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",1.038552755),2)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",2.801030799),50)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",3.348943841),71)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",5.9151736),93)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",5),89)
   expect_equal(percentile_value(estimates,"Region_1","SECCHI",100),100)
 })

 test_that("Percentile Values work for Region_1 - CHL", {
   expect_equal(percentile_value(estimates,"Region_1","CHL",0),0)
   expect_equal(percentile_value(estimates,"Region_1","CHL",1.272780988),2)
   expect_equal(percentile_value(estimates,"Region_1","CHL",8.710652276),76)
   expect_equal(percentile_value(estimates,"Region_1","CHL",24.9244048),95)
   expect_equal(percentile_value(estimates,"Region_1","CHL",96),100)
   expect_equal(percentile_value(estimates,"Region_1","CHL",105.2232224),100)
   expect_equal(percentile_value(estimates,"Region_1","CHL",2000),100)
 })

 test_that("Percentile Values work for Region_1 - PTL", {
   expect_equal(percentile_value(estimates,"Region_1","PTL",0),-Inf)
   expect_equal(percentile_value(estimates,"Region_1","PTL",9.161193182),25)
   expect_equal(percentile_value(estimates,"Region_1","PTL",37.10675882),90)
   expect_equal(percentile_value(estimates,"Region_1","PTL",87.78372891),100)
   expect_equal(percentile_value(estimates,"Region_1","PTL",391.406515),100)
   expect_equal(percentile_value(estimates,"Region_1","PTL",38),90)
   expect_equal(percentile_value(estimates,"Region_1","PTL",7500),100)
})
 
 #### Alabama ####
 
 test_that("Percentile Values work for Alabama - NTL", {
   expect_equal(percentile_value(estimates,"AL","NTL",0),-Inf)
   expect_equal(percentile_value(estimates,"AL","NTL",142.1295169),0)
   expect_equal(percentile_value(estimates,"AL","NTL",299.2637381),8)
   expect_equal(percentile_value(estimates,"AL","NTL",399.7152194),74)
   expect_equal(percentile_value(estimates,"AL","NTL",678.6753737),92)
   expect_equal(percentile_value(estimates,"AL","NTL",400),74)
   expect_equal(percentile_value(estimates,"AL","NTL",7500),100)
 })
 
 test_that("Percentile Values work for AL - SECCHI", {
   expect_equal(percentile_value(estimates,"AL","SECCHI",0),-Inf)
   expect_equal(percentile_value(estimates,"AL","SECCHI",1.038552755),40)
   expect_equal(percentile_value(estimates,"AL","SECCHI",2.801030799),100)
   expect_equal(percentile_value(estimates,"AL","SECCHI",3.348943841),100)
   expect_equal(percentile_value(estimates,"AL","SECCHI",5.9151736),100)
   expect_equal(percentile_value(estimates,"AL","SECCHI",5),100)
   expect_equal(percentile_value(estimates,"AL","SECCHI",100),100)
 })
 
 test_that("Percentile Values work for AL - CHL", {
   expect_equal(percentile_value(estimates,"AL","CHL",0),0)
   expect_equal(percentile_value(estimates,"AL","CHL",1.272780988),8)
   expect_equal(percentile_value(estimates,"AL","CHL",8.710652276),8)
   expect_equal(percentile_value(estimates,"AL","CHL",24.9244048),88)
   expect_equal(percentile_value(estimates,"AL","CHL",23),88)
   expect_equal(percentile_value(estimates,"AL","CHL",105.2232224),100)
   expect_equal(percentile_value(estimates,"AL","CHL",2000),100)
 })
 
 test_that("Percentile Values work for AL - PTL", {
   expect_equal(percentile_value(estimates,"AL","PTL",0),-Inf)
   expect_equal(percentile_value(estimates,"AL","PTL",9.161193182),0)
   expect_equal(percentile_value(estimates,"AL","PTL",37.10675882),74)
   expect_equal(percentile_value(estimates,"AL","PTL",87.78372891),100)
   expect_equal(percentile_value(estimates,"AL","PTL",391.406515),100)
   expect_equal(percentile_value(estimates,"AL","PTL",38),74)
   expect_equal(percentile_value(estimates,"AL","PTL",7500),100)
 })
 