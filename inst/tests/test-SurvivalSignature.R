context("Survival Signature")

test_that("Figure 1 example is correct", {
  # Define the structure
  fig1 <- graph.formula(2 -- 5, s -- 1 -- 2:3 -- 4 -- 5:6 -- t, 3 -- 6)
  # Specify the type of each component
  V(fig1)$compType <- V(fig1)$name
  V(fig1)$compType[match(c("1","2","5"), V(fig1)$name)] <- 1
  V(fig1)$compType[match(c("3","4","6"), V(fig1)$name)] <- 2
  V(fig1)$compType[match(c("s","t"), V(fig1)$name)] <- NA
  
  # Expect to see?
  res <- structure(list(`1` = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                   `2` = c(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3),
                   Probability = c(0, 0, 0, 0, 0, 0, 0.111111111111111, 0.333333333333333, 0, 0, 0.444444444444444, 0.666666666666667, 1, 1, 1, 1)),
                   .Names = c("1", "2", "Probability"), row.names = c(NA, -16L), class = "data.frame")
  resFrac <- structure(list(`1` = c(0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3),
                            `2` = c(0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3),
                            Probability = c("0", "0", "0", "0", "0", "0", "1/9", "1/3", "0", "0", "4/9", "2/3", "1", "1", "1", "1")),
                       .Names = c("1", "2", "Probability"), row.names = c(NA, -16L), class = "data.frame")
  
  expect_that(computeSystemSurvivalSignature(fig1), equals(res))
  expect_that(computeSystemSurvivalSignature(fig1, frac=TRUE), equals(resFrac))
})

test_that("Figure 3 example is correct", {
  # Define the structure
  fig3 <- graph.formula(s -- 1:4 -- 2:5 -- 3:6 -- t, s -- 7:8, 8 -- 9, 7:9 -- t)
  # Specify the type of each component
  V(fig3)$compType <- V(fig3)$name
  V(fig3)$compType[match(c("1"), V(fig3)$name)] <- 1
  V(fig3)$compType[match(c("2","3","4","7"), V(fig3)$name)] <- 2
  V(fig3)$compType[match(c("5","6","8","9"), V(fig3)$name)] <- 3
  V(fig3)$compType[match(c("s","t"), V(fig3)$name)] <- NA
  
  # Expect to see?
  res <- structure(list(`1` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                        `2` = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
                        `3` = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
                        Probability = c(0, 0, 0.166666666666667, 0.5, 1, 0.25, 0.25, 0.416666666666667, 0.75, 1, 0.5, 0.583333333333333, 0.75, 0.916666666666667, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0.333333333333333, 1, 1, 0.25, 0.375, 0.666666666666667, 1, 1, 0.666666666666667, 0.75, 0.888888888888889, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
                   .Names = c("1", "2", "3", "Probability"), row.names = c(NA, -50L), class = "data.frame")
  resFrac <- structure(list(`1` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                            `2` = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4),
                            `3` = c(0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4, 0, 1, 2, 3, 4),
                            Probability = c("0", "0", "1/6", "1/2", "1", "1/4", "1/4", "5/12", "3/4", "1", "1/2", "7/12", "3/4", "11/12", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "0", "0", "1/3", "1", "1", "1/4", "3/8", "2/3", "1", "1", "2/3", "3/4", "8/9", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1")),
                       .Names = c("1", "2", "3", "Probability"), row.names = c(NA, -50L), class = "data.frame")
  
  expect_that(computeSystemSurvivalSignature(fig3), equals(res))
  expect_that(computeSystemSurvivalSignature(fig3, frac=TRUE), equals(resFrac))
})
