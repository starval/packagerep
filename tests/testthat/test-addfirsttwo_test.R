
#tests spezifizieren
test_that("first two numbers are added", {
  expect_equal(addFirstTwo(c(0,0)), 0)
})

#tests laufen lassen
#devtools:::test()

#covr testet wir viele functionen im package getestet worden sind
