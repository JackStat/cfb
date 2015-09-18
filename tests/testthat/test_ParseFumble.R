
TestFrame <- data.frame(
  scoreText = c(
    # '2-K.Myers to TEP 39, FUMBLES. 2-K.Myers to TEP 39 for 0 yards.'
    # '2-K.Myers sacked at UTS 24 for -8 yards, FUMBLES (1-A.James). to UTS 24 for 0 yards.'
    "FUMBLES (8-K.Correa). 73-U.Lapuaho to BYU 22 for 0 yards."
    ,"FUMBLES (29-W.Ossai). 6-N.Romine to AF 26 for 0 yards." 
    )
  )


context("Fumbles")
test_that("Fumbles are correctly parsing", {
  Fumbles <- ParseFumble(TestFrame)
  
  expect_true(all(Fumbles$Fumble))
  expect_equal(Fumbles$FumbleReturnYards,
                    c(0, 0))
  
})