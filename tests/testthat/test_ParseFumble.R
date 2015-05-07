
TestFrame <- data.frame(
  scoreText = c(
    '2-K.Myers to TEP 39, FUMBLES. 2-K.Myers to TEP 39 for 0 yards.'
    ,'2-K.Myers sacked at UTS 24 for -8 yards, FUMBLES (1-A.James). to UTS 24 for 0 yards.'
    )
  )


context("Fumbles")
test_that("Fumbles are correctly parsing", {
  Fumbles <- ParseFumble(TestFrame)
  
  expect_true(all(Fumbles$Fumble))
  expect_equal(Fumbles$FumbleYards,
                    c(0, 0))
  expect_true(all(Fumbles$Turnover == 
                    c(FALSE
                      ,TRUE
                      )
                  )
  )
  
})