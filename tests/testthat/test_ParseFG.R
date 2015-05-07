

TestFrame <- data.frame(
  scoreText = c(
    '45-H.Stover 30 yards Field Goal is Good.'
    ,'11-T.Hunt 36 yards Field Goal is No Good.'
    ,'45-H.Stover extra point is good.'
    )
  )


context("Field Goal Attempts")
test_that("Field Goal Attempts are correctly parsing", {
  FG <- ParseFG(TestFrame)
  
  expect_true(all(FG$FGAtt))
  expect_equal(FG$FGYards,
                    c(30,36,20))
  expect_equal(FG$Kicker,
                    c("45-H.Stover"
                      ,"11-T.Hunt"
                      ,"45-H.Stover"
                      ))
  expect_equal(FG$FGGood,
                    c(TRUE
                      ,FALSE
                      ,TRUE
                      ))
  
  
})