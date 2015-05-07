

TestFrame <- data.frame(
  scoreText = c(
    '45-H.Stover kicks 65 yards from ULL 35 to NEV End Zone. touchback.'
    ,'11-T.Hunt kicks 58 yards from NEV 35. 24-G.Fuselier to ULL 4 for -3 yards.'
    ,'39-A.Phillips kicks 61 yards from UTH 35. 2-D.Gaines to CSU 20 for 16 yards.'
    ,'47-J.Roberts kicks 25 yards from CSU 35 to the UTH 40, downed by 52-H.Clegg.'
    ,'49-H.Hunt kicks 45 yards from CSU 13. 8-K.Clay pushed ob at UTH 42 for no gain.'
    ,'48-D.Cadona kicks 59 yards from ULL 21. 2-R.Turner pushed ob at NEV 20 for no gain (48-D.Cadona).'
    ,'47-J.Mattox kicks 63 yards from TEP 35, out of bounds at the UTS 2.'
    )
  )


context("Kickoffs")
test_that("Kickoffs are correctly parsing", {
  Kick <- ParseKick(TestFrame)
  
  expect_true(all(Kick$Kick))
  expect_equal(Kick$KickYards,
                    c(65,58,61,25,45,59,63))
  expect_equal(Kick$Kicker,
                    c("45-H.Stover"
                      ,"11-T.Hunt"
                      ,"39-A.Phillips"
                      ,"47-J.Roberts"
                      ,"49-H.Hunt"
                      ,"48-D.Cadona"
                      ,"47-J.Mattox"
                      ))
  expect_equal(Kick$KickReturn,
                    c(FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ))
  
  expect_equal(Kick$KickReturnYards,
                  c(NA
                    ,-3
                    ,16
                    ,NA
                    ,0
                    ,0
                    ,NA
                    ))
  
  
})