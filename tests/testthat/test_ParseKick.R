

TestFrame <- data.frame(
  scoreText = c(
    '45-H.Stover kicks 65 yards from ULL 35 to NEV End Zone. touchback.'
    ,'11-T.Hunt kicks 58 yards from NEV 35. 24-G.Fuselier to ULL 4 for -3 yards.'
    ,'39-A.Phillips kicks 61 yards from UTH 35. 2-D.Gaines to CSU 20 for 16 yards.'
    ,'47-J.Roberts kicks 25 yards from CSU 35 to the UTH 40, downed by 52-H.Clegg.'
    ,'49-H.Hunt kicks 45 yards from CSU 13. 8-K.Clay pushed ob at UTH 42 for no gain.'
    ,'48-D.Cadona kicks 59 yards from ULL 21. 2-R.Turner pushed ob at NEV 20 for no gain (48-D.Cadona).'
    ,'47-J.Mattox kicks 63 yards from TEP 35, out of bounds at the UTS 2.'
    ,'98-C.Rouleau kicks 49 yards from JVS 35 to AUB 16, fair catch by 36-K.Pettway.'
    ,"16-A.Gantz kicks 65 yards from CIN 35. 5-J.Thomas runs 100 yards for a touchdown."
    ,"12-L.Kaba kicks 65 yards from TXST 35. -F.Anderson to PRV 35 for 35 yards."
    ,"16-A.Gantz kicks 65 yards from CIN 35 to TEM End Zone. 5-M.Boone, touchback." 
    ,"kicks 16 yards from BALL 35. to TXAM 49 for 0 yards."
    ,"91-N.O-Toole kicks 65 yards from WVU 35 to LIB End Zone. touchback."                                                
    ,"91-N.O-Toole kicks 30 yards from WVU 35, out of bounds at the LIB 35." 
    ,"91-N.O-Toole kicks 56 yards from WVU 35. 7-D.King to LIB 22 for 13 yards (44-H.Christian)."
    ,"11-T.Grassman kicks 16 yards from BUF 35 to the PSU 49, downed by 88-M.Gesicki to PSU 49 for 0 yards."
    )
  )


context("Kickoffs")
test_that("Kickoffs are correctly parsing", {
  Kick <- ParseKick(TestFrame)
  
  expect_true(all(Kick$Kick))
  expect_equal(Kick$KickYards,
                    c(65,58,61,25,45,59,63,49,65,65,65,16,65,30,56,16))
  expect_equal(Kick$Kicker,
                    c("45-H.Stover"
                      ,"11-T.Hunt"
                      ,"39-A.Phillips"
                      ,"47-J.Roberts"
                      ,"49-H.Hunt"
                      ,"48-D.Cadona"
                      ,"47-J.Mattox"
                      ,"98-C.Rouleau"
                      ,"16-A.Gantz"
                      ,"12-L.Kaba"
                      ,"16-A.Gantz"
                      ,""
                      ,"91-N.O-Toole"
                      ,"91-N.O-Toole"
                      ,"91-N.O-Toole"
                      ,"11-T.Grassman"
                      ))
  expect_equal(Kick$KickReturn,
                    c(FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ,FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ,TRUE
                      ,FALSE
                      ,FALSE
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
                    ,NA
                    ,100
                    ,35
                    ,NA
                    ,0
                    ,NA
                    ,NA
                    ,13
                    ,NA
                    ))
  
  
})