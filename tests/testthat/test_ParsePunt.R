

TestFrame <- data.frame(
  scoreText = c(
    '49-H.Hunt punts 31 yards from CSU 22, out of bounds at the UTH 47.'
    ,'33-T.Hackett punts 39 yards from UTH 23. 24-P.Hodges to CSU 36 for -2 yards.'
    ,'33-T.Hackett punts 40 yards from UTH 40 to CSU 20, fair catch by 25-J.Hansley.'
    ,'49-H.Hunt punts 31 yards from CSU 22, out of bounds at the UTH 47.'
    ,'49-H.Hunt punts 44 yards from UTH 44 to UTH End Zone. touchback.'
    ,'49-H.Hunt punts 45 yards from CSU 13. 8-K.Clay pushed ob at UTH 42 for no gain.'
    ,'48-D.Cadona punts 59 yards from ULL 21. 2-R.Turner pushed ob at NEV 20 for 0 yards (48-D.Cadona).'
    ,'43-L.McPherson punts 39 yards from LT 30 Downed at the NTX 31.'
    ,'14-A.Melchiori punts 33 yards from KNT 49 Downed at the TOL 18.'
    ,'43-L.McPherson punts 45 yards from LT 29 to the NTX 26, downed by 28-K.Dixon.'
    ,'26-M.Haack punts 39 yards from ASU 40. 3-C.Kirk runs 79 yards for a touchdown.'
    ,"27-A.Hughes punts 38 yards from VT 14 to the OSU 48, downed by 59-E.D'Antuono."
    # ,"45-L.Yunker punts 0 yards from UNLV 41 blocked by 98-T.McKinley. 98-T.McKinley to UNLV 9 for 0 yards."
    ,"84-E.Keena punts 35 yards from NTX 27 to SMU 38, fair catch by." 
    ,"punts -13 yards from NEV 29, out of bounds at the NEV 16."
    ,"91-N.O-Toole punts 43 yards from WVU 39. 1-Z.Parker to LIB 21 for 3 yards (44-H.Christian)."
    )
  )


context("Punts")
test_that("Punts are correctly parsing", {
  Punt <- ParsePunt(TestFrame)
  
  expect_true(all(Punt$Punt))
  expect_equal(Punt$PuntYards,
                    c(31,39,40,31,44,45,59,39,33,45,39,38,35,-13,43))
  expect_equal(Punt$Kicker,
                    c("49-H.Hunt"
                      ,"33-T.Hackett"
                      ,"33-T.Hackett"
                      ,"49-H.Hunt"
                      ,"49-H.Hunt"
                      ,"49-H.Hunt"
                      ,"48-D.Cadona"
                      ,"43-L.McPherson"
                      ,"14-A.Melchiori"
                      ,"43-L.McPherson"
                      ,"26-M.Haack"
                      ,"27-A.Hughes"
                      ,"84-E.Keena"
                      ,""
                      ,"91-N.O-Toole"
                      ))
  expect_equal(Punt$PuntReturn,
                    c(FALSE
                      ,TRUE
                      ,FALSE
                      ,FALSE
                      ,FALSE
                      ,TRUE
                      ,TRUE
                      ,FALSE
                      ,FALSE
                      ,FALSE
                      ,TRUE
                      ,FALSE
                      ,FALSE
                      ,FALSE
                      ,TRUE
                      ))
  
  expect_equal(Punt$PuntReturnYards,
                  c(NA
                    ,-2
                    ,NA
                    ,NA
                    ,NA
                    ,0
                    ,0
                    ,0
                    ,0
                    ,0
                    ,79
                    ,0
                    ,NA
                    ,NA
                    ,3
                    ))
  
  
})