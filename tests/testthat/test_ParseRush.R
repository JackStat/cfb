

TestFrame <- data.frame(
  scoreText = c(
    '15-E.McGuire to NEV 46 for 6 yards (95-P.Choudja).'
    ,'8-T.Broadway to NEV 31 for -15 yards (50-J.McNeal).'
    ,'15-E.McGuire to NEV 46 for 6 yards.'
    ,'8-T.Broadway to NEV 31 for -15 yards.'
    ,'15-T.Hunt to UTAH 46 for 1 yard (95-P.Choudja).'
    ,'15-T.Hunt to UTAH 46 for 1 yard.'
    ,'15-T.Hunt scrambles to UTAH 46 for 1 yard.'
    ,'15-T.Hunt scrambles to UTAH 46 for 1 yard.'
    ,'8-T.Broadway scrambles to NEV 31 for -15 yards (50-J.McNeal).'
    ,'8-T.Broadway scrambles to NEV 31 for 15 yards (50-J.McNeal).'
    ,'15-T.Hunt scrambles to UTAH 46 for no gain.'
    ,'15-T.Hunt scrambles to UTAH 46 for no gain (50-J.McNeal).'
    ,'58-D.Dixon pushed ob UTAH 46 for no gain (50-J.McNeal).'
    ,'58-D.Dixon pushed ob UTAH 46 for no gain.'
    ,'58-D.Dixon pushed ob UTAH 46 for -15 yards.'
    ,'58-D.Dixon pushed ob UTAH 46 for 15 yards (50-J.McNeal).'
    ,'17-C.Fajardo pushed ob at ULL 11 for 15 yards (35-B.Anyama).'
    ,'8-T.Broadway scrambles to ULL 17 for 5 yards (93-D.Roberts-Jordan).'
    ,'7-T.Wilson runs 8 yards for a touchdown.'
    ,'18-G.Grayson scrambles, runs ob at UTH 44 for 2 yards.'
    ,'7-T.Wilson runs ob at UTH 40 for 2 yards.'
    ,'6-C.Clement to BGN 8 for 8 yards (35-J.Sanford,63-G.Schwieterman).'
    ,'6-D.Jackson to NEV 34 for no gain (59-J.Molbert,58-D.Scheurich).'
    ,'16-N.Strock sacked at KNT 24 for -1 yard (97-O.Jones).'
    ,'18-G.Grayson sacked at CSU 33 for -10 yards (51-J.Fanaika).'
    ,'15-J.Rudock scrambles, pushed ob at UTH 34 for 1 yard (13-G.Paul).'
    ,"10-K.Allen scrambles, runs 12 yards for a touchdown."
    ,"7-E.Jenkins kneels at JVS 31 for -4 yards."
    ,'5-M.Julian kneels at KNT 33 for -2 yards.'
    ,'23-D.Booker to ORS 6 for 53 yards. Team penalty on ORS, Offside, declined.'
    ,"13-J.McNichols to BOISE 26 for 4 yards (4-F.Warner)."
    ,"15-D.Prescott spikes the ball at LSU 29 for 0 yards." 
    ,"-F.Anderson to TXST 46 for 1 yard."
    ,"kneels at ORE 48 for -2 yards."
    ,"to UMASS 28 for -14 yards."
    )
  )


FailTests <- data.frame(
    scoreText = c(
      "92-K.Bambard 33 yards Field Goal is No Good. blocked by 27-S.Absanon. to EKY 20 for no gain."
    )
)

  
  


context("Rushing Attempts")
test_that("Rushing Attempts are correctly parsing", {
  Rushes <- ParseRush(TestFrame)
  
  expect_true(all(Rushes$RushAtt))
  expect_equal(Rushes$RushYards,
                    c(6,-15,6,-15,1,1,1,1,-15,15,0,0,0,0,-15,15,15,5,8,2,2,8,0,-1,-10,1,12,-4,-2, 53,4, 0, 1, -2, -14))
  expect_true(all(Rushes$Rusher == 
                    c("15-E.McGuire"
                      ,"8-T.Broadway"
                      ,"15-E.McGuire"
                      ,"8-T.Broadway"
                      ,"15-T.Hunt"
                      ,"15-T.Hunt"
                      ,"15-T.Hunt"
                      ,"15-T.Hunt"
                      ,"8-T.Broadway"
                      ,"8-T.Broadway"
                      ,"15-T.Hunt"
                      ,"15-T.Hunt"
                      ,"58-D.Dixon"
                      ,"58-D.Dixon"
                      ,"58-D.Dixon"
                      ,"58-D.Dixon"
                      ,"17-C.Fajardo"
                      ,"8-T.Broadway"
                      ,"7-T.Wilson"
                      ,"18-G.Grayson"
                      ,"7-T.Wilson"
                      ,"6-C.Clement"
                      ,"6-D.Jackson"
                      ,"16-N.Strock"
                      ,"18-G.Grayson"
                      ,"15-J.Rudock"
                      ,"10-K.Allen"
                      ,"7-E.Jenkins"
                      ,"5-M.Julian"
                      ,"23-D.Booker"
                      ,"13-J.McNichols"
                      ,"15-D.Prescott"
                      ,"-F.Anderson"
                      ,""
                      ,""
                      )))  
  
  
  
  notRushes <- ParseRush(FailTests)
  
})