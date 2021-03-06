

TestFrame <- data.frame(
  scoreText = c(
    "28-T. Cohen to NAT 42 for 4 yards (53-M.Carney)."
    ,"7-G.Hankerson complete to 15-M. Street. 15-M. Street to NFS 47 for 10 yards."
    ,"19-T.St.Germain punts 50 yards from NIC 9 to ULM 41, fair catch by 8-J.Williams."
    ,"8-T.Figaro complete to 3-D.Watson II. 3-D.Watson II to ULM 26 for 8 yards."
    ,"13-N.Dorka, Jr. 21 yards Field Goal is Good."
    )
  )


context("Player Names")
test_that("Player names are correctly cleaned", {
  cc <- cleanPlayers(TestFrame)
  
  expect_equal(cc$scoreText,
    c(
      "28-T.Cohen to NAT 42 for 4 yards (53-M.Carney)."
      ,"7-G.Hankerson complete to 15-M.Street. 15-M.Street to NFS 47 for 10 yards."
      ,"19-T.St-Germain punts 50 yards from NIC 9 to ULM 41, fair catch by 8-J.Williams."
      ,"8-T.Figaro complete to 3-D.Watson-II. 3-D.Watson-II to ULM 26 for 8 yards."
      ,"13-N.Dorka-Jr 21 yards Field Goal is Good."
      )
    )
})


