

TestFrame <- data.frame(
  scoreText = c(
    "28-T. Cohen to NAT 42 for 4 yards (53-M.Carney)."
    ,"7-G.Hankerson complete to 15-M. Street. 15-M. Street to NFS 47 for 10 yards."
    )
  )


context("Player Names")
test_that("Player names are correctly cleaned", {
  cc <- cleanPlayers(TestFrame)
  
  expect_equal(cc$scoreText,
    c(
      "28-T.Cohen to NAT 42 for 4 yards (53-M.Carney)."
      ,"7-G.Hankerson complete to 15-M.Street. 15-M.Street to NFS 47 for 10 yards."
      )
    )
})


