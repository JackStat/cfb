

TestFrame <- data.frame(
  scoreText = c(
    '18-R.Butler incomplete. Penalty on TEP 6-D.Payne, Pass interference, 15 yards, enforced at TEP 29. No Play.'
    # ,'23-D.Booker to ORS 6 for 53 yards. Team penalty on ORS, Offside, declined.'
    ,'Penalty on UTS 32-J.Hill, Illegal substitution, 5 yards, enforced at UTS 30. No Play.'
    ,'Penalty on TEP 77-C.Thomas, False start, 5 yards, enforced at TEP 26. No Play.'
    ,'Penalty on TEP 90-M.Chavis, Personal Foul, 15 yards, enforced at UTS 37. No Play.'
    ,'Penalty on TEP 1-J.Showers, Delay of game, 5 yards, enforced at TEP 37. No Play.'
    ,'Penalty on UTH 53-J.Whittingham, Unsportsmanlike conduct, 15 yards, enforced at UTH 35. No Play.'
    # ,'1-J.Showers incomplete. Intended for 14-I.Hamilton. Penalty on TEP 76-W.Hernandez, Ineligible player downfield during passing down, declined.'
    )
  )


context("Penalties")
test_that("Penalties are correctly parsing", {
  Penalties <- ParsePenalty(TestFrame)
  
  expect_true(all(Penalties$Penalty))
  expect_equal(Penalties$PenaltyYards,
                    c(15, 5, 5, 15, 5,15))
  expect_true(all(Penalties$PenaltyPlayer == 
                    c("6-D.Payne"
                      ,"32-J.Hill"
                      ,"77-C.Thomas"
                      ,"90-M.Chavis"
                      ,"1-J.Showers"
                      ,"53-J.Whittingham"
                      )
                  )
  )
  
})