

TestFrame <- data.frame(
  scoreText = c(
    '16-N.Strock incomplete.'
    ,'5-M.Julian incomplete. Intended for 80-M.Roberts.'
    ,'5-M.Julian complete to 24-D.Jones-Moore. 24-D.Jones-Moore to TOL 49 for 6 yards.'
    ,'11-L.Woodside complete to 4-C.Jones. 4-C.Jones to KNT 11 for 1 yard.'
    ,'16-N.Strock complete to 7-C.Pierce. 7-C.Pierce runs 3 yards for a touchdown.'
    ,'10-C.Reardon complete to 7-C.Pierce. 7-C.Pierce to KNT 25 for no gain.'
    ,'11-L.Woodside complete to 9-A.Russell. 9-A.Russell runs ob at TOL 33 for 8 yards.'
    ,'11-L.Woodside complete to 9-A.Russell. 9-A.Russell pushed ob at KNT 11 for 5 yards.'
    ,'8-T.Broadway complete to 24-G.Fuselier. 24-G.Fuselier to ULL 34 for -1 yard (25-B.Lane Jr.).'
    )
  )


context("Passing Attempts")
test_that("Passing Attempts are correctly parsing", {
  Passes <- ParsePass(TestFrame)
  
  expect_true(all(Passes$PassAtt))
  expect_equal(Passes$PassYards,
                    c(NA, NA, 6, 1, 3, 0, 8, 5, -1))
  expect_equal(Passes$Passer,
                    c("16-N.Strock"
                      ,"5-M.Julian"
                      ,"5-M.Julian"
                      ,"11-L.Woodside"
                      ,"16-N.Strock"
                      ,"10-C.Reardon"
                      ,"11-L.Woodside"
                      ,"11-L.Woodside"
                      ,"8-T.Broadway"
                      ))
  expect_equal(Passes$Receiver,
                  c(NA
                    ,"80-M.Roberts"
                    ,"24-D.Jones-Moore"
                    ,"4-C.Jones"
                    ,"7-C.Pierce"
                    ,"7-C.Pierce"
                    ,"9-A.Russell"
                    ,"9-A.Russell"
                    ,"24-G.Fuselier"
                    ))  
  
})