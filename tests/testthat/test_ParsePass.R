

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
    ,'10-K.Allen complete to 9-R.Seals-Jones. 9-R.Seals-Jones to TXAM 36 for 4 yards.'
    ,'1-K.Murray incomplete. Intended for 9-R.Seals-Jones.'
    ,'12-C.Jones complete to 4-C.Samuel. 4-C.Samuel runs 24 yards for a touchdown. Team penalty on VT, Pass interference, declined.'
    ,'16-C.Keeton complete to 81-Z.Van Leeuwen. 81-Z.Van Leeuwen to UTS 48 for 2 yards (16-C.Butler-Byrd).'
    ,"18-C.Cook complete to 12-R.Shelton. 12-R.Shelton to MSU 25 for 0 yards (8-R.Daniels)."
    ,"7-T.Green complete to -A.Michael. -A.Michael to TXST 37 for 5 yards."
    ,"8-T.Figaro complete to 23-T.Lucas Jr. 23-T.Lucas Jr. to NIC 15 for 5 yards."
    )
  )


context("Passing Attempts")
test_that("Passing Attempts are correctly parsing", {
  Passes <- ParsePass(TestFrame)
  
  expect_true(all(Passes$PassAtt))
  expect_equal(Passes$PassYards,
                    c(NA, NA, 6, 1, 3, 0, 8, 5, -1, 4, NA, 24, 2, 0, 5, 5))
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
                      ,"10-K.Allen"
                      ,"1-K.Murray"
                      ,"12-C.Jones"
                      ,"16-C.Keeton"
                      ,"18-C.Cook"
                      ,"7-T.Green"
                      ,"8-T.Figaro"
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
                    ,"9-R.Seals-Jones"
                    ,"9-R.Seals-Jones"
                    ,"4-C.Samuel"
                    ,"81-Z.Van-Leeuwen"
                    ,"12-R.Shelton"
                    ,"-A.Michael"
                    ,"23-T.Lucas-Jr"
                    ))  
  
})