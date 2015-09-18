

TestFrame <- data.frame(
  scoreText = c(
    '12-A.Appleby incomplete. INTERCEPTED by 5-K.Baxter at MSH 15. 5-K.Baxter to MSH 40 for 25 yards.'
    ,'12-A.Appleby incomplete. INTERCEPTED by 21-T.Lang at MSH 45. 21-T.Lang runs 55 yards for a touchdown.'
    ,'4-T.Armstrong incomplete. INTERCEPTED by 7-M.Hannemann at BYU 28. 7-M.Hannemann runs ob at BYU 28 for 0 yards.'
    ,'12-C.Jones incomplete. Intended for 81-N.Vannett, INTERCEPTED by 26-D.Frye at VT 44. 26-D.Frye to VT 44 for 0 yards.'
    ,'12-M.Brewer incomplete. INTERCEPTED by 11-V.Bell at VT 49. 11-V.Bell to VT 49 for 0 yards.'
    ,"7-E.Jenkins incomplete. Intended for 15-R.Gonzalez, INTERCEPTED by 23-J.Ford at AUB 35. 23-J.Ford, touchback."
    )
  )


context("Interceptions")
test_that("Interceptions are correctly parsing", {
  Int <- ParseInterception(TestFrame)
  
  expect_true(all(Int$Interception))
  expect_equal(Int$InterceptionYards
               , c(25,55,0,0,0,0))
  expect_equal(Int$Interceptor,
                    c("5-K.Baxter"
                      ,"21-T.Lang"
                      ,"7-M.Hannemann"
                      ,"26-D.Frye"
                      ,"11-V.Bell"
                      ,"23-J.Ford"
                      ))
  
})