

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
    ,'7-T.Wilson incomplete. Intended for 87-S.Fakailoatonga. Penalty on MICH 5-J.Peppers, Pass interference, 15 yards, enforced at UTH 49. No Play.'
    ,'7-T.Wilson incomplete. Intended for 87-S.Fakailoatonga. Team penalty on UTH, Illegal formation, 5 yards, enforced at MICH 27. No Play.'
    ,"Penalty on MICH 61-G.Glasgow, False start, 5 yards, enforced at MICH 25. No Play."
    ,"15-J.Rudock scrambles to UTH 41 for 3 yards (7-A.Godfrey). Penalty on UTH 49-H.Dimick, Offside, 5 yards, enforced at UTH 44. No Play."
    ,"15-J.Rudock incomplete. Intended for 9-G.Perry. Penalty on UTH 50-P.Taumoepenu, Offside, 5 yards, enforced at MICH 32. No Play."
    ,"32-T.Isaac to MICH 41 for 5 yards (13-G.Paul). Penalty on UTH 13-G.Paul, Unsportsmanlike conduct, 15 yards, enforced at MICH 41."
    ,"7-T.Wilson to MICH 46 for 8 yards (22-J.Wilson). Penalty on UTH 87-S.Fakailoatonga, Holding, 10 yards, enforced at MICH 46."
    ,"15-J.Rudock incomplete. Intended for 9-G.Perry. Penalty on MICH 71-B.Braden, Ineligible player downfield during passing down, declined."
    ,"2-M.Bercovici incomplete. Team penalty on ASU, Intentional grounding, 7 yards, enforced at ASU 13."
    ,"8-D.Kaser punts 42 yards from ASU 42 Downed at the ASU End Zone. Penalty on ASU 37-C.Gerhart, Roughing the kicker, 15 yards, enforced at ASU 42. No Play."
    ,'38-D.Kaser punts 51 yards from TXAM 16. 1-D.Hayes to ASU 37 for 4 yards. Penalty on TXAM 35-R.Garner, Running into kicker, declined.'
    ,'28-A.Ware to ARKS 21 for 2 yards. Penalty on ARKS 57-J.Swalley, Facemask, Incidental, 10 yards, enforced at ARKS 21.'
    ,'15-L.Ferguson punts 41 yards from ARKS 39. 29-C.Tober to USC 20 for 0 yards. Penalty on ARKS 16-T.Trosin, Fair catch interference, 15 yards, enforced at USC 20.'
    ,'2-J.Stave incomplete. Intended for 86-A.Erickson, INTERCEPTED by 4-E.Jackson at BAMA 34. 4-E.Jackson to WIS 25 for 41 yards. Penalty on BAMA 21-M.Smith, Illegal block in the back, 10 yards, enforced at WIS 25.'
    ,'11-M.Birdsong scrambles to MSH 40 for 6 yards. Team penalty on PUR, Unnecessary roughness, 15 yards, enforced at MSH 40.'
    ,'11-M.Birdsong incomplete. Intended for 21-E.Byrd. Penalty on PUR 4-J.Bentley, Roughing the passer, 15 yards, enforced at MSH 33. No Play.'
    ,'Team penalty on NEB, Illegal use of hands, 15 yards, enforced at BYU 35. No Play.'
    ,"42-M.Williams to VT 28 for 0 yards. Penalty on VT 5-J.Stanford, Clipping, 11 yards, enforced at VT 28. No Play."
    ,"11-M.Johnson incomplete. Intended for 19-T.Redding. Penalty on BGN 17-R.Burbrink, Chop block, 15 yards, enforced at TEN 49. No Play." 
    ,"98-T.Tate kicks 65 yards from BGN 35. 7-C.Sutton to BGN 33 for 67 yards. Penalty on BGN 98-T.Tate, Facemasking, 15 yards, enforced at BGN 33."
    ,"11-J.Dobbs to TEN 47 for 2 yards. Penalty on TEN 11-J.Dobbs, Illegal Forward Pass, 5 yards, enforced at TEN 47."
    ,"Team penalty on USF, 12 players, 5 yards, enforced at FSU 12. No Play."
    ,"Penalty on BOISE 66-M.Yakoo, False start, 5 yards, enforced at BOISE 41. No Play."
    ,"24-P.Davidson extra point is good. Penalty on NMS 24-P.Davidson, Illegal motion, 5 yards, enforced at GST 2. No Play."
    ,"Team penalty on STA, 12 men in the huddle, 5 yards, enforced at STA 12. No Play." 
    ,"7-T.Ervin to SJS 30 for 5 yards. Team penalty on SJS, Illegal shift, 5 yards, enforced at SJS 25. No Play."
    ,"Team penalty on BAY, Offside on Free Kick, 5 yards, enforced at LAM 25. No Play."
    ,"Penalty on NM 91-N.D'Avanzo, Personal Foul, 15 yards, enforced at NM 40. No Play." 
    ,"16-J.Licata incomplete. Intended for 7-M.McGill. Penalty on BUF 2-J.Johnson, Tripping, 15 yards, enforced at BUF 48. No Play."
    ,"Penalty on LT 43-L.McPherson, First onside kickoff out of bounds, 5 yards, enforced at LT 48. No Play."
    ,"Penalty on UCLA 11-J.Neuheisel, Delay of game at start of either half, 5 yards, enforced at UNLV 22. No Play." 
    ,"Team penalty on HOW, Illegal Procedure, 5 yards, enforced at HOW 25. No Play." 
    ,"Penalty on OKS 97-E.Davis, Encroachment, 5 yards, enforced at UCA 8. No Play."
    ,"Penalty on UCONN 20-O.Melifonwu, Taunting, 15 yards, enforced at ARM 29. No Play."
    )
  )


context("Penalties")
test_that("Penalties are correctly parsing", {
  Penalties <- ParsePenalty(TestFrame)
  
  expect_true(all(Penalties$Penalty))
  expect_equal(Penalties$PenaltyYards,
                    c(15, 5, 5, 15, 5, 15, 15, 5, 5, 5, 5, 15, 10, 0, 7, 15, 0, 10, 15, 10, 15, 15, 15, 11, 15, 15, 5, 5, 5, 5, 5, 5, 5, 15, 15, 5, 5, 5, 5, 15)
               )
  expect_true(all(Penalties$PenaltyPlayer == 
                    c("6-D.Payne"
                      ,"32-J.Hill"
                      ,"77-C.Thomas"
                      ,"90-M.Chavis"
                      ,"1-J.Showers"
                      ,"53-J.Whittingham"
                      ,"5-J.Peppers"
                      ,""
                      ,"61-G.Glasgow"
                      ,"49-H.Dimick"
                      ,"50-P.Taumoepenu"
                      ,"13-G.Paul"
                      ,"87-S.Fakailoatonga"
                      ,"71-B.Braden"
                      ,""
                      ,"37-C.Gerhart"
                      ,"35-R.Garner"
                      ,"57-J.Swalley"
                      ,"16-T.Trosin"
                      ,"21-M.Smith"
                      ,""
                      ,"4-J.Bentley"
                      ,""
                      ,"5-J.Stanford"
                      ,"17-R.Burbrink"
                      ,"98-T.Tate"
                      ,"11-J.Dobbs"
                      ,""
                      ,"66-M.Yakoo"
                      ,"24-P.Davidson"
                      ,""
                      ,""
                      ,""
                      ,"91-N.D-Avanzo"
                      ,"2-J.Johnson"
                      ,"43-L.McPherson"
                      ,"11-J.Neuheisel"
                      ,""
                      ,"97-E.Davis"
                      ,"20-O.Melifonwu"
                      )
                  )
  )
  
})