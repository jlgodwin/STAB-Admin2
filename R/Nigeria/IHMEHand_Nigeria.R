#### Admin1 ####
n.gadm <- dim(admin1.names)[1]
n.ihme <- length(unique(ihme.ests$adm1$ADM1_NAME))
n <- max(n.gadm, n.ihme)
adm1.compare <- data.frame(GADM = c(sort(as.character(admin1.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm1$ADM1_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm1.compare[,1]) ==
                          as.character(adm1.compare[,2])))
adm1.compare[mismatch.idx,]
adm1.compare


#### Admin2 ####
ihme.ests$adm2$ADM2_NAME <- as.character(ihme.ests$adm2$ADM2_NAME)
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Bassa" &
                           ihme.ests$adm2$ADM1_NAME == "Kogi"] <- "Bassa, Kogi"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Bassa" &
                           ihme.ests$adm2$ADM1_NAME == "Plateau"] <- "Bassa, Plateau"

ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Nassarawa" &
                 ihme.ests$adm2$ADM1_NAME == "Kano" ] <- "Nassarawa, Kano"

ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Obi" &
                 ihme.ests$adm2$ADM1_NAME == "Benue"] <- "Obi, Benue"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Obi" &
                           ihme.ests$adm2$ADM1_NAME == "Nassarawa"] <- "Obi, Nassarawa"

ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Surulere" &
                 ihme.ests$adm2$ADM1_NAME == "Lagos"] <- "Surulere, Lagos"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Surulere" &
                           ihme.ests$adm2$ADM1_NAME == "Oyo"] <- "Surulere, Oyo"

ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Irepodun" &
                 ihme.ests$adm2$ADM1_NAME == "Kwara"] <- "Irepodun, Kwara"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Irepodun" &
                           ihme.ests$adm2$ADM1_NAME == "Osun"] <- "Irepodun, Osun"

ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Ifelodun" &
                           ihme.ests$adm2$ADM1_NAME == "Kwara"] <- "Ifelodun, Kwara"
ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME == "Ifelodun" &
                           ihme.ests$adm2$ADM1_NAME == "Osun"] <- "Ifelodun, Osun"

n.gadm <- dim(admin2.names)[1]
n.ihme <- length(unique(ihme.ests$adm2$ADM2_NAME))
n <- max(n.gadm, n.ihme)
adm2.compare <- data.frame(GADM = c(sort(as.character(admin2.names$GADM)),
                                    rep(NA, n-n.gadm)),
                           IHME = c(sort(unique(as.character(ihme.ests$adm2$ADM2_NAME))),
                                    rep(NA, n - n.ihme)))
mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]



names.poss <- adm2.compare$GADM
adm2.compare$GADM[7:8] <- names.poss[8:7]
adm2.compare$GADM[15:16] <- names.poss[16:15]
adm2.compare$GADM[28:30] <- names.poss[c(78:79, 28)]
adm2.compare$GADM[31:40] <- names.poss[c(29:34,38,35:37)]
adm2.compare$GADM[41:50] <- names.poss[c(39:48)]
adm2.compare$GADM[51:60] <- names.poss[49:58]
adm2.compare$GADM[61:70] <- names.poss[59:68]
adm2.compare$GADM[71:80] <- names.poss[c(69:77,80)]
adm2.compare$GADM[111:120] <- names.poss[c(113,112,111,114:120)]
adm2.compare$GADM[129:130] <- names.poss[c(132,129)]
adm2.compare$GADM[131:132] <- names.poss[c(130:131)]  ##HERE##
# adm2.compare$GADM[141:150] <- names.poss[144:153]
# adm2.compare$GADM[151:160] <- names.poss[154:163]
# adm2.compare$GADM[161:170] <- names.poss[164:173]
# adm2.compare$GADM[171:180] <- names.poss[174:183]
# adm2.compare$GADM[181:190] <- names.poss[184:193]
# adm2.compare$GADM[191:200] <- names.poss[194:203]
 adm2.compare$GADM[205:206] <- names.poss[206:205]
# adm2.compare$GADM[211:220] <- names.poss[214:223]
# adm2.compare$GADM[221:230] <- names.poss[224:233]
# adm2.compare$GADM[231:240] <- names.poss[234:243]
# adm2.compare$GADM[241:250] <- names.poss[244:253]
# adm2.compare$GADM[251:260] <- names.poss[254:263]
# adm2.compare$GADM[261:270] <- names.poss[264:273]
# adm2.compare$GADM[271:280] <- names.poss[274:283]
# adm2.compare$GADM[281:290] <- names.poss[284:293]

adm2.compare$GADM[291:300] <- names.poss[c(291,293,292,294:298,302,299)]
adm2.compare$GADM[301:302] <- names.poss[300:301]
adm2.compare$GADM[316:318] <- names.poss[c(317,318,316)]
adm2.compare$GADM[321:330] <- names.poss[c(321:323,325,324,326:330)]
adm2.compare$GADM[342:343] <- names.poss[343:342]
# adm2.compare$GADM[351:360] <- names.poss[355:364]
adm2.compare$GADM[365:367] <- names.poss[c(367,365,366)]
# adm2.compare$GADM[371:380] <- names.poss[375:384]
# adm2.compare$GADM[381:390] <- names.poss[385:394]
# adm2.compare$GADM[391:400] <- names.poss[395:404]
# adm2.compare$GADM[401:410] <- names.poss[405:414]
# 
# 
# adm2.compare$GADM[361:370] <- names.poss[c(361:364,367,365,366,368:370)]
adm2.compare$GADM[421:422] <- names.poss[422:421]


adm2.compare$GADM[461:470] <- c(as.character(names.poss[461:462]), "Mainland", NA,
                                as.character(names.poss[463:468]))
adm2.compare$GADM[471:480] <- names.poss[469:478]
adm2.compare$GADM[481:490] <- names.poss[c(479:481,483:485,487:490)]
adm2.compare$GADM[491:500] <- c(as.character(names.poss[c(491:492,486,493:499)]))

adm2.compare$GADM[501:510] <- names.poss[500:509]
adm2.compare$GADM[511:520] <- names.poss[510:519]
adm2.compare$GADM[521:530] <- names.poss[c(521,520,522:529)]
adm2.compare$GADM[531:540] <- names.poss[c(530:539)]
adm2.compare$GADM[541:550] <- names.poss[540:549]
adm2.compare$GADM[551:560] <- names.poss[c(550:559)]
adm2.compare$GADM[561:570] <- names.poss[560:569]
adm2.compare$GADM[571:580] <- names.poss[570:579]
adm2.compare$GADM[581:590] <- names.poss[580:589]
adm2.compare$GADM[591:600] <- names.poss[590:599]
adm2.compare$GADM[601:610] <- names.poss[600:609]
adm2.compare$GADM[611:620] <- names.poss[610:619]
adm2.compare$GADM[621:630] <- names.poss[620:629]
adm2.compare$GADM[631:640] <- names.poss[630:639]
adm2.compare$GADM[641:650] <- names.poss[640:649]
adm2.compare$GADM[651:660] <- names.poss[650:659]
adm2.compare$GADM[661:670] <- names.poss[660:669]
adm2.compare$GADM[671:680] <- names.poss[670:679]
adm2.compare$GADM[681:690] <- names.poss[680:689]
adm2.compare$GADM[691:700] <- names.poss[690:699]
adm2.compare$GADM[701:710] <- names.poss[700:709]
adm2.compare$GADM[711:720] <- names.poss[710:719]
adm2.compare$GADM[721:730] <- names.poss[720:729]
adm2.compare$GADM[731:740] <- names.poss[730:739]
adm2.compare$GADM[741:750] <- names.poss[740:749]
adm2.compare$GADM[751:760] <- names.poss[750:759]
adm2.compare$GADM[761:770] <- names.poss[760:769]
adm2.compare$GADM[771:774] <- names.poss[770:773]

if(FALSE){
  pdf('Nigeria/Plots/ShapeCheck/Bassa_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Bassa",], border = 'red',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = 'red', legend = 'Bassa')
  dev.off()
  
  pdf('Nigeria/Plots/ShapeCheck/Surulere_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Surulere",], border = 'red',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = 'red', legend = 'Surulere')
  dev.off()
  
  pdf('Nigeria/Plots/ShapeCheck/Ifelodun_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Ifelodun",], border = 'red',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = 'red', legend = 'Ifelodun')
  dev.off()
  
  pdf('Nigeria/Plots/ShapeCheck/Irepodun_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Irepodun",], border = 'red',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = 'red', legend = 'Irepodun')
  dev.off()
  pdf('Nigeria/Plots/ShapeCheck/Obi_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Obi",], border = 'red',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = 'red', legend = 'Obi')
  dev.off()
  
  pdf('Nigeria/Plots/ShapeCheck/Lagos_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "LagosIsland",], border = 'red',
       add = T)
  plot(poly.adm2[poly.adm2$NAME_2 == "Mainland",], border = 'blue',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = c('red', 'blue'), legend = c('LagosIsland', 'Mainland'))
  dev.off()
  
  pdf('Nigeria/Plots/ShapeCheck/Nasarawa_test.pdf')
  plot(poly.adm0)
  plot(poly.adm2[poly.adm2$NAME_2 == "Nasarawa",], border = 'red',
       add = T)
  plot(poly.adm2[poly.adm2$NAME_2 == "Nassaraw",], border = 'blue',
       add = T)
  plot(poly.adm2[poly.adm2$NAME_2 == "Nassarawa Egon",], border = 'green',
       add = T)
  legend('topleft', bty = 'n', lty = 1,
         col = c('red', 'blue', 'green'), legend = c('Nasarawa', 'Nassaraw',
                                                     'Nassarawa Egon'))
  dev.off()
}

mismatch.idx <- which(!(as.character(adm2.compare[,1]) ==
                          as.character(adm2.compare[,2])))
adm2.compare[mismatch.idx,]

for(idx in mismatch.idx){
  ihme.ests$adm2$ADM2_NAME[ihme.ests$adm2$ADM2_NAME ==
                             as.character(adm2.compare$IHME[idx])] <- as.character(adm2.compare$GADM[idx])
}
sum(!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME))
admin2.names$GADM[!(admin2.names$GADM %in% ihme.ests$adm2$ADM2_NAME)]
