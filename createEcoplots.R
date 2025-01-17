# useful function to create plots or UI
fyplot1<-function(tspmain,axtitle,tspvar1){
  #par(mar=c(3,3.8,2.5,0.4))
  tsyears<-length(tspvar1)
  tseq<-seq(0,360,by=1)
  plmax<-1.5*max(max(tspvar1))
  plmin<-0
  plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE,main=tspmain)
  axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
  axis(side=2,las=1,cex.axis=0.9)
  mtext("Days",cex=1.5,side=1,line=2)
  mtext(axtitle,cex=1.5,side=2,line=2.8)
  mtext(tspmain,cex=1.5,side=3,line=0.5)
}

fyplot2<-function(tspmain,axtitle,tsptitle1,tsptitle2,tspvar1,tspvar2){
  #par(mar=c(3,3.8,2.5,0.4))
  tsyears<-length(tspvar1)
  tseq<-seq(0,360,by=1)
  plmax<-max(max(tspvar1),max(tspvar2))
  plmin<-0
  plmax<-plmax+0.55*(plmax-plmin)
  plot(tseq,tspvar1,type="l",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  lines(tseq,tspvar2,type="l",lty="dashed")
  axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
  axis(side=2,las=1,cex.axis=0.9)
  mtext("Days",cex=1.5,side=1,line=2)
  mtext(axtitle,cex=1.5,side=2,line=2.8)
  mtext(tspmain,cex=1.5,side=3,line=0.5)
  legend("topleft",c(tsptitle1,tsptitle2),box.lty=0,bg="transparent",col=c("black","black"),lty=c(1,2),cex=c(0.9,0.9),pt.cex=c(1,1))
}



fyplot3<-function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3){
  #par(mar=c(3,3.8,2.5,0.4))
  tsyears<-length(tspvar1)
  tseq<-seq(0,360,by=1)
  plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
  plmin<-0
  plmax<-plmax+1.15*(plmax-plmin)
  plot(tseq,tspvar1,type="l",col="black",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  lines(tseq,tspvar2,type="l",col="black",lty="dashed")
  lines(tseq,tspvar3,type="l",col="red",lty="dashed")
  axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
  axis(side=2,las=1,cex.axis=0.9)
  mtext("Days",cex=1.5,side=1,line=2)
  mtext(axtitle,cex=1.5,side=2,line=2.8)
  mtext(tspmain,cex=1.5,side=3,line=0.5)
  legend("topleft",c(tsptitle1,tsptitle2,tsptitle3),box.lty=0,bg="transparent",col=c("black","black","red"),lty=c(1,2,2),cex=c(0.9,0.9,0.9),pt.cex=c(1,1,1))
}

fyplot3_hab <- function(tspmain,axtitle,tsptitle1,tsptitle2,tsptitle3,tspvar1,tspvar2,tspvar3) {
  #par(mar=c(3,3.8,1.3,0.4))
  tsyears<-length(tspvar1)
  tseq<-seq(0,360,by=1)
  plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
  plmin<-0
  plmax<-plmax+(1.4*(plmax-plmin))
  plot(tseq,tspvar1,type="l",col="red",yaxt="n",xlim=c(0,360),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  lines(tseq,tspvar2,type="l",col="red",lty="dashed")
  lines(tseq,tspvar3,type="l",col="black",lty="dashed")
  axis(side=1,at=c(0,60,120,180,240,300,360),las=1,cex.axis=0.9)
  axis(side=2,las=1,cex.axis=0.9)
  mtext("Days",cex=1.5,side=1,line=2)
  mtext(axtitle,cex=1.5,side=2,line=2.8)
  mtext(tspmain,cex=1.5,side=3,line=0.2)
  legend("topleft",c(tsptitle1,tsptitle2,tsptitle3),box.lty=0,bg="transparent",col=c("red","red","black"),lty=c(1,2,2),pt.cex=c(1,1,1),cex=c(0.9,0.9,0.9))
}



createEcoplots	<- function( model, results, selection, subSelection ) {
  elt <- StrathE2E2:::elt
  #fyplot3 <-StrathE2E2:::fyplot3
  #fyplot3_hab <-StrathE2E2:::fyplot3_hab
  #fyplot2 <-StrathE2E2:::fyplot2
  #fyplot1 <-StrathE2E2:::fyplot1
  start_par = par()$mfrow
  on.exit(par(mfrow = start_par))
  build <- elt(results, "build")
  run <- elt(build, "run")
  nyears <- elt(run, "nyears")
  ndays <- elt(run, "ndays")
  data <- elt(model, "data")
  physical.parameters <- elt(data, "physical.parameters")
  si_depth <- elt(physical.parameters, "si_depth")
  so_depth <- elt(physical.parameters, "so_depth")
  d_depth <- elt(physical.parameters, "d_depth")
  x_shallowprop <- elt(physical.parameters, "x_shallowprop")
  x_poros_s1 <- elt(physical.parameters, "x_poros_s1")
  x_poros_s2 <- elt(physical.parameters, "x_poros_s2")
  x_poros_s3 <- elt(physical.parameters, "x_poros_s3")
  x_area_s1 <- elt(physical.parameters, "x_area_s1")
  x_area_s2 <- elt(physical.parameters, "x_area_s2")
  x_area_s3 <- elt(physical.parameters, "x_area_s3")
  x_depth_s1 <- elt(physical.parameters, "x_depth_s1")
  x_depth_s2 <- elt(physical.parameters, "x_depth_s2")
  x_depth_s3 <- elt(physical.parameters, "x_depth_s3")
  x_poros_d1 <- elt(physical.parameters, "x_poros_d1")
  x_poros_d2 <- elt(physical.parameters, "x_poros_d2")
  x_poros_d3 <- elt(physical.parameters, "x_poros_d3")
  x_area_d1 <- elt(physical.parameters, "x_area_d1")
  x_area_d2 <- elt(physical.parameters, "x_area_d2")
  x_area_d3 <- elt(physical.parameters, "x_area_d3")
  x_depth_d1 <- elt(physical.parameters, "x_depth_d1")
  x_depth_d2 <- elt(physical.parameters, "x_depth_d2")
  x_depth_d3 <- elt(physical.parameters, "x_depth_d3")
  output <- elt(results, "output")
  corpse_d1 <- elt(output, "corpse_d1")
  corpse_d2 <- elt(output, "corpse_d2")
  corpse_d3 <- elt(output, "corpse_d3")
  corpse_s1 <- elt(output, "corpse_s1")
  corpse_s2 <- elt(output, "corpse_s2")
  corpse_s3 <- elt(output, "corpse_s3")
  kelpdebris <- elt(output, "kelpdebris")
  detritus_so <- elt(output, "detritus_so")
  detritus_si <- elt(output, "detritus_si")
  detritus_d <- elt(output, "detritus_d")
  nitrate_so <- elt(output, "nitrate_so")
  nitrate_si <- elt(output, "nitrate_si")
  nitrate_d <- elt(output, "nitrate_d")
  ammonia_so <- elt(output, "ammonia_so")
  ammonia_si <- elt(output, "ammonia_si")
  ammonia_d <- elt(output, "ammonia_d")
  phyt_so <- elt(output, "phyt_so")
  phyt_si <- elt(output, "phyt_si")
  phyt_d <- elt(output, "phyt_d")
  omni_o <- elt(output, "omni_o")
  omni_i <- elt(output, "omni_i")
  carn_o <- elt(output, "carn_o")
  carn_i <- elt(output, "carn_i")
  benths_o <- elt(output, "benths_o")
  benths_i <- elt(output, "benths_i")
  kelpN <- elt(output, "kelpN")
  benthslar_o <- elt(output, "benthslar_o")
  benthslar_i <- elt(output, "benthslar_i")
  benthc_o <- elt(output, "benthc_o")
  benthc_i <- elt(output, "benthc_i")
  benthclar_o <- elt(output, "benthclar_o")
  benthclar_i <- elt(output, "benthclar_i")
  fishp_o <- elt(output, "fishp_o")
  fishp_i <- elt(output, "fishp_i")
  fishplar_o <- elt(output, "fishplar_o")
  fishplar_i <- elt(output, "fishplar_i")
  fishd_o <- elt(output, "fishd_o")
  fishd_i <- elt(output, "fishd_i")
  fishdlar_o <- elt(output, "fishdlar_o")
  fishdlar_i <- elt(output, "fishdlar_i")
  fishm_o <- elt(output, "fishm_o")
  fishm_i <- elt(output, "fishm_i")
  bird_o <- elt(output, "bird_o")
  bird_i <- elt(output, "bird_i")
  ceta_o <- elt(output, "ceta_o")
  ceta_i <- elt(output, "ceta_i")
  seal_o <- elt(output, "seal_o")
  seal_i <- elt(output, "seal_i")
  discard_o <- elt(output, "discard_o")
  discard_i <- elt(output, "discard_i")
  x_ammonia_s1 <- elt(output, "x_ammonia_s1")
  x_ammonia_s2 <- elt(output, "x_ammonia_s2")
  x_ammonia_s3 <- elt(output, "x_ammonia_s3")
  x_ammonia_d1 <- elt(output, "x_ammonia_d1")
  x_ammonia_d2 <- elt(output, "x_ammonia_d2")
  x_ammonia_d3 <- elt(output, "x_ammonia_d3")
  x_nitrate_s1 <- elt(output, "x_nitrate_s1")
  x_nitrate_s2 <- elt(output, "x_nitrate_s2")
  x_nitrate_s3 <- elt(output, "x_nitrate_s3")
  x_nitrate_d1 <- elt(output, "x_nitrate_d1")
  x_nitrate_d2 <- elt(output, "x_nitrate_d2")
  x_nitrate_d3 <- elt(output, "x_nitrate_d3")
  x_detritus_s1 <- elt(output, "x_detritus_s1")
  xR_detritus_s1 <- elt(output, "xR_detritus_s1")
  x_detritus_s2 <- elt(output, "x_detritus_s2")
  xR_detritus_s2 <- elt(output, "xR_detritus_s2")
  x_detritus_s3 <- elt(output, "x_detritus_s3")
  xR_detritus_s3 <- elt(output, "xR_detritus_s3")
  x_detritus_d1 <- elt(output, "x_detritus_d1")
  xR_detritus_d1 <- elt(output, "xR_detritus_d1")
  x_detritus_d2 <- elt(output, "x_detritus_d2")
  xR_detritus_d2 <- elt(output, "xR_detritus_d2")
  x_detritus_d3 <- elt(output, "x_detritus_d3")
  xR_detritus_d3 <- elt(output, "xR_detritus_d3")
  aggregates <- elt(results, "aggregates")
  x_poros <- elt(aggregates, "x_poros")
  x_depth <- elt(aggregates, "x_depth")
  xvolume_si <- si_depth * x_shallowprop
  xvolume_so <- so_depth * (1 - x_shallowprop)
  xd_volume <- d_depth * (1 - x_shallowprop)
  if (selection == "NUT_PHYT") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Detritus") {
    l1 <- detritus_so[((nyears - 1) * 360 + 1):ndays]/xvolume_so
    l2 <- detritus_si[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    l3 <- detritus_d[((nyears - 1) * 360 + 1):ndays]/xd_volume
    fyplot3("Suspended bact. & detritus", "", "S-offshore", 
            "S-inshore", "Deep", l1, l2, l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2,line = 2.5)
    #mtext("Suspended bact. & detritus",cex=1.5,side=3,line=0.5)
    }
    if (subSelection == "Ammonia") {
    l1 <- ammonia_so[((nyears - 1) * 360 + 1):ndays]/xvolume_so
    l2 <- ammonia_si[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    l3 <- ammonia_d[((nyears - 1) * 360 + 1):ndays]/xd_volume
    fyplot3("Ammonia", "", "S-offshore", "S-inshore", "Deep", 
            l1, l2, l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Nitrate") {
    l1 <- nitrate_so[((nyears - 1) * 360 + 1):ndays]/xvolume_so
    l2 <- nitrate_si[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    l3 <- nitrate_d[((nyears - 1) * 360 + 1):ndays]/xd_volume
    fyplot3("Nitrate", "", "S-offshore", "S-inshore", "Deep", 
            l1, l2, l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Phytoplankton") {
    l1 <- phyt_so[((nyears - 1) * 360 + 1):ndays]/xvolume_so
    l2 <- phyt_si[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    l3 <- phyt_d[((nyears - 1) * 360 + 1):ndays]/xd_volume
    fyplot3("Phytoplankton", "", "S-offshore", "S-inshore", 
            "Deep", l1, l2, l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "SEDIMENT") {
    #par(mfrow = c(4, 2))
    if (subSelection == "Inshore ammonia") {
    if (x_poros_s1 > 0 && x_area_s1 > 0) {
      l1 <- x_ammonia_s1[((nyears - 1) * 360 + 1):ndays]/(x_area_s1 * 
                                                            x_depth_s1 * x_poros_s1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_poros_s2 > 0 && x_area_s2 > 0) {
      l2 <- x_ammonia_s2[((nyears - 1) * 360 + 1):ndays]/(x_area_s2 * 
                                                            x_depth_s2 * x_poros_s2)
    }
    else {
      l2 <- rep(NA, 361)
    }
    if (x_poros_s3 > 0 && x_area_s3 > 0) {
      l3 <- x_ammonia_s3[((nyears - 1) * 360 + 1):ndays]/(x_area_s3 * 
                                                            x_depth_s3 * x_poros_s3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Inshore ammonia", "", "Area_s1 porewater", 
                "Area_s2 porewater", "Area_s3 porewater", l1, l2, 
                l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Offshore ammonia") {
    if (x_poros_d1 > 0 && x_area_d1 > 0) {
      l1 <- x_ammonia_d1[((nyears - 1) * 360 + 1):ndays]/(x_area_d1 * 
                                                            x_depth_d1 * x_poros_d1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_poros_d2 > 0 && x_area_d2 > 0) {
      l2 <- x_ammonia_d2[((nyears - 1) * 360 + 1):ndays]/(x_area_d2 * 
                                                            x_depth_d2 * x_poros_d2)
    }
    else {
      l2 <- rep(NA, 361)
    }
    if (x_poros_d3 > 0 && x_area_d3 > 0) {
      l3 <- x_ammonia_d3[((nyears - 1) * 360 + 1):ndays]/(x_area_d3 * 
                                                            x_depth_d3 * x_poros_d3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Offshore ammonia", "", "Area_d1 porewater", 
                "Area_d2 porewater", "Area_d3 porewater", l1, l2, 
                l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Inshore nitrate") {
    if (x_poros_s1 > 0 && x_area_s1 > 0) {
      l1 <- x_nitrate_s1[((nyears - 1) * 360 + 1):ndays]/(x_area_s1 * 
                                                            x_depth_s1 * x_poros_s1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_poros_s2 > 0 && x_area_s2 > 0) {
      l2 <- x_nitrate_s2[((nyears - 1) * 360 + 1):ndays]/(x_area_s2 * 
                                                            x_depth_s2 * x_poros_s2)
    }
    else {
      l3 <- rep(NA, 361)
    }
    if (x_poros_s3 > 0 && x_area_s3 > 0) {
      l3 <- x_nitrate_s3[((nyears - 1) * 360 + 1):ndays]/(x_area_s3 * 
                                                            x_depth_s3 * x_poros_s3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Inshore nitrate", "", "Area_s1 porewater", 
                "Area_s2 porewater", "Area_s3 porewater", l1, l2, 
                l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Offshore nitrate") {
    if (x_poros_d1 > 0 && x_area_d1 > 0) {
      l1 <- x_nitrate_d1[((nyears - 1) * 360 + 1):ndays]/(x_area_d1 * 
                                                            x_depth_d1 * x_poros_d1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_poros_d2 > 0 && x_area_d2 > 0) {
      l2 <- x_nitrate_d2[((nyears - 1) * 360 + 1):ndays]/(x_area_d2 * 
                                                            x_depth_d2 * x_poros_d2)
    }
    else {
      l2 <- rep(NA, 361)
    }
    if (x_poros_d3 > 0 && x_area_d3 > 0) {
      l3 <- x_nitrate_d3[((nyears - 1) * 360 + 1):ndays]/(x_area_d3 * 
                                                            x_depth_d3 * x_poros_d3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Offshore nitrate", "", "Area_d1 porewater", 
                "Area_d2 porewater", "Area_d3 porewater", l1, l2, 
                l3)
    mtext(bquote("Concentration mMN.m"^-3), cex = 1.5, side = 2, 
          line = 2.5)
    }
   # }
    if (subSelection == "Inshore detritus") {
    if (x_poros_s1 > 0 && x_area_s1 > 0) {
      l1a <- 100 * (((x_detritus_s1[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_s1 * x_depth_s1 * 
                                                                 (((1 - x_poros_s1) * (2650 * 1000))))
      l1b <- 100 * (((xR_detritus_s1[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_s1 * x_depth_s1 * 
                                                                  (((1 - x_poros_s1) * (2650 * 1000))))
    }
    else {
      l1a <- rep(NA, 361)
      l1b <- rep(NA, 361)
    }
    if (x_poros_s2 > 0 && x_area_s2 > 0) {
      l2a <- 100 * (((x_detritus_s2[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_s2 * x_depth_s2 * 
                                                                 (((1 - x_poros_s2) * (2650 * 1000))))
      l2b <- 100 * (((xR_detritus_s2[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_s2 * x_depth_s2 * 
                                                                  (((1 - x_poros_s2) * (2650 * 1000))))
    }
    else {
      l2a <- rep(NA, 361)
      l2b <- rep(NA, 361)
    }
    if (x_poros_s3 > 0 && x_area_s3 > 0) {
      l3a <- 100 * (((x_detritus_s3[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_s3 * x_depth_s3 * 
                                                                 (((1 - x_poros_s3) * (2650 * 1000))))
      l3b <- 100 * (((xR_detritus_s3[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_s3 * x_depth_s3 * 
                                                                  (((1 - x_poros_s3) * (2650 * 1000))))
    }
    else {
      l3a <- rep(NA, 361)
      l3b <- rep(NA, 361)
    }
    l1 <- l1a + l1b
    l2 <- l2a + l2b
    l3 <- l3a + l3b
    fyplot3_hab("Inshore detritus", "", "Area_s1 sediment", 
                "Area_s2 sediment", "Area_s3 sediment", l1, l2, 
                l3)
    mtext(bquote("Nitrogen.DW"^-1 ~ " g.g"^-1), cex = 1.5, 
          side = 2, line = 2.5)
    }
    if (subSelection == "Offshore detritus") {
    if (x_poros_d1 > 0 && x_area_d1 > 0) {
      l1a <- 100 * (((x_detritus_d1[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_d1 * x_depth_d1 * 
                                                                 (((1 - x_poros_d1) * (2650 * 1000))))
      l1b <- 100 * (((xR_detritus_d1[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_d1 * x_depth_d1 * 
                                                                  (((1 - x_poros_d1) * (2650 * 1000))))
    }
    else {
      l1a <- rep(NA, 361)
      l1b <- rep(NA, 361)
    }
    if (x_poros_d2 > 0 && x_area_d2 > 0) {
      l2a <- 100 * (((x_detritus_d2[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_d2 * x_depth_d2 * 
                                                                 (((1 - x_poros_d2) * (2650 * 1000))))
      l2b <- 100 * (((xR_detritus_d2[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_d2 * x_depth_d2 * 
                                                                  (((1 - x_poros_d2) * (2650 * 1000))))
    }
    else {
      l2a <- rep(NA, 361)
      l2b <- rep(NA, 361)
    }
    if (x_poros_d3 > 0 && x_area_d3 > 0) {
      l3a <- 100 * (((x_detritus_d3[((nyears - 1) * 360 + 
                                       1):ndays]) * 14)/1000)/(x_area_d3 * x_depth_d3 * 
                                                                 (((1 - x_poros_d3) * (2650 * 1000))))
      l3b <- 100 * (((xR_detritus_d3[((nyears - 1) * 360 + 
                                        1):ndays]) * 14)/1000)/(x_area_d3 * x_depth_d3 * 
                                                                  (((1 - x_poros_d3) * (2650 * 1000))))
    }
    else {
      l3a <- rep(NA, 361)
      l3b <- rep(NA, 361)
    }
    l1 <- l1a + l1b
    l2 <- l2a + l2b
    l3 <- l3a + l3b
    fyplot3_hab("Offshore detritus", "", "Area_d1 sediment", 
                "Area_d2 sediment", "Area_d3 sediment", l1, l2, 
                l3)
    mtext(bquote("Nitrogen.DW"^-1 ~ " g.g"^-1), cex = 1.5, 
          side = 2, line = 2.5)
    }
    if (subSelection == "Inshore corpses") {
    if (x_area_s1 > 0) {
      l1 <- corpse_s1[((nyears - 1) * 360 + 1):ndays]/(x_area_s1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_area_s2 > 0) {
      l2 <- corpse_s2[((nyears - 1) * 360 + 1):ndays]/(x_area_s2)
    }
    else {
      l2 <- rep(NA, 361)
    }
    if (x_area_s3 > 0) {
      l3 <- corpse_s3[((nyears - 1) * 360 + 1):ndays]/(x_area_s3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Inshore corpses", "", "Area_s1", "Area_s2", 
                "Area_s3", l1, l2, l3)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Offshore corpses") {
    if (x_area_d1 > 0) {
      l1 <- corpse_d1[((nyears - 1) * 360 + 1):ndays]/(x_area_d1)
    }
    else {
      l1 <- rep(NA, 361)
    }
    if (x_area_d2 > 0) {
      l2 <- corpse_d2[((nyears - 1) * 360 + 1):ndays]/(x_area_d2)
    }
    else {
      l2 <- rep(NA, 361)
    }
    if (x_area_d3 > 0) {
      l3 <- corpse_d3[((nyears - 1) * 360 + 1):ndays]/(x_area_d3)
    }
    else {
      l3 <- rep(NA, 361)
    }
    fyplot3_hab("Offshore corpses", "", "Area_d1", "Area_d2", 
                "Area_d3", l1, l2, l3)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "ZOOPLANKTON") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Omnivorous zooplankton") {
    l1 <- omni_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- omni_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Omnivorous zooplankton", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Carnivorous zooplankton") {
    l1 <- carn_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- carn_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Carnivorous zooplankton", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "FISH") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Planktivorous fish") {
    l1 <- fishp_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- fishp_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Planktivorous fish", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Planktivorous fish larvae") {
    l1 <- fishplar_o[((nyears - 1) * 360 + 1):ndays]/(1 - 
                                                        x_shallowprop)
    l2 <- fishplar_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Planktivorous fish larvae", "", "Offshore", 
            "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Demersal fish") {
    l1 <- fishd_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- fishd_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Demersal fish", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Demersal fish larvae") {
    l1 <- fishdlar_o[((nyears - 1) * 360 + 1):ndays]/(1 - 
                                                        x_shallowprop)
    l2 <- fishdlar_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Demersal fish larvae", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "BENTHOS") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Benthos susp/dep feeders") {
    l1 <- benths_o[((nyears - 1) * 360 + 1):ndays]/(1 - 
                                                      x_shallowprop)
    l2 <- benths_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Benthos susp/dep feeders", "", "Offshore", 
            "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Benthos susp/dep feeders larvae") {
    l1 <- benthslar_o[((nyears - 1) * 360 + 1):ndays]/(xvolume_so + 
                                                         xd_volume)
    l2 <- benthslar_i[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    fyplot2("Benthos susp/dep feeders larvae", "", "Offshore", 
            "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Benthos carn/scav feeders") {
    l1 <- benthc_o[((nyears - 1) * 360 + 1):ndays]/(1 - 
                                                      x_shallowprop)
    l2 <- benthc_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Benthos carn/scav feeders", "", "Offshore", 
            "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Benthos carn/scav feeders larvae") {
    l1 <- benthclar_o[((nyears - 1) * 360 + 1):ndays]/(xvolume_so + 
                                                         xd_volume)
    l2 <- benthclar_i[((nyears - 1) * 360 + 1):ndays]/xvolume_si
    fyplot2("Benthos carn/scav feeders larvae", "", "Offshore", 
            "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "PREDATORS") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Birds") {
    l1 <- bird_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- bird_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Birds", "", "Offshore", "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Pinnipeds") {
    l1 <- seal_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- seal_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Pinnipeds", "", "Offshore", "Inshore", l1, 
            l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Cetaceans") {
    l1 <- ceta_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- ceta_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Cetaceans", "", "Offshore", "Inshore", l1, 
            l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Migratory fish") {
    l1 <- fishm_o[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    l2 <- fishm_i[((nyears - 1) * 360 + 1):ndays]/x_shallowprop
    fyplot2("Migratory fish", "", "Offshore", "Inshore", 
            l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "CORP_DISC") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Corpses") {
    l1 <- (corpse_d1[((nyears - 1) * 360 + 1):ndays] + corpse_d2[((nyears - 
                                                                     1) * 360 + 1):ndays] + corpse_d3[((nyears - 1) * 
                                                                                                         360 + 1):ndays])/(1 - x_shallowprop)
    l2 <- (corpse_s1[((nyears - 1) * 360 + 1):ndays] + corpse_s2[((nyears - 
                                                                     1) * 360 + 1):ndays] + corpse_s3[((nyears - 1) * 
                                                                                                         360 + 1):ndays])/x_shallowprop
    fyplot2("Corpses", "", "Offshore", "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Discards") {
    l1 <- (discard_o[((nyears - 1) * 360 + 1):ndays])/(1 - 
                                                         x_shallowprop)
    l2 <- (discard_i[((nyears - 1) * 360 + 1):ndays])/x_shallowprop
    fyplot2("Discards", "", "Offshore", "Inshore", l1, l2)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else if (selection == "MACROPHYTE") {
    #par(mfrow = c(2, 2))
    if (subSelection == "Inshore macrophytes") {
    l1 <- kelpN[((nyears - 1) * 360 + 1):ndays]/(1 - x_shallowprop)
    fyplot1("Inshore macrophytes", "", l1)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
    if (subSelection == "Inshore macrophyte debris") {
    l1 <- (kelpdebris[((nyears - 1) * 360 + 1):ndays])/x_shallowprop
    fyplot1("Inshore macrophyte debris", "", l1)
    mtext(bquote("Area density mMN.m"^-2), cex = 1.5, side = 2, 
          line = 2.5)
    }
  }
  else {
    stop("Error: unknown selection '", selection, "' !\n")
  }
	}