# useful function to create plots or UI
tsmonthplot1 <- function(tsptitle, tspvar1) {
  #par(mar=c(3,4.1,0.5,0.4))
  tseq<-seq(0.5,11.5,by=1)
  plmax<-max((tspvar1))
  #	plmin<-min((tspvar1))
  plmin<-0
  plmax<-plmax+(0.1*(plmax-plmin))
  plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  points(tseq,tspvar1,pch=16)
  axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
  axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
  mtext("Months",cex=1.2,side=1,line=1.5)
  mtext(tsptitle,cex=1.2,side=2,line=3.1)
}

tsmonthplot2 <- function(tsptitle,tspvar1,tspvar2) {
 # par(mar=c(3,4.1,0.5,0.4))
  tseq<-seq(0.5,11.5,by=1)
  plmax<-max(max(tspvar1),max(tspvar2))
  #	plmin<-min(min(tspvar1),min(tspvar2))
  plmin<-0
  plmax<-plmax+(0.5*(plmax-plmin))
  plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  points(tseq,tspvar1,pch=16)
  lines(tseq,tspvar2,lty="dashed")
  points(tseq,tspvar2,pch=1)
  axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
  axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
  mtext("Months",cex=1.2,side=1,line=1.5)
  mtext(tsptitle,cex=1.2,side=2,line=3.1)
}

tsmonthplot3 <- function(tsptitle,tspvar1,tspvar2,tspvar3) {
#  par(mar=c(3,4.1,0.5,0.4))
  tseq<-seq(0.5,11.5,by=1)
  plmax<-max(max(tspvar1),max(tspvar2),max(tspvar3))
  #	plmin<-min(min(tspvar1),min(tspvar2),min(tspvar3))
  plmin<-0
  plmax<-plmax+(0.8*(plmax-plmin))
  plot(tseq,tspvar1,type="l",lty=1,yaxt="n",xlim=c(0,12),ylim=c(plmin,plmax),xaxt="n",ann=FALSE)
  points(tseq,tspvar1,pch=16)
  lines(tseq,tspvar2,lty="dashed")
  points(tseq,tspvar2,pch=1)
  lines(tseq,tspvar3,lty=1,col="grey")
  points(tseq,tspvar3,pch=16,col="grey")
  axis(side=1,at=c(0,3,6,9,12),las=1,cex.axis=0.9,padj=-0.80)
  axis(side=2,las=1,cex.axis=0.9, hadj=0.82)
  mtext("Months",cex=1.2,side=1,line=1.5)
  mtext(tsptitle,cex=1.2,side=2,line=3.1)
}

createEDriversPlots	<- function( model, selection) {
  elt <- StrathE2E2:::elt
  #tsmonthplot1 <-StrathE2E2:::tsmonthplot1
  #tsmonthplot2 <-StrathE2E2:::tsmonthplot2
  #tsmonthplot3 <-StrathE2E2:::tsmonthplot3
  oo <- options()
  on.exit(options(oo))
  #start_par = par()$mfrow
  #on.exit(par(mfrow = start_par))
  data <- elt(model, "data")
  physics.drivers <- elt(data, "physics.drivers")
  chemistry.drivers <- elt(data, "chemistry.drivers")
  physical.parameters <- elt(data, "physical.parameters")
  habitat_areas <- elt(physical.parameters, "habitat_areas")
  so_depth <- elt(physical.parameters, "so_depth")
  d_depth <- elt(physical.parameters, "d_depth")
  si_depth <- elt(physical.parameters, "si_depth")
  shallowprop <- sum(habitat_areas[1:4])
  #par(mfrow = c(4, 4))
  if (selection == "Surface irradiance") {
  tsmonthplot1("Surface irradiance", physics.drivers$sslight)
  }
  if (selection == "Susp.partic. matter") {
  tsmonthplot2("Susp.partic. matter", exp(physics.drivers$so_logespm)/1000, 
               exp(physics.drivers$si_logespm)/1000)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Offshore", 
                                                                 "Inshore"), col = c("black", "black"), pch = c(16, 1), 
         lty = c(1, 2), pt.cex = c(1, 1), cex = c(0.9, 0.9))
  }
  if (selection == "Temperature") {
  tsmonthplot3("Temperature", (physics.drivers$so_temp), (physics.drivers$si_temp), 
               (physics.drivers$d_temp))
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "Diffusivity gradient") {
  vdif <- ((10^(physics.drivers$logkvert))/(physics.drivers$mixlscale * 
                                              (so_depth + d_depth))) * (60 * 60 * 24)
  tsmonthplot1("Diffusivity gradient", vdif)
  }
  if (selection == "External Inflow") {
  so_inflowvol <- physics.drivers$so_inflow * (so_depth * 
                                                 (1 - shallowprop))
  d_inflowvol <- physics.drivers$d_inflow * (d_depth * (1 - 
                                                          shallowprop))
  si_inflowvol <- physics.drivers$si_inflow * (si_depth * 
                                                 (shallowprop))
  tsmonthplot3("External Inflow", so_inflowvol, si_inflowvol, 
               d_inflowvol)

  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "River discharge") {
  rivinflow <- physics.drivers$rivervol * (si_depth * (shallowprop))
  tsmonthplot1("River discharge", rivinflow)
  }
  if (selection == "Wave height") {
  tsmonthplot1("Wave height", physics.drivers$Inshore_waveheight)
  }
  if (selection == "Sediment disturbance") {
  offshoreseddist <- (physics.drivers$d1_pdist * habitat_areas[6] + 
                        physics.drivers$d2_pdist * habitat_areas[7] + physics.drivers$d3_pdist * 
                        habitat_areas[8])/sum(habitat_areas[6:8])
  inshoreseddist <- (physics.drivers$s1_pdist * habitat_areas[2] + 
                       physics.drivers$s2_pdist * habitat_areas[3] + physics.drivers$s3_pdist * 
                       habitat_areas[4])/sum(habitat_areas[2:4])
  tsmonthplot2("Sediment disturbance", offshoreseddist, inshoreseddist)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Offshore", 
                                                                 "Inshore"), col = c("black", "black"), pch = c(16, 1), 
         lty = c(1, 2), pt.cex = c(1, 1), cex = c(0.9, 0.9))
  }
  if (selection == "Boundary nitrate") {
  tsmonthplot3("Boundary nitrate", chemistry.drivers$so_nitrate, 
               chemistry.drivers$si_nitrate, chemistry.drivers$d_nitrate)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "Boundary ammonia") {
  tsmonthplot3("Boundary ammonia", chemistry.drivers$so_ammonia, 
               chemistry.drivers$si_ammonia, chemistry.drivers$d_ammonia)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "Boundary phytoplankton") {
  tsmonthplot3("Boundary phytoplankton", chemistry.drivers$so_phyt, 
               chemistry.drivers$si_phyt, chemistry.drivers$d_phyt)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "Boundary detritus") {
  tsmonthplot3("Boundary detritus", chemistry.drivers$so_detritus, 
               chemistry.drivers$si_detritus, chemistry.drivers$d_detritus)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Surf.off", 
                                                                 "Inshore", "Deep"), col = c("black", "black", "grey"), 
         pch = c(16, 1, 16), lty = c(1, 2, 1), pt.cex = c(1, 
                                                          1), cex = c(0.9, 0.9, 0.9))
  }
  if (selection == "River nitrate") {
  tsmonthplot1("River nitrate", chemistry.drivers$rivnitrate)
  }
  if (selection == "River ammonia") {
  tsmonthplot1("River ammonia", chemistry.drivers$rivammonia)
  }
  if (selection == "Atmospheric nitrate") {
  tsmonthplot2("Atmospheric nitrate", chemistry.drivers$so_atmnitrate, 
               chemistry.drivers$si_atmnitrate)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Offshore", 
                                                                 "Inshore"), col = c("black", "black"), pch = c(16, 1), 
         lty = c(1, 2), pt.cex = c(1, 1), cex = c(0.9, 0.9))
  }
  if (selection == "Atmospheric ammonia") {
  tsmonthplot2("Atmospheric ammonia", chemistry.drivers$so_atmammonia, 
               chemistry.drivers$si_atmammonia)
  legend("topright", box.lty = 0, bg = "transparent", legend = c("Offshore", 
                                                                 "Inshore"), col = c("black", "black"), pch = c(16, 1), 
         lty = c(1, 2), pt.cex = c(1, 1), cex = c(0.9, 0.9))
  }
}