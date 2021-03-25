# useful function to create plots or UI

createCatchPlotPerGuild	<- function( model, results, dsa ) {
  elt <- StrathE2E2:::elt
  data <- elt(model, "data")
  fleet.model <- elt(data, "fleet.model")
  gear_codes <- elt(fleet.model, "gear_codes")
  final.year.outputs <- elt(results, "final.year.outputs")
  offshore_catchmat <- elt(final.year.outputs, "offshore_catchmat")
  offshore_discmat <- elt(final.year.outputs, "offshore_discmat")
  offshore_landmat <- elt(final.year.outputs, "offshore_landmat")
  inshore_catchmat <- elt(final.year.outputs, "inshore_catchmat")
  inshore_discmat <- elt(final.year.outputs, "inshore_discmat")
  inshore_landmat <- elt(final.year.outputs, "inshore_landmat")
  #dsa <- guild
  mt <- (rownames(offshore_catchmat))[dsa]
  offshore_data2plot <- rbind(offshore_discmat[dsa, ], 
                              offshore_landmat[dsa, ])
  inshore_data2plot <- rbind(inshore_discmat[dsa, ], inshore_landmat[dsa, 
  ])
  colnames(offshore_data2plot) <- gear_codes
  if (sum(offshore_data2plot + inshore_data2plot) > 0) {
    yaxmax <- (1.2 * max(c(offshore_catchmat[dsa, ], 
                           inshore_catchmat[dsa, ])))
  }
  if (sum(offshore_data2plot + inshore_data2plot) == 0) {
    yaxmax <- 1
  }
  barplot(offshore_data2plot, col = c("black", "green"), 
          ylim = c(0, yaxmax), xlim = c(0, 12), width = rep(0.4, 
                                                            12), space = c(0.5, rep(1.2, 11)), yaxt = "n", 
          ann = FALSE, cex.axis = 0.6)
   axis(side = 2, las = 1, cex.axis = 0.9)
   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
   title(main = mt, cex.main = 1)
  barplot(inshore_data2plot, col = c("grey", "blue"),
          add = T, width = rep(0.4, 12), space = c(1.5, rep(1.2,
                                                            11)), yaxt = "n", xaxt = "n", ann = FALSE)
  
  # mt <- "All guilds combined"
  # offshore_data2plot <- rbind(colSums(offshore_discmat), colSums(offshore_landmat))
  # inshore_data2plot <- rbind(colSums(inshore_discmat), colSums(inshore_landmat))
  # colnames(offshore_data2plot) <- gear_codes
  # if (sum(offshore_data2plot + inshore_data2plot) > 0) {
  #   yaxmax <- (1.2 * max(c(colSums(offshore_catchmat), colSums(inshore_catchmat))))
  # }
  # if (sum(offshore_data2plot + inshore_data2plot) == 0) {
  #   yaxmax <- 1
  # }
  # barplot(offshore_data2plot, col = c("black", "green"), ylim = c(0, 
  #                                                                 yaxmax), xlim = c(0, 12), width = rep(0.4, 12), space = c(0.5, 
  #                                                                                                                           rep(1.2, 11)), yaxt = "n", ann = FALSE, cex.axis = 0.5)
  # axis(side = 2, las = 1, cex.axis = 0.9)
  # if (dsa == 1) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # if (dsa == 3) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # if (dsa == 5) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # if (dsa == 7) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # if (dsa == 9) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # if (dsa == 11) 
  #   mtext("Catch", cex = 0.7, side = 2, line = 3.5)
  # title(main = mt, cex.main = 1)
  # barplot(inshore_data2plot, col = c("grey", "blue"), add = T, 
  #         width = rep(0.4, 12), space = c(1.5, rep(1.2, 11)), 
  #         yaxt = "n", xaxt = "n", ann = FALSE)
  legend(grconvertX(0.15, "ndc", "user"), grconvertY(0.05,
                                                     "ndc", "user"), c("offshore landings", "offshore discards",
                                                                       "inshore landings", "inshore discards"), fill = c("green",
                                                                                                                         "black", "blue", "grey"), ncol = 4, bty = "n", xpd = NA)
  
}