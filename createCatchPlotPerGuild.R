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
  if(dsa < 12){
  mt <- (rownames(offshore_catchmat))[dsa]
  offshore_data2plot <- rbind(offshore_discmat[dsa, ], 
                              offshore_landmat[dsa, ])
  inshore_data2plot <- rbind(inshore_discmat[dsa, ], inshore_landmat[dsa, 
  ])
  #colnames(offshore_data2plot) <- gear_codes
  gear_labels <- c("Pelagic_Trawl+Seine", "Sandeel+sprat_trawl", "Longline_mackerel",
                    "Beam_Trawl_BT1+BT2", "Demeral_Seine", "Demersal_Otter_Trawl_TR1", 
                    "Gill_Nets+Longline_demersal", "Beam_Trawl_shrimp", "Nephrops_Trawl_TR2", 
                    "Creels", "Mollusc_Dredge", "Whaler")
  if (sum(offshore_data2plot + inshore_data2plot) > 0) {
    yaxmax <- (1.2 * max(c(offshore_catchmat[dsa, ], 
                           inshore_catchmat[dsa, ])))
  }
  if (sum(offshore_data2plot + inshore_data2plot) == 0) {
    yaxmax <- 1
  }
  par(mar=c(11,6,4,4))
  barplot(offshore_data2plot, col = c("black", "green"), 
          ylim = c(0, yaxmax), xlim = c(0, 12), width = rep(0.4, 
                                                            12), space = c(0.5, rep(1.2, 11)), yaxt = "n", xaxt = "n", 
          ann = FALSE, cex.axis = 0.6)
   axis(1, labels = FALSE)
   axis(side = 2, las = 1, cex.axis = 0.9)
   text(x = 1:12, y = 0, srt = 45, label = gear_labels, adj = 1.1,  xpd = NA, cex = 1.2)
   mtext("Catch", cex = 1.2, side = 2, line = 3.5)
   title(main = mt, cex.main = 1.2)
  barplot(inshore_data2plot, col = c("grey", "blue"),
          add = T, width = rep(0.4, 12), space = c(1.5, rep(1.2,
                                                            11)), yaxt = "n", xaxt = "n", ann = FALSE)
  # legend(grconvertX(0.15, "ndc", "user"), grconvertY(0.05,
  #                                                    "ndc", "user"), c("offshore landings", "offshore discards",
  #                                                                      "inshore landings", "inshore discards"), fill = c("green",
  #                                                                                                                        "black", "blue", "grey"), ncol = 4, bty = "n", xpd = NA)
  legend("top", c("offshore landings", "offshore discards","inshore landings", "inshore discards"), fill = c("green",
                                                                                                                         "black", "blue", "grey"), ncol = 4, bty = "n", xpd = NA)
  }
  if(dsa == 12){
  mt <- "All guilds combined"
  offshore_data2plot <- rbind(colSums(offshore_discmat), colSums(offshore_landmat))
  inshore_data2plot <- rbind(colSums(inshore_discmat), colSums(inshore_landmat))
  colnames(offshore_data2plot) <- gear_codes
  if (sum(offshore_data2plot + inshore_data2plot) > 0) {
    yaxmax <- (1.2 * max(c(colSums(offshore_catchmat), colSums(inshore_catchmat))))
  }
  if (sum(offshore_data2plot + inshore_data2plot) == 0) {
    yaxmax <- 1
  }
  gear_labels <- c("Pelagic_Trawl+Seine", "Sandeel+sprat_trawl", "Longline_mackerel",
                   "Beam_Trawl_BT1+BT2", "Demeral_Seine", "Demersal_Otter_Trawl_TR1", 
                   "Gill_Nets+Longline_demersal", "Beam_Trawl_shrimp", "Nephrops_Trawl_TR2", 
                   "Creels", "Mollusc_Dredge", "Whaler")
  par(mar=c(11,6,4,4))
  barplot(offshore_data2plot, col = c("black", "green"), ylim = c(0,
                                                                  yaxmax), xlim = c(0, 12), width = rep(0.4, 12), space = c(0.5, rep(1.2, 11)), yaxt = "n", xaxt = "n", ann = FALSE, cex.axis = 0.5)

  axis(1, labels = FALSE)
  axis(side = 2, las = 1, cex.axis = 0.9)
  text(x = 1:12, y = 0, srt = 45, label = gear_labels, adj = 1.1,  xpd = NA, cex = 1.2)
  mtext("Catch", cex = 1.2, side = 2, line = 3.5)
  title(main = mt, cex.main = 1.2)
  barplot(inshore_data2plot, col = c("grey", "blue"), add = T,
          width = rep(0.4, 12), space = c(1.5, rep(1.2, 11)),
          yaxt = "n", xaxt = "n", ann = FALSE)
  # legend(grconvertX(0.15, "ndc", "user"), grconvertY(0.05,
  #                                                    "ndc", "user"), c("offshore landings", "offshore discards",
  #                                                                      "inshore landings", "inshore discards"), fill = c("green",
  #                                                                                                                       "black", "blue", "grey"), ncol = 4, bty = "n", xpd = NA)
  legend("topright", c("offshore landings", "offshore discards","inshore landings", "inshore discards"), fill = c("green","black", "blue", "grey"), ncol = 4, bty = "n", xpd = NA)
  }
}