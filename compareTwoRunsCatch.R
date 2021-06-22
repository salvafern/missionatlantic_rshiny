compareTwoRunsCatch <- function (model1 = NA, from.csv1 = FALSE, results1, model2 = NA, 
          from.csv2 = FALSE, results2, log.pc = "PC", zone = "W", 
          bpmin = (-50), bpmax = (+50), maintitle = "") 
{
  elt <- StrathE2E2:::elt

  if (from.csv1 == FALSE) {
    print("Using baseline data held in memory from an existing model run")
    final.year.outputs1 <- elt(results1, "final.year.outputs")
    if (zone == "I") 
      baselinelanddata <- elt(final.year.outputs1, "inshore_landmat")
    if (zone == "O") 
      baselinelanddata <- elt(final.year.outputs1, "offshore_landmat")
    if (zone == "W") 
      baselinelanddata <- elt(final.year.outputs1, "inshore_landmat") + 
      elt(final.year.outputs1, "offshore_landmat")
    if (zone == "I") 
      baselinediscdata <- elt(final.year.outputs1, "inshore_discmat")
    if (zone == "O") 
      baselinediscdata <- elt(final.year.outputs1, "offshore_discmat")
    if (zone == "W") 
      baselinediscdata <- elt(final.year.outputs1, "inshore_discmat") + 
      elt(final.year.outputs1, "offshore_discmat")
  }
  if (from.csv2 == FALSE) {
    print("Using scenario data held in memory from an existing model run")
    final.year.outputs2 <- elt(results2, "final.year.outputs")
    if (zone == "I") 
      scenariolanddata <- elt(final.year.outputs2, "inshore_landmat")
    if (zone == "O") 
      scenariolanddata <- elt(final.year.outputs2, "offshore_landmat")
    if (zone == "W") 
      scenariolanddata <- elt(final.year.outputs2, "inshore_landmat") + 
      elt(final.year.outputs2, "offshore_landmat")
    if (zone == "I") 
      scenariodiscdata <- elt(final.year.outputs2, "inshore_discmat")
    if (zone == "O") 
      scenariodiscdata <- elt(final.year.outputs2, "offshore_discmat")
    if (zone == "W") 
      scenariodiscdata <- elt(final.year.outputs2, "inshore_discmat") + 
      elt(final.year.outputs2, "offshore_discmat")
  }
  if (from.csv1 == TRUE) {
    print("Using baseline data held in a csv files from a past model run")
    resultsdir1 <- elt(model1, "setup", "resultsdir")
    model.ident1 <- elt(model1, "setup", "model.ident")
    baselandifile <- csvname(resultsdir1, "INSHORE_landingcomposition_by_gear", 
                             model.ident1)
    baselandofile <- csvname(resultsdir1, "OFFSHORE_landingcomposition_by_gear", 
                             model.ident1)
    basediscifile <- csvname(resultsdir1, "INSHORE_discardcomposition_by_gear", 
                             model.ident1)
    basediscofile <- csvname(resultsdir1, "OFFSHORE_discardcomposition_by_gear", 
                             model.ident1)
    check.exists(baselandifile)
    check.exists(baselandofile)
    check.exists(basediscifile)
    check.exists(basediscofile)
    baselinelandidata <- readcsv(baselandifile, row.names = 1)
    baselinelandodata <- readcsv(baselandofile, row.names = 1)
    baselinediscidata <- readcsv(basediscifile, row.names = 1)
    baselinediscodata <- readcsv(basediscofile, row.names = 1)
    if (zone == "I") {
      baselinelanddata <- baselinelandidata
      baselinediscdata <- baselinediscidata
    }
    if (zone == "O") {
      baselinelanddata <- baselinelandodata
      baselinediscdata <- baselinediscodata
    }
    if (zone == "W") {
      baselinelanddata <- baselinelandidata + baselinelandodata
      baselinediscdata <- baselinediscidata + baselinediscodata
    }
  }
  if (from.csv2 == TRUE) {
    print("Using scenario data held in a csv files from a past model run")
    resultsdir2 <- elt(model2, "setup", "resultsdir")
    model.ident2 <- elt(model2, "setup", "model.ident")
    scenlandifile <- csvname(resultsdir2, "INSHORE_landingcomposition_by_gear", 
                             model.ident2)
    scenlandofile <- csvname(resultsdir2, "OFFSHORE_landingcomposition_by_gear", 
                             model.ident2)
    scendiscifile <- csvname(resultsdir2, "INSHORE_discardcomposition_by_gear", 
                             model.ident2)
    scendiscofile <- csvname(resultsdir2, "OFFSHORE_discardcomposition_by_gear", 
                             model.ident2)
    check.exists(scenlandifile)
    check.exists(scenlandofile)
    check.exists(scendiscifile)
    check.exists(scendiscofile)
    scenariolandidata <- readcsv(scenlandifile, row.names = 1)
    scenariolandodata <- readcsv(scenlandofile, row.names = 1)
    scenariodiscidata <- readcsv(scendiscifile, row.names = 1)
    scenariodiscodata <- readcsv(scendiscofile, row.names = 1)
    if (zone == "I") {
      scenariolanddata <- scenariolandidata
      scenariodiscdata <- scenariodiscidata
    }
    if (zone == "O") {
      scenariolanddata <- scenariolandodata
      scenariodiscdata <- scenariodiscodata
    }
    if (zone == "W") {
      scenariolanddata <- scenariolandidata + scenariolandodata
      scenariodiscdata <- scenariodiscidata + scenariodiscodata
    }
  }
  baselineland <- data.frame(rep(0, 11))
  scenarioland <- data.frame(rep(0, 11))
  baselinedisc <- data.frame(rep(0, 11))
  scenariodisc <- data.frame(rep(0, 11))
  baselineland[1, 1] <- sum(baselinelanddata[11, ], na.rm = TRUE)
  baselineland[2, 1] <- sum(baselinelanddata[5, ], na.rm = TRUE)
  baselineland[3, 1] <- sum(baselinelanddata[6, ], na.rm = TRUE)
  baselineland[4, 1] <- sum(baselinelanddata[7, ], na.rm = TRUE)
  baselineland[5, 1] <- sum(baselinelanddata[1, ], na.rm = TRUE)
  baselineland[6, 1] <- sum(baselinelanddata[4, ], na.rm = TRUE)
  baselineland[7, 1] <- sum(baselinelanddata[3, ], na.rm = TRUE)
  baselineland[8, 1] <- sum(baselinelanddata[2, ], na.rm = TRUE)
  baselineland[9, 1] <- sum(baselinelanddata[8, ], na.rm = TRUE)
  baselineland[10, 1] <- sum(baselinelanddata[9, ], na.rm = TRUE)
  baselineland[11, 1] <- sum(baselinelanddata[10, ], na.rm = TRUE)
  scenarioland[1, 1] <- sum(scenariolanddata[11, ], na.rm = TRUE)
  scenarioland[2, 1] <- sum(scenariolanddata[5, ], na.rm = TRUE)
  scenarioland[3, 1] <- sum(scenariolanddata[6, ], na.rm = TRUE)
  scenarioland[4, 1] <- sum(scenariolanddata[7, ], na.rm = TRUE)
  scenarioland[5, 1] <- sum(scenariolanddata[1, ], na.rm = TRUE)
  scenarioland[6, 1] <- sum(scenariolanddata[4, ], na.rm = TRUE)
  scenarioland[7, 1] <- sum(scenariolanddata[3, ], na.rm = TRUE)
  scenarioland[8, 1] <- sum(scenariolanddata[2, ], na.rm = TRUE)
  scenarioland[9, 1] <- sum(scenariolanddata[8, ], na.rm = TRUE)
  scenarioland[10, 1] <- sum(scenariolanddata[9, ], na.rm = TRUE)
  scenarioland[11, 1] <- sum(scenariolanddata[10, ], na.rm = TRUE)
  baselinedisc[1, 1] <- sum(baselinediscdata[11, ], na.rm = TRUE)
  baselinedisc[2, 1] <- sum(baselinediscdata[5, ], na.rm = TRUE)
  baselinedisc[3, 1] <- sum(baselinediscdata[6, ], na.rm = TRUE)
  baselinedisc[4, 1] <- sum(baselinediscdata[7, ], na.rm = TRUE)
  baselinedisc[5, 1] <- sum(baselinediscdata[1, ], na.rm = TRUE)
  baselinedisc[6, 1] <- sum(baselinediscdata[4, ], na.rm = TRUE)
  baselinedisc[7, 1] <- sum(baselinediscdata[3, ], na.rm = TRUE)
  baselinedisc[8, 1] <- sum(baselinediscdata[2, ], na.rm = TRUE)
  baselinedisc[9, 1] <- sum(baselinediscdata[8, ], na.rm = TRUE)
  baselinedisc[10, 1] <- sum(baselinediscdata[9, ], na.rm = TRUE)
  baselinedisc[11, 1] <- sum(baselinediscdata[10, ], na.rm = TRUE)
  scenariodisc[1, 1] <- sum(scenariodiscdata[11, ], na.rm = TRUE)
  scenariodisc[2, 1] <- sum(scenariodiscdata[5, ], na.rm = TRUE)
  scenariodisc[3, 1] <- sum(scenariodiscdata[6, ], na.rm = TRUE)
  scenariodisc[4, 1] <- sum(scenariodiscdata[7, ], na.rm = TRUE)
  scenariodisc[5, 1] <- sum(scenariodiscdata[1, ], na.rm = TRUE)
  scenariodisc[6, 1] <- sum(scenariodiscdata[4, ], na.rm = TRUE)
  scenariodisc[7, 1] <- sum(scenariodiscdata[3, ], na.rm = TRUE)
  scenariodisc[8, 1] <- sum(scenariodiscdata[2, ], na.rm = TRUE)
  scenariodisc[9, 1] <- sum(scenariodiscdata[8, ], na.rm = TRUE)
  scenariodisc[10, 1] <- sum(scenariodiscdata[9, ], na.rm = TRUE)
  scenariodisc[11, 1] <- sum(scenariodiscdata[10, ], na.rm = TRUE)
  rownames(baselineland) <- c("Macrophytes", "Susp/dep feeding benthos", 
                              "Carn/scav feeding benthos", "Pelagic invertebrates", 
                              "Planktivorous fish", "Migratory fish", "Non-quota demersal fish", 
                              "Quota-limited demersal fish", "Birds", "Pinnipeds", 
                              "LANDINGS            Cetaceans")
  rownames(baselinedisc) <- c("Macrophytes", "Susp/dep feeding benthos", 
                              "Carn/scav feeding benthos", "Pelagic invertebrates", 
                              "Planktivorous fish", "Migratory fish", "Non-quota demersal fish", 
                              "Quota-limited demersal fish", "Birds", "Pinnipeds", 
                              "DISCARDS            Cetaceans")
  rownames(baselinedisc) <- rownames(baselinedisc)
  rownames(scenarioland) <- rownames(baselineland)
  rownames(scenariodisc) <- rownames(baselinedisc)
  changeland <- baselineland
  changeland[, 1] <- NA
  changeland[, 2] <- NA
  colnames(changeland) <- c("LG", "PC")
  changedisc <- baselinedisc
  changedisc[, 1] <- NA
  changedisc[, 2] <- NA
  colnames(changedisc) <- c("LG", "PC")
  for (zz in 1:nrow(changeland)) {
    if ((is.na(scenarioland[zz, 1]) == FALSE) & (is.na(baselineland[zz, 
                                                                    1]) == FALSE) & (baselineland[zz, 1] > 0)) {
      if (scenarioland[zz, 1] > 0) 
        changeland[zz, 1] <- log10(scenarioland[zz, 
                                                1]/baselineland[zz, 1])
      if (scenarioland[zz, 1] == 0) 
        changeland[zz, 1] <- log10((scenarioland[zz, 
                                                 1] + 1e-20)/baselineland[zz, 1])
      changeland[zz, 2] <- ((scenarioland[zz, 1]/baselineland[zz, 
                                                              1]) - 1) * 100
    }
  }
  for (zz in 1:nrow(changedisc)) {
    if ((is.na(scenariodisc[zz, 1]) == FALSE) & (is.na(baselinedisc[zz, 
                                                                    1]) == FALSE) & (baselinedisc[zz, 1] > 0)) {
      if (scenariodisc[zz, 1] > 0) 
        changedisc[zz, 1] <- log10(scenariodisc[zz, 
                                                1]/baselinedisc[zz, 1])
      if (scenariodisc[zz, 1] == 0) 
        changedisc[zz, 1] <- log10((scenariodisc[zz, 
                                                 1] + 1e-20)/baselinedisc[zz, 1])
      changedisc[zz, 2] <- ((scenariodisc[zz, 1]/baselinedisc[zz, 
                                                              1]) - 1) * 100
    }
  }
  if (log.pc == "LG") {
    changeland2p <- as.data.frame(changeland[, 1])
    changedisc2p <- as.data.frame(changedisc[, 1])
  }
  if (log.pc == "PC") {
    changeland2p <- as.data.frame(changeland[, 2])
    changedisc2p <- as.data.frame(changedisc[, 2])
  }
  rownames(changeland2p) <- rownames(changeland)
  rownames(changedisc2p) <- rownames(changedisc)
  overlabelland <- rep("", nrow(changeland2p))
  overlabeldisc <- rep("", nrow(changedisc2p))
  for (zz in 1:nrow(changeland2p)) {
    if (is.na(changeland2p[zz, 1]) == FALSE) {
      if (changeland2p[zz, 1] < (bpmin)) {
        overlabelland[zz] <- as.character((floor(100 * 
                                                   changeland2p[zz, 1]))/100)
        changeland2p[zz, 1] <- bpmin
      }
    }
  }
  for (zz in 1:nrow(changedisc2p)) {
    if (is.na(changedisc2p[zz, 1]) == FALSE) {
      if (changedisc2p[zz, 1] < (bpmin)) {
        overlabeldisc[zz] <- as.character((floor(100 * 
                                                   changedisc2p[zz, 1]))/100)
        changedisc2p[zz, 1] <- bpmin
      }
    }
  }
  wcolvec <- rep("green", length(changeland2p[, 1]))
  wcolvec[which(changeland2p[, 1] < 0)] <- "red"
  scolvec <- rep("black", length(changedisc2p[, 1]))
  scolvec[which(changedisc2p[, 1] < 0)] <- "grey"
  par(mfrow = c(2, 1))
  par(mar = c(1.5, 13, 2, 1))
  
  changeland2p_df <- data.frame(changeland2p[, 1])
  names(changeland2p_df)[1] <- "x"
  changeland2p_df$y <- rownames(changeland2p)
  changeland2p_df$colour <- ifelse(changeland2p_df$x < 0, "negative","positive")
  
  if (log.pc == "PC") 
    xlabel <- "Percent change in landings & discards"
  if (log.pc == "LG") 
    xlabel <- "Log10 change in landings & discards"
  
  barplot_land <-  ggplot(data = changeland2p_df,aes(x = x, y = y)) +
    geom_bar(stat = "identity",aes(fill = colour)) +
    scale_fill_manual(labels = c("Less than baseline", "More than baseline"), values=c(negative="firebrick1",positive="green")) +
    ggtitle(maintitle) +
    coord_cartesian(xlim=c(bpmin,bpmax)) +
    theme(axis.text.y =element_text(size=8)) +
    xlab(xlabel) + 
    ylab("") +
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.position = "none")
  
  changedisc2p_df <- data.frame(changedisc2p[, 1])
  names(changedisc2p_df)[1] <- "x"
  changedisc2p_df$y <- rownames(changedisc2p)
  changedisc2p_df$colour <- ifelse(changedisc2p_df$x < 0, "negative","positive")
  
  barplot_disc <-  ggplot(data = changedisc2p_df,aes(x = x, y = y)) +
    geom_bar(stat = "identity",aes(fill = colour)) +
    scale_fill_manual(labels = c("Less than baseline", "More than baseline"), values=c(negative="firebrick1",positive="green")) +
    ggtitle(maintitle) +
    coord_cartesian(xlim=c(bpmin,bpmax)) +
    theme(axis.text.y =element_text(size=8)) +
    xlab(xlabel) + 
    ylab("") +
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.title = element_blank())
  
  figure <- ggarrange(barplot_land, barplot_disc,
                      ncol = 1, nrow = 2,
                      common.legend = FALSE,
                      align = c("v"))
  
  return(figure)

}
