compareTwoRunsAAM <- function (model1 = NA, from.csv1 = FALSE, results1, model2 = NA, 
          from.csv2 = FALSE, results2, log.pc = "PC", zone = "W", 
          bpmin = (-50), bpmax = (+50), maintitle = "", outputType = "PLOT") 
{
  elt <- StrathE2E2:::elt
  if (from.csv1 == FALSE) {
    print("Using baseline data held in memory from an existing model run")
    final.year.outputs1 <- elt(results1, "final.year.outputs")
    if (zone == "I") 
      baselinedata <- elt(final.year.outputs1, "mass_results_inshore")
    if (zone == "O") 
      baselinedata <- elt(final.year.outputs1, "mass_results_offshore")
    if (zone == "W") 
      baselinedata <- elt(final.year.outputs1, "mass_results_wholedomain")
  }
  if (from.csv2 == FALSE) {
    print("Using scenario data held in memory from an existing model run")
    final.year.outputs2 <- elt(results2, "final.year.outputs")
    if (zone == "I") 
      scenariodata <- elt(final.year.outputs2, "mass_results_inshore")
    if (zone == "O") 
      scenariodata <- elt(final.year.outputs2, "mass_results_offshore")
    if (zone == "W") 
      scenariodata <- elt(final.year.outputs2, "mass_results_wholedomain")
  }
  if (from.csv1 == TRUE) {
    print("Using baseline data held in a csv files from a past model run")
    resultsdir1 <- elt(model1, "setup", "resultsdir")
    model.ident1 <- elt(model1, "setup", "model.ident")
    if (zone == "I") 
      basefile <- csvname(resultsdir1, "INSHORE_model_anav_biomass", 
                          model.ident1)
    if (zone == "O") 
      basefile <- csvname(resultsdir1, "OFFSHORE_model_anav_biomass", 
                          model.ident1)
    if (zone == "W") 
      basefile <- csvname(resultsdir1, "WHOLEDOMAIN_model_anav_biomass", 
                          model.ident1)
    check.exists(basefile)
    baselinedata <- readcsv(basefile)
  }
  if (from.csv2 == TRUE) {
    print("Using scenario data held in a csv files from a past model run")
    resultsdir2 <- elt(model2, "setup", "resultsdir")
    model.ident2 <- elt(model2, "setup", "model.ident")
    if (zone == "I") 
      scenfile <- csvname(resultsdir2, "INSHORE_model_anav_biomass", 
                          model.ident2)
    if (zone == "O") 
      scenfile <- csvname(resultsdir2, "OFFSHORE_model_anav_biomass", 
                          model.ident2)
    if (zone == "W") 
      scenfile <- csvname(resultsdir2, "WHOLEDOMAIN_model_anav_biomass", 
                          model.ident2)
    check.exists(scenfile)
    scenariodata <- readcsv(scenfile)
  }
  baselinewater <- data.frame(rep(0, 15))
  scenariowater <- data.frame(rep(0, 15))
  baselineseabed <- data.frame(rep(0, 8))
  scenarioseabed <- data.frame(rep(0, 8))
  baselinewater[1, 1] <- sum(baselinedata[1:2, 1], na.rm = TRUE)
  baselinewater[2, 1] <- sum(baselinedata[8:9, 1], na.rm = TRUE)
  baselinewater[3, 1] <- sum(baselinedata[11:12, 1], na.rm = TRUE)
  baselinewater[4, 1] <- (baselinedata[14, 1])
  baselinewater[5, 1] <- sum(baselinedata[15:16, 1], na.rm = TRUE)
  baselinewater[6, 1] <- (baselinedata[19, 1])
  baselinewater[7, 1] <- (baselinedata[17, 1])
  baselinewater[8, 1] <- (baselinedata[21, 1])
  baselinewater[9, 1] <- (baselinedata[18, 1])
  baselinewater[10, 1] <- sum(baselinedata[23:24, 1], na.rm = TRUE)
  baselinewater[11, 1] <- (baselinedata[25, 1])
  baselinewater[12, 1] <- sum(baselinedata[26:27, 1], na.rm = TRUE)
  baselinewater[13, 1] <- (baselinedata[28, 1])
  baselinewater[14, 1] <- (baselinedata[29, 1])
  baselinewater[15, 1] <- (baselinedata[30, 1])
  baselineseabed[1, 1] <- (baselinedata[5, 1])
  baselineseabed[2, 1] <- (baselinedata[6, 1])
  baselineseabed[3, 1] <- (baselinedata[3, 1])
  baselineseabed[4, 1] <- (baselinedata[7, 1])
  baselineseabed[5, 1] <- (baselinedata[10, 1])
  baselineseabed[6, 1] <- (baselinedata[13, 1])
  baselineseabed[7, 1] <- (baselinedata[20, 1])
  baselineseabed[8, 1] <- (baselinedata[22, 1])
  scenariowater[1, 1] <- sum(scenariodata[1:2, 1], na.rm = TRUE)
  scenariowater[2, 1] <- sum(scenariodata[8:9, 1], na.rm = TRUE)
  scenariowater[3, 1] <- sum(scenariodata[11:12, 1], na.rm = TRUE)
  scenariowater[4, 1] <- (scenariodata[14, 1])
  scenariowater[5, 1] <- sum(scenariodata[15:16, 1], na.rm = TRUE)
  scenariowater[6, 1] <- (scenariodata[19, 1])
  scenariowater[7, 1] <- (scenariodata[17, 1])
  scenariowater[8, 1] <- (scenariodata[21, 1])
  scenariowater[9, 1] <- (scenariodata[18, 1])
  scenariowater[10, 1] <- sum(scenariodata[23:24, 1], na.rm = TRUE)
  scenariowater[11, 1] <- (scenariodata[25, 1])
  scenariowater[12, 1] <- sum(scenariodata[26:27, 1], na.rm = TRUE)
  scenariowater[13, 1] <- (scenariodata[28, 1])
  scenariowater[14, 1] <- (scenariodata[29, 1])
  scenariowater[15, 1] <- (scenariodata[30, 1])
  scenarioseabed[1, 1] <- (scenariodata[5, 1])
  scenarioseabed[2, 1] <- (scenariodata[6, 1])
  scenarioseabed[3, 1] <- (scenariodata[3, 1])
  scenarioseabed[4, 1] <- (scenariodata[7, 1])
  scenarioseabed[5, 1] <- (scenariodata[10, 1])
  scenarioseabed[6, 1] <- (scenariodata[13, 1])
  scenarioseabed[7, 1] <- (scenariodata[20, 1])
  scenarioseabed[8, 1] <- (scenariodata[22, 1])
  rownames(baselinewater) <- c("Suspended bacteria & detritus", 
                               "Water column ammonia", "Water column nitrate", "Macrophytes", 
                               "Phytoplankton", "Benthic susp/dep feeder larvae", "Omnivorous zooplankton", 
                               "Benthic carn/scav feeder larvae", "Carnivorous zooplankton", 
                               "Plantiv. fish adults+larvae", "Migratory fish", "Demersal fish adults+larvae", 
                               "Birds", "Pinnipeds", "Cetaceans")
  rownames(baselineseabed) <- c("Fishery discards & offal", 
                                "Corpses", "Sediment bacteria & detritus", "Macrophyte debris", 
                                "Porewater ammonia", "Porewater nitrate", "Benthic susp/dep feeders", 
                                "Benthic carn/scav feeders")
  rownames(scenariowater) <- rownames(baselinewater)
  rownames(scenarioseabed) <- rownames(baselineseabed)
  changewater <- baselinewater
  changewater[, 1] <- NA
  changewater[, 2] <- NA
  colnames(changewater) <- c("LG", "PC")
  changeseabed <- baselineseabed
  changeseabed[, 1] <- NA
  changeseabed[, 2] <- NA
  colnames(changeseabed) <- c("LG", "PC")
  for (zz in 1:nrow(changewater)) {
    if ((is.na(scenariowater[zz, 1]) == FALSE) & (is.na(baselinewater[zz, 
                                                                      1]) == FALSE) & (baselinewater[zz, 1] > 0)) {
      if (scenariowater[zz, 1] > 0) 
        changewater[zz, 1] <- log10(scenariowater[zz, 
                                                  1]/baselinewater[zz, 1])
      if (scenariowater[zz, 1] == 0) 
        changewater[zz, 1] <- log10((scenariowater[zz, 
                                                   1] + 1e-20)/baselinewater[zz, 1])
      changewater[zz, 2] <- ((scenariowater[zz, 1]/baselinewater[zz, 
                                                                 1]) - 1) * 100
    }
  }
  for (zz in 1:nrow(changeseabed)) {
    if ((is.na(scenarioseabed[zz, 1]) == FALSE) & (is.na(baselineseabed[zz, 
                                                                        1]) == FALSE) & (baselineseabed[zz, 1] > 0)) {
      if (scenarioseabed[zz, 1] > 0) 
        changeseabed[zz, 1] <- log10(scenarioseabed[zz, 
                                                    1]/baselineseabed[zz, 1])
      if (scenarioseabed[zz, 1] == 0) 
        changeseabed[zz, 1] <- log10((scenarioseabed[zz, 
                                                     1] + 1e-20)/baselineseabed[zz, 1])
      changeseabed[zz, 2] <- ((scenarioseabed[zz, 1]/baselineseabed[zz, 
                                                                    1]) - 1) * 100
    }
  }
  if (log.pc == "LG") {
    changewater2p <- as.data.frame(changewater[, 1])
    changeseabed2p <- as.data.frame(changeseabed[, 1])
  }
  if (log.pc == "PC") {
    changewater2p <- as.data.frame(changewater[, 2])
    changeseabed2p <- as.data.frame(changeseabed[, 2])
  }
  rownames(changewater2p) <- rownames(changewater)
  rownames(changeseabed2p) <- rownames(changeseabed)
  overlabelwater <- rep("", nrow(changewater2p))
  overlabelseabed <- rep("", nrow(changeseabed2p))
  for (zz in 1:nrow(changewater2p)) {
    if (is.na(changewater2p[zz, 1]) == FALSE) {
      if (changewater2p[zz, 1] < (bpmin)) {
        overlabelwater[zz] <- as.character((floor(100 * 
                                                    changewater2p[zz, 1]))/100)
        changewater2p[zz, 1] <- bpmin
      }
    }
  }
  for (zz in 1:nrow(changeseabed2p)) {
    if (is.na(changeseabed2p[zz, 1]) == FALSE) {
      if (changeseabed2p[zz, 1] < (bpmin)) {
        overlabelseabed[zz] <- as.character((floor(100 * 
                                                     changeseabed2p[zz, 1]))/100)
        changeseabed2p[zz, 1] <- bpmin
      }
    }
  }
  
  if (log.pc == "PC") 
    xlabelwater <- "Percent change in annual average mass in water column"
  if (log.pc == "LG") 
    xlabelwater <- "Log10 change in annual average mass in water column"
  changewater2p_df <- data.frame(changewater2p[, 1])
  names(changewater2p_df)[1] <- "x"
  changewater2p_df$y <- rownames(changewater2p)
  changewater2p_df$colour <- ifelse(changewater2p_df$x <= 0, "negative","positive")
  changewater2p_df["type"] <- "waterColumn" 
  changewater2p_df$x[is.na(changewater2p_df$x)] = 0
  #changewater2p_df <- changewater2p_df[complete.cases(changewater2p_df), ]
  levels_order <- factor(changewater2p_df$y, level = c("Suspended bacteria & detritus", 
           "Water column ammonia", "Water column nitrate", "Macrophytes", 
           "Phytoplankton", "Benthic susp/dep feeder larvae", "Omnivorous zooplankton", 
           "Benthic carn/scav feeder larvae", "Carnivorous zooplankton", 
           "Plantiv. fish adults+larvae", "Migratory fish", "Demersal fish adults+larvae", 
           "Birds", "Pinnipeds", "Cetaceans"))
  barplot_water <-  ggplot(data = changewater2p_df,aes(x = x, y = levels_order)) +
    geom_bar(stat = "identity",aes(fill = colour)) +
    scale_fill_manual(labels = c("Less than baseline", "More than baseline"), values=c(negative="firebrick1",positive="green")) +
    ggtitle(maintitle) +
    coord_cartesian(xlim=c(bpmin,bpmax)) +
    theme(axis.text.y =element_text(size=8)) +
    xlab(xlabelwater) + 
    ylab("") +
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.position = "none")
  
  if (log.pc == "PC") 
    xlabelseabed <- "Percent change in annual average mass in/on seabed"
  if (log.pc == "LG") 
    xlabelseabed <- "Log10 change in annual average mass in/on seabed"
  changeseabed2p_df <- data.frame(changeseabed2p[, 1])
  names(changeseabed2p_df)[1] <- "x"
  changeseabed2p_df$y <- rownames(changeseabed2p)
  changeseabed2p_df$colour <- ifelse(changeseabed2p_df$x <= 0, "negative","positive")
  changeseabed2p_df["type"] <- "seabed"  
  changeseabed2p_df$x[is.na(changeseabed2p_df$x)] = 0
  #changeseabed2p_df <- changeseabed2p_df[complete.cases(changeseabed2p_df), ]
  levels_order_seabed <- factor(changeseabed2p_df$y, level = c("Fishery discards & offal", 
                                                       "Corpses", "Sediment bacteria & detritus", "Macrophyte debris", 
                                                       "Porewater ammonia", "Porewater nitrate", "Benthic susp/dep feeders", 
                                                       "Benthic carn/scav feeders"))
  barplot_seabed <-  ggplot(data = changeseabed2p_df,aes(x = x, y = levels_order_seabed)) +
    geom_bar(stat = "identity",aes(fill = colour)) +
    scale_fill_manual(labels = c("Less than baseline", "More than baseline"), values=c(negative="firebrick1",positive="green")) +
    ggtitle(maintitle) +
    coord_cartesian(xlim=c(bpmin,bpmax)) +
    theme(axis.text.y =element_text(size=10)) +
    xlab(xlabelseabed) + 
    ylab("") +
    theme(panel.grid.minor.x = element_blank()) +
    theme(legend.title = element_blank())
  

  dataJoined <- rbind(changewater2p_df,changeseabed2p_df)
  names(dataJoined)[names(dataJoined) == "x"] <- "percentage_change"
  names(dataJoined)[names(dataJoined) == "y"] <- "guild"
  dataJoined = subset(dataJoined, select = -c(colour) )

  figure <- ggarrange(barplot_water, barplot_seabed,
                      ncol = 1, nrow = 2,
                      common.legend = FALSE,
                      align = c("v"))
  
  if(outputType == "PLOT") {
    return(figure)
  } else {
    return(dataJoined)
  }
}
