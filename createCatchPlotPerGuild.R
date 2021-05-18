# useful function to create plots or UI

createCatchPlotPerGuild	<- function( model, results, dsa ) {
  elt <- StrathE2E2:::elt
  data <- elt(model, "data")
  fleet.model <- elt(data, "fleet.model")
  gear_codes <- elt(fleet.model, "gear_codes")
  gear_labels <- elt(fleet.model, "gear_labels")
  final.year.outputs <- elt(results, "final.year.outputs")
  offshore_catchmat <- elt(final.year.outputs, "offshore_catchmat")
  offshore_discmat <- elt(final.year.outputs, "offshore_discmat")
  offshore_landmat <- elt(final.year.outputs, "offshore_landmat")
  inshore_catchmat <- elt(final.year.outputs, "inshore_catchmat")
  inshore_discmat <- elt(final.year.outputs, "inshore_discmat")
  inshore_landmat <- elt(final.year.outputs, "inshore_landmat")
  if(dsa < 12){
  mt <- (rownames(offshore_catchmat))[dsa]
  landings_data2plot <- rbind(inshore_landmat[dsa, ],offshore_landmat[dsa, ])
  discard_data2plot <- rbind(inshore_discmat[dsa, ],offshore_discmat[dsa, ])
  #View(inshore_discmat[dsa, ])
  #View(offshore_discmat[dsa, ])
  #View(inshore_landmat[dsa, ])
  #View(offshore_landmat[dsa, ])
  #View(landings_data2plot)
  #View(discard_data2plot)
  
  landings_data2plot_df<- data.frame(landings_data2plot)
  discard_data2plot_df <- data.frame(discard_data2plot)
  row.names(landings_data2plot_df) <- c("inshore","offshore")
  row.names(discard_data2plot_df) <- c("inshore","offshore")
  landings_data2plot_df$zone <- rownames(landings_data2plot_df)
  landings_data2plot_df$type <- c("landings")
  discard_data2plot_df$zone <- rownames(discard_data2plot_df)
  discard_data2plot_df$type <- c("discard")
  #View(landings_data2plot_df)
  #View(discard_data2plot_df)
  
  landings_long <- gather(landings_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  discard_long <- gather(discard_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  
  combined <- rbind(landings_long, discard_long)
  combined$x <- factor(paste(combined$gear, combined$zone),levels = unique(paste(combined$gear, combined$zone)))
  combined$fill <- factor(paste(combined$zone, combined$type), levels = c("offshore landings", "inshore landings", "inshore discard", "offshore discard")) 
  View(combined)
  
   barplot <-  ggplot(combined) +
    geom_col(aes(x = as.numeric(x), y = catch,  fill = fill)) +
    scale_x_continuous(breaks = seq(from = 1.5, to = length(unique(combined$x)), by = 2),
                       labels = as.character(unique(combined$gear)),expand = c(0, 0.5)) +
       theme(axis.text.x = element_text(angle = 45, hjust=1)) +
       scale_fill_manual(values = c('inshore discard' = "grey", 'offshore discard' = "black",
                                  'offshore landings' = "green", 'inshore landings' = "blue")) +
       ggtitle(mt) +
       theme(plot.title = element_text(hjust = 0.5)) +
       theme(plot.title = element_text(size=16)) +
       theme(axis.title=element_text(size=14)) +
       xlab("Gear") + ylab("Catch") #+
       #labs(fill = catch)
  
  return(barplot)
  }
  if(dsa == 12){
  mt <- "All guilds combined"
  offshore_data2plot <- rbind(colSums(offshore_discmat), colSums(offshore_landmat))
  inshore_data2plot <- rbind(colSums(inshore_discmat), colSums(inshore_landmat))
  offshore_data2plot_df<- data.frame(offshore_data2plot)
  inshore_data2plot_df <- data.frame(inshore_data2plot)
  
  row.names(offshore_data2plot_df) <- c("landings","discards")
  row.names(inshore_data2plot_df) <- c("landings","discards")
  offshore_data2plot_df$type <- rownames(offshore_data2plot_df)
  offshore_data2plot_df$zone <- c("offshore")
  inshore_data2plot_df$type <- rownames(inshore_data2plot_df)
  inshore_data2plot_df$zone <- c("inshore")
  
  offshore_long <- gather(offshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  inshore_long <- gather(inshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  
  combined <- rbind(offshore_long,inshore_long)
  combined$x <- factor(paste(combined$gear, combined$zone))
  
  barplot <-  ggplot(combined) +
    geom_col(aes(x = as.numeric(x), y = catch, fill = paste(zone, type))) +
    scale_x_continuous(breaks = seq(from = 1.5, to = length(unique(combined$x)), by = 2),
                       labels = as.character(unique(combined$gear))) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ggtitle(mt) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=16)) +
    theme(axis.title=element_text(size=14)) +
    xlab("Gear") + ylab("Catch")
  return(barplot)
  }
}