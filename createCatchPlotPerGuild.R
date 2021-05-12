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
  offshore_data2plot <- rbind(offshore_discmat[dsa, ], 
                              offshore_landmat[dsa, ])
  inshore_data2plot <- rbind(inshore_discmat[dsa, ], inshore_landmat[dsa, 
  ])

  offshore_data2plot_df<- data.frame(offshore_data2plot)
  inshore_data2plot_df <- data.frame(inshore_data2plot)
  row.names(offshore_data2plot_df) <- c("offshore landings","offshore discards")
  row.names(inshore_data2plot_df) <- c("inshore landings","inshore discards")
  offshore_data2plot_df$type <- rownames(offshore_data2plot_df)
  inshore_data2plot_df$type <- rownames(inshore_data2plot_df)

  offshore_long <- gather(offshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  inshore_long <- gather(inshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  
  combined <- rbind(offshore_long,inshore_long)
  
  barplot <- ggplot(data=combined, aes(x=gear,y=catch, fill=type)) +
    geom_bar( stat="identity", position ="dodge", alpha=.8) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ggtitle(mt) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=16)) +
    theme(axis.title=element_text(size=14)) +
    xlab("Gear") + ylab("Catch")
  return(barplot)
  }
  if(dsa == 12){
  mt <- "All guilds combined"
  offshore_data2plot <- rbind(colSums(offshore_discmat), colSums(offshore_landmat))
  inshore_data2plot <- rbind(colSums(inshore_discmat), colSums(inshore_landmat))

  offshore_data2plot_df<- data.frame(offshore_data2plot)
  inshore_data2plot_df <- data.frame(inshore_data2plot)
  row.names(offshore_data2plot_df) <- c("offshore landings","offshore discards")
  row.names(inshore_data2plot_df) <- c("inshore landings","inshore discards")
  offshore_data2plot_df$type <- rownames(offshore_data2plot_df)
  inshore_data2plot_df$type <- rownames(inshore_data2plot_df)
  
  offshore_long <- gather(offshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  inshore_long <- gather(inshore_data2plot_df, gear, catch, Pelagic_Trawl.Seine:Whaler, factor_key=TRUE)
  
  combined <- rbind(offshore_long,inshore_long)
  
  barplot <- ggplot(data=combined, aes(x=gear,y=catch, fill=type)) +
    geom_bar( stat="identity", position ="dodge", alpha=.8) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    ggtitle(mt) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=16)) +
    theme(axis.title=element_text(size=14)) +
    xlab("Gear") + ylab("Catch")
  return(barplot)
  }
}