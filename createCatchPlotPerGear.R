# useful function to create plots or UI

createCatchPlotPerGear	<- function( model, results, dsa ) {
  elt <- StrathE2E2:::elt
  data <- elt(model, "data")
  final.year.outputs <- elt(results, "final.year.outputs")
  offshore_catchmat <- elt(final.year.outputs, "offshore_catchmat")
  offshore_discmat <- elt(final.year.outputs, "offshore_discmat")
  offshore_landmat <- elt(final.year.outputs, "offshore_landmat")
  inshore_catchmat <- elt(final.year.outputs, "inshore_catchmat")
  inshore_discmat <- elt(final.year.outputs, "inshore_discmat")
  inshore_landmat <- elt(final.year.outputs, "inshore_landmat")
  mt <- (colnames(offshore_catchmat))[dsa]
  landings_data2plot <- rbind(inshore_landmat[,dsa], offshore_landmat[, dsa])
  discard_data2plot <- rbind(inshore_discmat[, dsa], offshore_discmat[, dsa])
  
  landings_data2plot_df<- data.frame(landings_data2plot)
  discard_data2plot_df <- data.frame(discard_data2plot)
  row.names(landings_data2plot_df) <- c("inshore","offshore")
  row.names(discard_data2plot_df) <- c("inshore","offshore")
  landings_data2plot_df$zone <- rownames(landings_data2plot_df)
  landings_data2plot_df$type <- c("landings")
  discard_data2plot_df$zone <- rownames(discard_data2plot_df)
  discard_data2plot_df$type <- c("discard")
  
  landings_long <- gather(landings_data2plot_df, guild, catch, Planktivorous.fish:Macrophytes, factor_key=TRUE)
  discard_long <- gather(discard_data2plot_df, guild, catch, Planktivorous.fish:Macrophytes, factor_key=TRUE)
  
  combined <- rbind(landings_long, discard_long)
  combined$x <- factor(paste(combined$guild, combined$zone),levels = unique(paste(combined$guild, combined$zone)))
  combined$fill <- factor(paste(combined$zone, combined$type), levels = c("offshore landings", "inshore landings", "inshore discard", "offshore discard")) 
 
  barplot <-  ggplot(combined) +
    geom_col(aes(x = as.numeric(x), y = catch, fill = fill)) +
    scale_x_continuous(breaks = seq(from = 1.5, to = length(unique(combined$x)), by = 2),
                       labels = as.character(unique(combined$guild))) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
      scale_fill_manual(values = c('inshore discard' = "grey", 'offshore discard' = "black",
                                   'offshore landings' = "green", 'inshore landings' = "blue")) +
    ggtitle(mt) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=16)) +
    theme(axis.title=element_text(size=14)) +
    xlab("Guild") + ylab("Catch") +
    theme(panel.grid.minor.x = element_blank()) +
    labs(fill = NULL)
  
  return(barplot)
  }