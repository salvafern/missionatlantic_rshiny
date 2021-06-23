e2e_compare_runs_bar_gg	<- function (selection = "AAM", model1 = NA, use.saved1 = FALSE, 
          results1, model2 = NA, use.saved2 = FALSE, results2, log.pc = "PC", 
          zone = "W", bpmin = (-50), bpmax = (+50), maintitle = "", outputType = "PLOT") 
{
  oo <- options()
  on.exit(options(oo))
  plotted_data <- NULL
  elt <- StrathE2E2:::elt
  if (selection == "AAM") {
    plotted_data <- compareTwoRunsAAM(model1 = model1, 
                                         from.csv1 = use.saved1, results1 = results1, model2 = model2, 
                                         from.csv2 = use.saved2, results2 = results2, log.pc = log.pc, 
                                         zone = zone, bpmin = (bpmin), bpmax = (bpmax), maintitle = maintitle, outputType)
  }
  else if (selection == "CATCH") {
    plotted_data <- compareTwoRunsCatch(model1 = model1, 
                                           from.csv1 = use.saved1, results1 = results1, model2 = model2, 
                                           from.csv2 = use.saved2, results2 = results2, log.pc = log.pc, 
                                           zone = zone, bpmin = (bpmin), bpmax = (bpmax), maintitle = maintitle, outputType)
  }
  else {
    stop("Error: unknown selection '", selection, "' !\n")
  }
  return(plotted_data)
}
