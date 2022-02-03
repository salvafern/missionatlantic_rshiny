# Function to make all adjustments to scenario_model before it is run


makeScenarioAdjustments <- function(scenario_model, input) {
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[1] <-
      scenario_model$data$physics.drivers$so_temp[1] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[2] <-
      scenario_model$data$physics.drivers$so_temp[2] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[3] <-
      scenario_model$data$physics.drivers$so_temp[3] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[4] <-
      scenario_model$data$physics.drivers$so_temp[4] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[5] <-
      scenario_model$data$physics.drivers$so_temp[5] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[6] <-
      scenario_model$data$physics.drivers$so_temp[6] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[7] <-
      scenario_model$data$physics.drivers$so_temp[7] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[8] <-
      scenario_model$data$physics.drivers$so_temp[8] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[9] <-
      scenario_model$data$physics.drivers$so_temp[9] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[10] <-
      scenario_model$data$physics.drivers$so_temp[10] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[11] <-
      scenario_model$data$physics.drivers$so_temp[11] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$so_temp[12] <-
      scenario_model$data$physics.drivers$so_temp[12] + input$temperature
  
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[1] <-
      scenario_model$data$physics.drivers$d_temp[1] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[2] <-
      scenario_model$data$physics.drivers$d_temp[2] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[3] <-
      scenario_model$data$physics.drivers$d_temp[3] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[4] <-
      scenario_model$data$physics.drivers$d_temp[4] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[5] <-
      scenario_model$data$physics.drivers$d_temp[5] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[6] <-
      scenario_model$data$physics.drivers$d_temp[6] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[7] <-
      scenario_model$data$physics.drivers$d_temp[7] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[8] <-
      scenario_model$data$physics.drivers$d_temp[8] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[9] <-
      scenario_model$data$physics.drivers$d_temp[9] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[10] <-
      scenario_model$data$physics.drivers$d_temp[10] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[11] <-
      scenario_model$data$physics.drivers$d_temp[11] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$d_temp[12] <-
      scenario_model$data$physics.drivers$d_temp[12] + input$temperature
  
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[1] <-
      scenario_model$data$physics.drivers$si_temp[1] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[2] <-
      scenario_model$data$physics.drivers$si_temp[2] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[3] <-
      scenario_model$data$physics.drivers$si_temp[3] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[4] <-
      scenario_model$data$physics.drivers$si_temp[4] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[5] <-
      scenario_model$data$physics.drivers$si_temp[5] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[6] <-
      scenario_model$data$physics.drivers$si_temp[6] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[7] <-
      scenario_model$data$physics.drivers$si_temp[7] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[8] <-
      scenario_model$data$physics.drivers$si_temp[8] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[9] <-
      scenario_model$data$physics.drivers$si_temp[9] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[10] <-
      scenario_model$data$physics.drivers$si_temp[10] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[11] <-
      scenario_model$data$physics.drivers$si_temp[11] + input$temperature
  if (!is.null(input$temperature))
    scenario_model$data$physics.drivers$si_temp[12] <-
      scenario_model$data$physics.drivers$si_temp[12] + input$temperature
  #print("Got this far 2")
  # Nutrients
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[1] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[1] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[2] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[2] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[3] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[3] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[4] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[4] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[5] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[5] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[6] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[6] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[7] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[7] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[8] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[8] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[9] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[9] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[10] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[10] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[11] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[11] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$si_atmnitrate[12] <-
      scenario_model$data$chemistry.drivers$si_atmnitrate[12] * input$atmnitrate
  
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[1] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[1] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[2] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[2] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[3] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[3] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[4] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[4] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[5] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[5] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[6] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[6] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[7] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[7] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[8] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[8] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[9] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[9] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[10] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[10] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[11] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[11] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$si_atmammonia[12] <-
      scenario_model$data$chemistry.drivers$si_atmammonia[12] * input$atmammonia
  
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[1] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[1] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[2] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[2] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[3] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[3] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[4] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[4] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[5] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[5] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[6] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[6] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[7] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[7] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[8] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[8] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[9] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[9] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[10] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[10] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[11] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[11] * input$atmnitrate
  if (!is.null(input$atmnitrate))
    scenario_model$data$chemistry.drivers$so_atmnitrate[12] <-
      scenario_model$data$chemistry.drivers$so_atmnitrate[12] * input$atmnitrate
  
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[1] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[1] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[2] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[2] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[3] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[3] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[4] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[4] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[5] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[5] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[6] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[6] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[7] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[7] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[8] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[8] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[9] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[9] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[10] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[10] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[11] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[11] * input$atmammonia
  if (!is.null(input$atmammonia))
    scenario_model$data$chemistry.drivers$so_atmammonia[12] <-
      scenario_model$data$chemistry.drivers$so_atmammonia[12] * input$atmammonia
  
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[1] <-
      scenario_model$data$chemistry.drivers$rivnitrate[1] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[2] <-
      scenario_model$data$chemistry.drivers$rivnitrate[2] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[3] <-
      scenario_model$data$chemistry.drivers$rivnitrate[3] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[4] <-
      scenario_model$data$chemistry.drivers$rivnitrate[4] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[5] <-
      scenario_model$data$chemistry.drivers$rivnitrate[5] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[6] <-
      scenario_model$data$chemistry.drivers$rivnitrate[6] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[7] <-
      scenario_model$data$chemistry.drivers$rivnitrate[7] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[8] <-
      scenario_model$data$chemistry.drivers$rivnitrate[8] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[9] <-
      scenario_model$data$chemistry.drivers$rivnitrate[9] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[10] <-
      scenario_model$data$chemistry.drivers$rivnitrate[10] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[11] <-
      scenario_model$data$chemistry.drivers$rivnitrate[11] * input$rivnitrate
  if (!is.null(input$rivnitrate))
    scenario_model$data$chemistry.drivers$rivnitrate[12] <-
      scenario_model$data$chemistry.drivers$rivnitrate[12] * input$rivnitrate
  
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[1] <-
      scenario_model$data$chemistry.drivers$rivammonia[1] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[2] <-
      scenario_model$data$chemistry.drivers$rivammonia[2] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[3] <-
      scenario_model$data$chemistry.drivers$rivammonia[3] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[4] <-
      scenario_model$data$chemistry.drivers$rivammonia[4] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[5] <-
      scenario_model$data$chemistry.drivers$rivammonia[5] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[6] <-
      scenario_model$data$chemistry.drivers$rivammonia[6] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[7] <-
      scenario_model$data$chemistry.drivers$rivammonia[7] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[8] <-
      scenario_model$data$chemistry.drivers$rivammonia[8] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[9] <-
      scenario_model$data$chemistry.drivers$rivammonia[9] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[10] <-
      scenario_model$data$chemistry.drivers$rivammonia[10] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[11] <-
      scenario_model$data$chemistry.drivers$rivammonia[11] * input$rivammonia
  if (!is.null(input$rivammonia))
    scenario_model$data$chemistry.drivers$rivammonia[12] <-
      scenario_model$data$chemistry.drivers$rivammonia[12] * input$rivammonia
  #print("Got this far 3")
  # Gear Mult
  if (!is.null(input$pelTrawlAct))
    scenario_model$data$fleet.model$gear_mult[1] <-
      input$pelTrawlAct
  if (!is.null(input$sanSpratTrawlAct))
    scenario_model$data$fleet.model$gear_mult[2] <-
      input$sanSpratTrawlAct
  if (!is.null(input$llMackerel))
    scenario_model$data$fleet.model$gear_mult[3] <-
      input$llMackerel
  if (!is.null(input$beamTrawl))
    scenario_model$data$fleet.model$gear_mult[4] <-
      input$beamTrawl
  if (!is.null(input$demersalSeine))
    scenario_model$data$fleet.model$gear_mult[5] <-
      input$demersalSeine
  if (!is.null(input$demersalOtterTrawl))
    scenario_model$data$fleet.model$gear_mult[6] <-
      input$demersalOtterTrawl
  if (!is.null(input$gillLongDemersal))
    scenario_model$data$fleet.model$gear_mult[7] <-
      input$gillLongDemersal
  if (!is.null(input$beamTrawlShrimp))
    scenario_model$data$fleet.model$gear_mult[8] <-
      input$beamTrawlShrimp
  if (!is.null(input$nephropsTrawl))
    scenario_model$data$fleet.model$gear_mult[9] <-
      input$nephropsTrawl
  if (!is.null(input$creels))
    scenario_model$data$fleet.model$gear_mult[10] <-
      input$creels
  if (!is.null(input$molluscDredge))
    scenario_model$data$fleet.model$gear_mult[11] <-
      input$molluscDredge
  if (!is.null(input$whaler))
    scenario_model$data$fleet.model$gear_mult[12] <-
      input$whaler
  #print("Got this far 4")
  # Seabed abrasian
  if (!is.null(input$pelTrawlPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[1] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[1] * input$pelTrawlPlough # TODO Should this be a multiplier on baseline?
  if (!is.null(input$sanSpratTrawlPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[2] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[2] * input$sanSpratTrawlPlough
  if (!is.null(input$llMackerelPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[3] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[3] * input$llMackerelPlough
  if (!is.null(input$beamTrawlPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[4] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[4] * input$beamTrawlPlough
  if (!is.null(input$demersalSeinePlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[5] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[5] * input$demersalSeinePlough
  if (!is.null(input$demersalOtterTrawlPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[6] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[6] * input$demersalOtterTrawlPlough
  if (!is.null(input$gillLongDemersalPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[7] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[7] * input$gillLongDemersalPlough
  if (!is.null(input$beamTrawlShrimpPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[8] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[8] * input$beamTrawlShrimpPlough
  if (!is.null(input$nephropsTrawlPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[9] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[9] * input$nephropsTrawlPlough
  if (!is.null(input$creelsPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[10] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[10] * input$creelsPlough
  if (!is.null(input$molluscDredgePlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[11] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[11] * input$molluscDredgePlough
  if (!is.null(input$whalerPlough))
    scenario_model$data$fleet.model$gear_ploughing_rate[12] <-
      scenario_model$data$fleet.model$gear_ploughing_rate[12] * input$whalerPlough
  #print("Got this far 5")
  # Discard per gear
  if (!is.null(input$pelagicTrawlDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[1] <-
      input$pelagicTrawlDiscard_pel
  if (!is.null(input$sanSpratTrawlDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[2] <-
      input$sanSpratTrawlDiscard_pel
  if (!is.null(input$llMackerelDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[3] <-
      input$llMackerelDiscard_pel
  if (!is.null(input$beamTrawlDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[4] <-
      input$beamTrawlDiscard_pel
  if (!is.null(input$demersalSeineDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[5] <-
      input$demersalSeineDiscard_pel
  if (!is.null(input$demersalOtterTrawlDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[6] <-
      input$demersalOtterTrawlDiscard_pel
  if (!is.null(input$gillLongDemersalDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[7] <-
      input$gillLongDemersalDiscard_pel
  if (!is.null(input$beamTrawlShrimpDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[8] <-
      input$beamTrawlShrimpDiscard_pel
  if (!is.null(input$nephropsTrawlDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[9] <-
      input$nephropsTrawlDiscard_pel
  if (!is.null(input$creelsDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[10] <-
      input$creelsDiscard_pel
  if (!is.null(input$molluscDredgeDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[11] <-
      input$molluscDredgeDiscard_pel
  if (!is.null(input$whalerDiscard_pel))
    scenario_model$data$fleet.model$gear_group_discard$pelagic[12] <-
      input$whalerDiscard_pel
  
  if (!is.null(input$pelagicTrawlDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[1] <-
      input$pelagicTrawlDiscard_dem
  if (!is.null(input$sanSpratTrawlDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[2] <-
      input$sanSpratTrawlDiscard_dem
  if (!is.null(input$llMackerelDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[3] <-
      input$llMackerelDiscard_dem
  if (!is.null(input$beamTrawlDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[4] <-
      input$beamTrawlDiscard_dem
  if (!is.null(input$demersalSeineDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[5] <-
      input$demersalSeineDiscard_dem
  if (!is.null(input$demersalOtterTrawlDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[6] <-
      input$demersalOtterTrawlDiscard_dem
  if (!is.null(input$gillLongDemersalDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[7] <-
      input$gillLongDemersalDiscard_dem
  if (!is.null(input$beamTrawlShrimpDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[8] <-
      input$beamTrawlShrimpDiscard_dem
  if (!is.null(input$nephropsTrawlDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[9] <-
      input$nephropsTrawlDiscard_dem
  if (!is.null(input$creelsDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[10] <-
      input$creelsDiscard_dem
  if (!is.null(input$molluscDredgeDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[11] <-
      input$molluscDredgeDiscard_dem
  if (!is.null(input$whalerDiscard_dem))
    scenario_model$data$fleet.model$gear_group_discard$demersal[12] <-
      input$whalerDiscard_dem
  #print("Got this far 7")
  if (!is.null(input$pelagicTrawlDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[1] <-
      input$pelagicTrawlDiscard_mig
  if (!is.null(input$sanSpratTrawlDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[2] <-
      input$sanSpratTrawlDiscard_mig
  if (!is.null(input$llMackerelDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[3] <-
      input$llMackerelDiscard_mig
  if (!is.null(input$beamTrawlDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[4] <-
      input$beamTrawlDiscard_mig
  if (!is.null(input$demersalSeineDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[5] <-
      input$demersalSeineDiscard_mig
  if (!is.null(input$demersalOtterTrawlDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[6] <-
      input$demersalOtterTrawlDiscard_mig
  if (!is.null(input$gillLongDemersalDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[7] <-
      input$gillLongDemersalDiscard_mig
  if (!is.null(input$beamTrawlShrimpDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[8] <-
      input$beamTrawlShrimpDiscard_mig
  if (!is.null(input$nephropsTrawlDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[9] <-
      input$nephropsTrawlDiscard_mig
  if (!is.null(input$creelsDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[10] <-
      input$creelsDiscard_mig
  if (!is.null(input$molluscDredgeDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[11] <-
      input$molluscDredgeDiscard_mig
  if (!is.null(input$whalerDiscard_mig))
    scenario_model$data$fleet.model$gear_group_discard$migratory[12] <-
      input$whalerDiscard_mig
  #print("Got this far 8")
  if (!is.null(input$pelagicTrawlDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[1] <-
      input$pelagicTrawlDiscard_fb
  if (!is.null(input$sanSpratTrawlDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[2] <-
      input$sanSpratTrawlDiscard_fb
  if (!is.null(input$llMackerelDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[3] <-
      input$llMackerelDiscard_fb
  if (!is.null(input$beamTrawlDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[4] <-
      input$beamTrawlDiscard_fb
  if (!is.null(input$demersalSeineDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[5] <-
      input$demersalSeineDiscard_fb
  if (!is.null(input$demersalOtterTrawlDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[6] <-
      input$demersalOtterTrawlDiscard_fb
  if (!is.null(input$gillLongDemersalDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[7] <-
      input$gillLongDemersalDiscard_fb
  if (!is.null(input$beamTrawlShrimpDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[8] <-
      input$beamTrawlShrimpDiscard_fb
  if (!is.null(input$nephropsTrawlDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[9] <-
      input$nephropsTrawlDiscard_fb
  if (!is.null(input$creelsDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[10] <-
      input$creelsDiscard_fb
  if (!is.null(input$molluscDredgeDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[11] <-
      input$molluscDredgeDiscard_fb
  if (!is.null(input$whalerDiscard_fb))
    scenario_model$data$fleet.model$gear_group_discard$filtben[12] <-
      input$whalerDiscard_fb
  #print("Got this far 9")
  if (!is.null(input$pelagicTrawlDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[1] <-
      input$pelagicTrawlDiscard_cb
  if (!is.null(input$sanSpratTrawlDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[2] <-
      input$sanSpratTrawlDiscard_cb
  if (!is.null(input$llMackerelDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[3] <-
      input$llMackerelDiscard_cb
  if (!is.null(input$beamTrawlDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[4] <-
      input$beamTrawlDiscard_cb
  if (!is.null(input$demersalSeineDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[5] <-
      input$demersalSeineDiscard_cb
  if (!is.null(input$demersalOtterTrawlDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[6] <-
      input$demersalOtterTrawlDiscard_cb
  if (!is.null(input$gillLongDemersalDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[7] <-
      input$gillLongDemersalDiscard_cb
  if (!is.null(input$beamTrawlShrimpDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[8] <-
      input$beamTrawlShrimpDiscard_cb
  if (!is.null(input$nephropsTrawlDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[9] <-
      input$nephropsTrawlDiscard_cb
  if (!is.null(input$creelsDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[10] <-
      input$creelsDiscard_cb
  if (!is.null(input$molluscDredgeDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[11] <-
      input$molluscDredgeDiscard_cb
  if (!is.null(input$whalerDiscard_cb))
    scenario_model$data$fleet.model$gear_group_discard$carnben[12] <-
      input$whalerDiscard_cb
  #print("Got this far 10")
  if (!is.null(input$pelagicTrawlDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[1] <-
      input$pelagicTrawlDiscard_cz
  if (!is.null(input$sanSpratTrawlDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[2] <-
      input$sanSpratTrawlDiscard_cz
  if (!is.null(input$llMackerelDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[3] <-
      input$llMackerelDiscard_cz
  if (!is.null(input$beamTrawlDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[4] <-
      input$beamTrawlDiscard_cz
  if (!is.null(input$demersalSeineDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[5] <-
      input$demersalSeineDiscard_cz
  if (!is.null(input$demersalOtterTrawlDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[6] <-
      input$demersalOtterTrawlDiscard_cz
  if (!is.null(input$gillLongDemersalDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[7] <-
      input$gillLongDemersalDiscard_cz
  if (!is.null(input$beamTrawlShrimpDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[8] <-
      input$beamTrawlShrimpDiscard_cz
  if (!is.null(input$nephropsTrawlDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[9] <-
      input$nephropsTrawlDiscard_cz
  if (!is.null(input$creelsDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[10] <-
      input$creelsDiscard_cz
  if (!is.null(input$molluscDredgeDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[11] <-
      input$molluscDredgeDiscard_cz
  if (!is.null(input$whalerDiscard_cz))
    scenario_model$data$fleet.model$gear_group_discard$carnzoo[12] <-
      input$whalerDiscard_cz
  #print("Got this far 11")
  if (!is.null(input$pelagicTrawlDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[1] <-
      input$pelagicTrawlDiscard_b
  if (!is.null(input$sanSpratTrawlDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[2] <-
      input$sanSpratTrawlDiscard_b
  if (!is.null(input$llMackerelDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[3] <-
      input$llMackerelDiscard_b
  if (!is.null(input$beamTrawlDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[4] <-
      input$beamTrawlDiscard_b
  if (!is.null(input$demersalSeineDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[5] <-
      input$demersalSeineDiscard_b
  if (!is.null(input$demersalOtterTrawlDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[6] <-
      input$demersalOtterTrawlDiscard_b
  if (!is.null(input$gillLongDemersalDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[7] <-
      input$gillLongDemersalDiscard_b
  if (!is.null(input$beamTrawlShrimpDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[8] <-
      input$beamTrawlShrimpDiscard_b
  if (!is.null(input$nephropsTrawlDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[9] <-
      input$nephropsTrawlDiscard_b
  if (!is.null(input$creelsDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[10] <-
      input$creelsDiscard_b
  if (!is.null(input$molluscDredgeDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[11] <-
      input$molluscDredgeDiscard_b
  if (!is.null(input$whalerDiscard_b))
    scenario_model$data$fleet.model$gear_group_discard$bird[12] <-
      input$whalerDiscard_b
  #print("Got this far 12")
  if (!is.null(input$pelagicTrawlDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[1] <-
      input$pelagicTrawlDiscard_s
  if (!is.null(input$sanSpratTrawlDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[2] <-
      input$sanSpratTrawlDiscard_s
  if (!is.null(input$llMackerelDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[3] <-
      input$llMackerelDiscard_s
  if (!is.null(input$beamTrawlDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[4] <-
      input$beamTrawlDiscard_s
  if (!is.null(input$demersalSeineDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[5] <-
      input$demersalSeineDiscard_s
  if (!is.null(input$demersalOtterTrawlDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[6] <-
      input$demersalOtterTrawlDiscard_s
  if (!is.null(input$gillLongDemersalDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[7] <-
      input$gillLongDemersalDiscard_s
  if (!is.null(input$beamTrawlShrimpDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[8] <-
      input$beamTrawlShrimpDiscard_s
  if (!is.null(input$nephropsTrawlDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[9] <-
      input$nephropsTrawlDiscard_s
  if (!is.null(input$creelsDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[10] <-
      input$creelsDiscard_s
  if (!is.null(input$molluscDredgeDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[11] <-
      input$molluscDredgeDiscard_s
  if (!is.null(input$whalerDiscard_s))
    scenario_model$data$fleet.model$gear_group_discard$seal[12] <-
      input$whalerDiscard_s
  #print("Got this far 13")
  if (!is.null(input$pelagicTrawlDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[1] <-
      input$pelagicTrawlDiscard_ceta
  if (!is.null(input$sanSpratTrawlDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[2] <-
      input$sanSpratTrawlDiscard_ceta
  if (!is.null(input$llMackerelDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[3] <-
      input$llMackerelDiscard_ceta
  if (!is.null(input$beamTrawlDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[4] <-
      input$beamTrawlDiscard_ceta
  if (!is.null(input$demersalSeineDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[5] <-
      input$demersalSeineDiscard_ceta
  if (!is.null(input$demersalOtterTrawlDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[6] <-
      input$demersalOtterTrawlDiscard_ceta
  if (!is.null(input$gillLongDemersalDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[7] <-
      input$gillLongDemersalDiscard_ceta
  if (!is.null(input$beamTrawlShrimpDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[8] <-
      input$beamTrawlShrimpDiscard_ceta
  if (!is.null(input$nephropsTrawlDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[9] <-
      input$nephropsTrawlDiscard_ceta
  if (!is.null(input$creelsDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[10] <-
      input$creelsDiscard_ceta
  if (!is.null(input$molluscDredgeDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[11] <-
      input$molluscDredgeDiscard_ceta
  if (!is.null(input$whalerDiscard_ceta))
    scenario_model$data$fleet.model$gear_group_discard$ceta[12] <-
      input$whalerDiscard_ceta
  #print("Got this far 14")
  if (!is.null(input$pelagicTrawlDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[1] <-
      input$pelagicTrawlDiscard_kelp
  if (!is.null(input$sanSpratTrawlDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[2] <-
      input$sanSpratTrawlDiscard_kelp
  if (!is.null(input$llMackerelDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[3] <-
      input$llMackerelDiscard_kelp
  if (!is.null(input$beamTrawlDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[4] <-
      input$beamTrawlDiscard_kelp
  if (!is.null(input$demersalSeineDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[5] <-
      input$demersalSeineDiscard_kelp
  if (!is.null(input$demersalOtterTrawlDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[6] <-
      input$demersalOtterTrawlDiscard_kelp
  if (!is.null(input$gillLongDemersalDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[7] <-
      input$gillLongDemersalDiscard_kelp
  if (!is.null(input$beamTrawlShrimpDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[8] <-
      input$beamTrawlShrimpDiscard_kelp
  if (!is.null(input$nephropsTrawlDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[9] <-
      input$nephropsTrawlDiscard_kelp
  if (!is.null(input$creelsDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[10] <-
      input$creelsDiscard_kelp
  if (!is.null(input$molluscDredgeDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[11] <-
      input$molluscDredgeDiscard_kelp
  if (!is.null(input$whalerDiscard_kelp))
    scenario_model$data$fleet.model$gear_group_discard$kelp[12] <-
      input$whalerDiscard_kelp
  
  # Start of gear distribution per habitat
  if (!is.null(input$percentagePelInRockInput)) {
    ## HAve to do the following because if area is zero they are removed from the interface and remain 0
    if (is.null(input$percentagePelInFineInput)) {
      percentagePelInFineInput <- 0
    } else {
      percentagePelInFineInput <- input$percentagePelInFineInput
    }
    
    if (is.null(input$percentagePelInMedInput)) {
      percentagePelInMedInput <- 0
    } else {
      percentagePelInMedInput <- input$percentagePelInMedInput
    }
    if (is.null(input$percentagePelInCoarseInput)) {
      percentagePelInCoarseInput <- 0
    } else {
      percentagePelInCoarseInput <- input$percentagePelInCoarseInput
    }
    
    if (is.null(input$percentagePelOffFineInput)) {
      percentagePelOffFineInput <- 0
    } else {
      percentagePelOffFineInput <- input$percentagePelOffFineInput
    }
    
    if (is.null(input$percentagePelOffMedInput)) {
      percentagePelOffMedInput <- 0
    } else {
      percentagePelOffMedInput <- input$percentagePelOffMedInput
    }
    if (is.null(input$percentagePelOffCoarseInput)) {
      percentagePelOffCoarseInput <- 0
    } else {
      percentagePelOffCoarseInput <- input$percentagePelOffCoarseInput
    }
    
    newPelInRockProp <-
      (input$inshorePercentagePel * input$percentagePelInRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[1] <-
      newPelInRockProp
    
    newPelInFineProp <-
      (input$inshorePercentagePel * percentagePelInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[1] <-
      newPelInFineProp
    
    newPelInMedProp <-
      (input$inshorePercentagePel * percentagePelInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[1] <-
      newPelInMedProp
    
    newPelInCoarseProp <-
      (input$inshorePercentagePel * percentagePelInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[1] <-
      newPelInCoarseProp
    
    newPelOffRockProp <-
      (input$offshorePercentagePel * input$percentagePelOffRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[1] <-
      newPelOffRockProp
    
    newPelOffFineProp <-
      (input$offshorePercentagePel * percentagePelOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[1] <-
      newPelOffFineProp
    
    newPelOffMedProp <-
      (input$offshorePercentagePel * percentagePelOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[1] <-
      newPelOffMedProp
    
    newPelOffCoarseProp <-
      (input$offshorePercentagePel * percentagePelOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[1] <-
      newPelOffCoarseProp
    
    total <-
      newPelInRockProp + newPelInFineProp + newPelInMedProp + newPelInCoarseProp + newPelOffRockProp + newPelOffFineProp + newPelOffMedProp + newPelOffCoarseProp
    cat("Total for pel: ", total)
  }
  
  if (!is.null(input$percentageSandeelInRockInput)) {
    if (is.null(input$percentageSandeelInFineInput)) {
      percentageSandeelInFineInput <- 0
    } else {
      percentageSandeelInFineInput <- input$percentageSandeelInFineInput
    }
    
    if (is.null(input$percentageSandeelInMedInput)) {
      percentageSandeelInMedInput <- 0
    } else {
      percentageSandeelInMedInput <- input$percentageSandeelInMedInput
    }
    if (is.null(input$percentageSandeelInCoarseInput)) {
      percentageSandeelInCoarseInput <- 0
    } else {
      percentageSandeelInCoarseInput <-
        input$percentageSandeelInCoarseInput
    }
    
    if (is.null(input$percentageSandeelOffFineInput)) {
      percentageSandeelOffFineInput <- 0
    } else {
      percentageSandeelOffFineInput <- input$percentageSandeelOffFineInput
    }
    
    if (is.null(input$percentageSandeelOffMedInput)) {
      percentageSandeelOffMedInput <- 0
    } else {
      percentageSandeelOffMedInput <- input$percentageSandeelOffMedInput
    }
    if (is.null(input$percentageSandeelOffCoarseInput)) {
      percentageSandeelOffCoarseInput <- 0
    } else {
      percentageSandeelOffCoarseInput <-
        input$percentageSandeelOffCoarseInput
    }
    newSandeelInRockProp <-
      (input$inshorePercentageSandeel * input$percentageSandeelInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[2] <-
      newSandeelInRockProp
    
    newSandeelInFineProp <-
      (input$inshorePercentageSandeel * percentageSandeelInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[2] <-
      newSandeelInFineProp
    
    newSandeelInMedProp <-
      (input$inshorePercentageSandeel * percentageSandeelInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[2] <-
      newSandeelInMedProp
    
    newSandeelInCoarseProp <-
      (input$inshorePercentageSandeel * percentageSandeelInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[2] <-
      newSandeelInCoarseProp
    
    newSandeelOffRockProp <-
      (input$offshorePercentageSandeel * input$percentageSandeelOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[2] <-
      newSandeelOffRockProp
    
    newSandeelOffFineProp <-
      (input$offshorePercentageSandeel * percentageSandeelOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[2] <-
      newSandeelOffFineProp
    
    newSandeelOffMedProp <-
      (input$offshorePercentageSandeel * percentageSandeelOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[2] <-
      newSandeelOffMedProp
    
    newSandeelOffCoarseProp <-
      (input$offshorePercentageSandeel * percentageSandeelOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[2] <-
      newSandeelOffCoarseProp
    
    #total <- newSandeelInRockProp + newSandeelInFineProp + newSandeelInMedProp + newSandeelInCoarseProp + newSandeelOffRockProp + newSandeelOffFineProp + newSandeelOffMedProp + newSandeelOffCoarseProp
    #cat("Total for sandeel: ",total)
  }
  
  if (!is.null(input$percentageOtterInRockInput)) {
    if (is.null(input$percentageOtterInFineInput)) {
      percentageOtterInFineInput <- 0
    } else {
      percentageOtterInFineInput <- input$percentageOtterInFineInput
    }
    
    if (is.null(input$percentageOtterInMedInput)) {
      percentageOtterInMedInput <- 0
    } else {
      percentageOtterInMedInput <- input$percentageOtterInMedInput
    }
    if (is.null(input$percentageOtterInCoarseInput)) {
      percentageOtterInCoarseInput <- 0
    } else {
      percentageOtterInCoarseInput <- input$percentageOtterInCoarseInput
    }
    
    if (is.null(input$percentageOtterOffFineInput)) {
      percentageOtterOffFineInput <- 0
    } else {
      percentageOtterOffFineInput <- input$percentageOtterOffFineInput
    }
    
    if (is.null(input$percentageOtterOffMedInput)) {
      percentageOtterOffMedInput <- 0
    } else {
      percentageOtterOffMedInput <- input$percentageOtterOffMedInput
    }
    if (is.null(input$percentageOtterOffCoarseInput)) {
      percentageOtterOffCoarseInput <- 0
    } else {
      percentageOtterOffCoarseInput <- input$percentageOtterOffCoarseInput
    }
    
    
    newOtterInRockProp <-
      (input$inshorePercentageOtter * input$percentageOtterInRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[2] <-
      newOtterInRockProp
    
    newOtterInFineProp <-
      (input$inshorePercentageOtter * percentageOtterInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[2] <-
      newOtterInFineProp
    
    newOtterInMedProp <-
      (input$inshorePercentageOtter * percentageOtterInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[2] <-
      newOtterInMedProp
    
    newOtterInCoarseProp <-
      (input$inshorePercentageOtter * percentageOtterInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[2] <-
      newOtterInCoarseProp
    
    newOtterOffRockProp <-
      (input$offshorePercentageOtter * input$percentageOtterOffRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[2] <-
      newOtterOffRockProp
    
    newOtterOffFineProp <-
      (input$offshorePercentageOtter * percentageOtterOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[2] <-
      newOtterOffFineProp
    
    newOtterOffMedProp <-
      (input$offshorePercentageOtter * percentageOtterOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[2] <-
      newOtterOffMedProp
    
    newOtterOffCoarseProp <-
      (input$offshorePercentageOtter * percentageOtterOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[2] <-
      newOtterOffCoarseProp
    
    #total <- newOtterInRockProp + newOtterInFineProp + newOtterInMedProp + newOtterInCoarseProp + newOtterOffRockProp + newOtterOffFineProp + newOtterOffMedProp + newOtterOffCoarseProp
    #cat("Total for otter: ",total)
  }
  
  if (!is.null(input$percentageLonMackInRockInput)) {
    if (is.null(input$percentageLonMackInFineInput)) {
      percentageLonMackInFineInput <- 0
    } else {
      percentageLonMackInFineInput <- input$percentageLonMackInFineInput
    }
    
    if (is.null(input$percentageLonMackInMedInput)) {
      percentageLonMackInMedInput <- 0
    } else {
      percentageLonMackInMedInput <- input$percentageLonMackInMedInput
    }
    if (is.null(input$percentageLonMackInCoarseInput)) {
      percentageLonMackInCoarseInput <- 0
    } else {
      percentageLonMackInCoarseInput <-
        input$percentageLonMackInCoarseInput
    }
    
    if (is.null(input$percentageLonMackOffFineInput)) {
      percentageLonMackOffFineInput <- 0
    } else {
      percentageLonMackOffFineInput <- input$percentageLonMackOffFineInput
    }
    
    if (is.null(input$percentageLonMackOffMedInput)) {
      percentageLonMackOffMedInput <- 0
    } else {
      percentageLonMackOffMedInput <- input$percentageLonMackOffMedInput
    }
    if (is.null(input$percentageLonMackOffCoarseInput)) {
      percentageLonMackOffCoarseInput <- 0
    } else {
      percentageLonMackOffCoarseInput <-
        input$percentageLonMackOffCoarseInput
    }
    
    newLonMackInRockProp <-
      (input$inshorePercentageLonMack * input$percentageLonMackInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[3] <-
      newLonMackInRockProp
    
    newLonMackInFineProp <-
      (input$inshorePercentageLonMack * percentageLonMackInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[3] <-
      newLonMackInFineProp
    
    newLonMackInMedProp <-
      (input$inshorePercentageLonMack * percentageLonMackInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[3] <-
      newLonMackInMedProp
    
    newLonMackInCoarseProp <-
      (input$inshorePercentageLonMack * percentageLonMackInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[3] <-
      newLonMackInCoarseProp
    
    newLonMackOffRockProp <-
      (input$offshorePercentageLonMack * input$percentageLonMackOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[3] <-
      newLonMackOffRockProp
    
    newLonMackOffFineProp <-
      (input$offshorePercentageLonMack * percentageLonMackOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[3] <-
      newLonMackOffFineProp
    
    newLonMackOffMedProp <-
      (input$offshorePercentageLonMack * percentageLonMackOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[3] <-
      newLonMackOffMedProp
    
    newLonMackOffCoarseProp <-
      (input$offshorePercentageLonMack * percentageLonMackOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[3] <-
      newLonMackOffCoarseProp
    
    #total <- newLonMackInRockProp + newLonMackInFineProp + newLonMackInMedProp + newLonMackInCoarseProp + newLonMackOffRockProp + newLonMackOffFineProp + newLonMackOffMedProp + newLonMackOffCoarseProp
    #cat("Total for lonmack: ",total)
  }
  
  if (!is.null(input$percentageBeamTrawlInRockInput)) {
    if (is.null(input$percentageBeamTrawlInFineInput)) {
      percentageBeamTrawlInFineInput <- 0
    } else {
      percentageBeamTrawlInFineInput <-
        input$percentageBeamTrawlInFineInput
    }
    
    if (is.null(input$percentageBeamTrawlInMedInput)) {
      percentageBeamTrawlInMedInput <- 0
    } else {
      percentageBeamTrawlInMedInput <- input$percentageBeamTrawlInMedInput
    }
    if (is.null(input$percentageBeamTrawlInCoarseInput)) {
      percentageBeamTrawlInCoarseInput <- 0
    } else {
      percentageBeamTrawlInCoarseInput <-
        input$percentageBeamTrawlInCoarseInput
    }
    
    if (is.null(input$percentageBeamTrawlOffFineInput)) {
      percentageBeamTrawlOffFineInput <- 0
    } else {
      percentageBeamTrawlOffFineInput <-
        input$percentageBeamTrawlOffFineInput
    }
    
    if (is.null(input$percentageBeamTrawlOffMedInput)) {
      percentageBeamTrawlOffMedInput <- 0
    } else {
      percentageBeamTrawlOffMedInput <-
        input$percentageBeamTrawlOffMedInput
    }
    if (is.null(input$percentageBeamTrawlOffCoarseInput)) {
      percentageBeamTrawlOffCoarseInput <- 0
    } else {
      percentageBeamTrawlOffCoarseInput <-
        input$percentageBeamTrawlOffCoarseInput
    }
    
    newBeamTrawlInRockProp <-
      (input$inshorePercentageBeamTrawl * input$percentageBeamTrawlInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[4] <-
      newBeamTrawlInRockProp
    
    newBeamTrawlInFineProp <-
      (input$inshorePercentageBeamTrawl * percentageBeamTrawlInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[4] <-
      newBeamTrawlInFineProp
    
    newBeamTrawlInMedProp <-
      (input$inshorePercentageBeamTrawl * percentageBeamTrawlInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[4] <-
      newBeamTrawlInMedProp
    
    newBeamTrawlInCoarseProp <-
      (input$inshorePercentageBeamTrawl * percentageBeamTrawlInCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[4] <-
      newBeamTrawlInCoarseProp
    
    newBeamTrawlOffRockProp <-
      (input$offshorePercentageBeamTrawl * input$percentageBeamTrawlOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[4] <-
      newBeamTrawlOffRockProp
    
    newBeamTrawlOffFineProp <-
      (input$offshorePercentageBeamTrawl * percentageBeamTrawlOffFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[4] <-
      newBeamTrawlOffFineProp
    
    newBeamTrawlOffMedProp <-
      (input$offshorePercentageBeamTrawl * percentageBeamTrawlOffMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[4] <-
      newBeamTrawlOffMedProp
    
    newBeamTrawlOffCoarseProp <-
      (input$offshorePercentageBeamTrawl * percentageBeamTrawlOffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[4] <-
      newBeamTrawlOffCoarseProp
    
    #total <- newBeamTrawlInRockProp + newBeamTrawlInFineProp + newBeamTrawlInMedProp + newBeamTrawlInCoarseProp + newBeamTrawlOffRockProp + newBeamTrawlOffFineProp + newBeamTrawlOffMedProp + newBeamTrawlOffCoarseProp
    #cat("Total for beam trawl: ",total)
  }
  
  if (!is.null(input$percentageDemSeineInRockInput)) {
    if (is.null(input$percentageDemSeineInFineInput)) {
      percentageDemSeineInFineInput <- 0
    } else {
      percentageDemSeineInFineInput <- input$percentageDemSeineInFineInput
    }
    
    if (is.null(input$percentageDemSeineInMedInput)) {
      percentageDemSeineInMedInput <- 0
    } else {
      percentageDemSeineInMedInput <- input$percentageDemSeineInMedInput
    }
    if (is.null(input$percentageDemSeineInCoarseInput)) {
      percentageDemSeineInCoarseInput <- 0
    } else {
      percentageDemSeineInCoarseInput <-
        input$percentageDemSeineInCoarseInput
    }
    
    if (is.null(input$percentageDemSeineOffFineInput)) {
      percentageDemSeineOffFineInput <- 0
    } else {
      percentageDemSeineOffFineInput <-
        input$percentageDemSeineOffFineInput
    }
    
    if (is.null(input$percentageDemSeineOffMedInput)) {
      percentageDemSeineOffMedInput <- 0
    } else {
      percentageDemSeineOffMedInput <- input$percentageDemSeineOffMedInput
    }
    if (is.null(input$percentageDemSeineOffCoarseInput)) {
      percentageDemSeineOffCoarseInput <- 0
    } else {
      percentageDemSeineOffCoarseInput <-
        input$percentageDemSeineOffCoarseInput
    }
    
    
    newDemSeineInRockProp <-
      (input$inshorePercentageDemSeine * input$percentageDemSeineInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[5] <-
      newDemSeineInRockProp
    
    newDemSeineInFineProp <-
      (input$inshorePercentageDemSeine * percentageDemSeineInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[5] <-
      newDemSeineInFineProp
    
    newDemSeineInMedProp <-
      (input$inshorePercentageDemSeine * percentageDemSeineInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[5] <-
      newDemSeineInMedProp
    
    newDemSeineInCoarseProp <-
      (input$inshorePercentageDemSeine * percentageDemSeineInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[5] <-
      newDemSeineInCoarseProp
    
    newDemSeineOffRockProp <-
      (input$offshorePercentageDemSeine * input$percentageDemSeineOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[5] <-
      newDemSeineOffRockProp
    
    newDemSeineOffFineProp <-
      (input$offshorePercentageDemSeine * percentageDemSeineOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[5] <-
      newDemSeineOffFineProp
    
    newDemSeineOffMedProp <-
      (input$offshorePercentageDemSeine * percentageDemSeineOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[5] <-
      newDemSeineOffMedProp
    
    newDemSeineOffCoarseProp <-
      (input$offshorePercentageDemSeine * percentageDemSeineOffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[5] <-
      newDemSeineOffCoarseProp
    
    #total <- newDemSeineInRockProp + newDemSeineInFineProp + newDemSeineInMedProp + newDemSeineInCoarseProp + newDemSeineOffRockProp + newDemSeineOffFineProp + newDemSeineOffMedProp + newDemSeineOffCoarseProp
    #cat("Total for dem seine: ",total)
  }
  
  if (!is.null(input$percentageDemOtterInRockInput)) {
    if (is.null(input$percentageDemOtterInFineInput)) {
      percentageDemOtterInFineInput <- 0
    } else {
      percentageDemOtterInFineInput <- input$percentageDemOtterInFineInput
    }
    
    if (is.null(input$percentageDemOtterInMedInput)) {
      percentageDemOtterInMedInput <- 0
    } else {
      percentageDemOtterInMedInput <- input$percentageDemOtterInMedInput
    }
    if (is.null(input$percentageDemOtterInCoarseInput)) {
      percentageDemOtterInCoarseInput <- 0
    } else {
      percentageDemOtterInCoarseInput <-
        input$percentageDemOtterInCoarseInput
    }
    
    if (is.null(input$percentageDemOtterOffFineInput)) {
      percentageDemOtterOffFineInput <- 0
    } else {
      percentageDemOtterOffFineInput <-
        input$percentageDemOtterOffFineInput
    }
    
    if (is.null(input$percentageDemOtterOffMedInput)) {
      percentageDemOtterOffMedInput <- 0
    } else {
      percentageDemOtterOffMedInput <- input$percentageDemOtterOffMedInput
    }
    if (is.null(input$percentageDemOtterOffCoarseInput)) {
      percentageDemOtterOffCoarseInput <- 0
    } else {
      percentageDemOtterOffCoarseInput <-
        input$percentageDemOtterOffCoarseInput
    }
    
    
    newDemOtterInRockProp <-
      (input$inshorePercentageDemOtter * input$percentageDemOtterInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[6] <-
      newDemOtterInRockProp
    
    newDemOtterInFineProp <-
      (input$inshorePercentageDemOtter * percentageDemOtterInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[6] <-
      newDemOtterInFineProp
    
    newDemOtterInMedProp <-
      (input$inshorePercentageDemOtter * percentageDemOtterInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[6] <-
      newDemOtterInMedProp
    
    newDemOtterInCoarseProp <-
      (input$inshorePercentageDemOtter * percentageDemOtterInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[6] <-
      newDemOtterInCoarseProp
    
    newDemOtterOffRockProp <-
      (input$offshorePercentageDemOtter * input$percentageDemOtterOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[6] <-
      newDemOtterOffRockProp
    
    newDemOtterOffFineProp <-
      (input$offshorePercentageDemOtter * percentageDemOtterOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[6] <-
      newDemOtterOffFineProp
    
    newDemOtterOffMedProp <-
      (input$offshorePercentageDemOtter * percentageDemOtterOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[6] <-
      newDemOtterOffMedProp
    
    newDemOtterOffCoarseProp <-
      (input$offshorePercentageDemOtter * percentageDemOtterOffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[6] <-
      newDemOtterOffCoarseProp
    
    #total <- newDemOtterInRockProp + newDemOtterInFineProp + newDemOtterInMedProp + newDemOtterInCoarseProp + newDemOtterOffRockProp + newDemOtterOffFineProp + newDemOtterOffMedProp + newDemOtterOffCoarseProp
    #cat("Total for dem otter: ",total)
  }
  
  if (!is.null(input$percentageGillNetInRockInput)) {
    if (is.null(input$percentageGillNetInFineInput)) {
      percentageGillNetInFineInput <- 0
    } else {
      percentageGillNetInFineInput <- input$percentageGillNetInFineInput
    }
    
    if (is.null(input$percentageGillNetInMedInput)) {
      percentageGillNetInMedInput <- 0
    } else {
      percentageGillNetInMedInput <- input$percentageGillNetInMedInput
    }
    if (is.null(input$percentageGillNetInCoarseInput)) {
      percentageGillNetInCoarseInput <- 0
    } else {
      percentageGillNetInCoarseInput <-
        input$percentageGillNetInCoarseInput
    }
    
    if (is.null(input$percentageGillNetOffFineInput)) {
      percentageGillNetOffFineInput <- 0
    } else {
      percentageGillNetOffFineInput <- input$percentageGillNetOffFineInput
    }
    
    if (is.null(input$percentageGillNetOffMedInput)) {
      percentageGillNetOffMedInput <- 0
    } else {
      percentageGillNetOffMedInput <- input$percentageGillNetOffMedInput
    }
    if (is.null(input$percentageGillNetOffCoarseInput)) {
      percentageGillNetOffCoarseInput <- 0
    } else {
      percentageGillNetOffCoarseInput <-
        input$percentageGillNetOffCoarseInput
    }
    
    newGillNetInRockProp <-
      (input$inshorePercentageGillNet * input$percentageGillNetInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[7] <-
      newGillNetInRockProp
    
    newGillNetInFineProp <-
      (input$inshorePercentageGillNet * percentageGillNetInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[7] <-
      newGillNetInFineProp
    
    newGillNetInMedProp <-
      (input$inshorePercentageGillNet * percentageGillNetInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[7] <-
      newGillNetInMedProp
    
    newGillNetInCoarseProp <-
      (input$inshorePercentageGillNet * percentageGillNetInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[7] <-
      newGillNetInCoarseProp
    
    newGillNetOffRockProp <-
      (input$offshorePercentageGillNet * input$percentageGillNetOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[7] <-
      newGillNetOffRockProp
    
    newGillNetOffFineProp <-
      (input$offshorePercentageGillNet * percentageGillNetOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[7] <-
      newGillNetOffFineProp
    
    newGillNetOffMedProp <-
      (input$offshorePercentageGillNet * percentageGillNetOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[7] <-
      newGillNetOffMedProp
    
    newGillNetOffCoarseProp <-
      (input$offshorePercentageGillNet * percentageGillNetOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[7] <-
      newGillNetOffCoarseProp
    
    #total <- newGillNetInRockProp + newGillNetInFineProp + newGillNetInMedProp + newGillNetInCoarseProp + newGillNetOffRockProp + newGillNetOffFineProp + newGillNetOffMedProp + newGillNetOffCoarseProp
    #cat("Total for gill net: ",total)
  }
  
  if (!is.null(input$percentageBeamShrimpInRockInput)) {
    if (is.null(input$percentageBeamShrimpInFineInput)) {
      percentageBeamShrimpInFineInput <- 0
    } else {
      percentageBeamShrimpInFineInput <-
        input$percentageBeamShrimpInFineInput
    }
    
    if (is.null(input$percentageBeamShrimpInMedInput)) {
      percentageBeamShrimpInMedInput <- 0
    } else {
      percentageBeamShrimpInMedInput <-
        input$percentageBeamShrimpInMedInput
    }
    if (is.null(input$percentageBeamShrimpInCoarseInput)) {
      percentageBeamShrimpInCoarseInput <- 0
    } else {
      percentageBeamShrimpInCoarseInput <-
        input$percentageBeamShrimpInCoarseInput
    }
    
    if (is.null(input$percentageBeamShrimpOffFineInput)) {
      percentageBeamShrimpOffFineInput <- 0
    } else {
      percentageBeamShrimpOffFineInput <-
        input$percentageBeamShrimpOffFineInput
    }
    
    if (is.null(input$percentageBeamShrimpOffMedInput)) {
      percentageBeamShrimpOffMedInput <- 0
    } else {
      percentageBeamShrimpOffMedInput <-
        input$percentageBeamShrimpOffMedInput
    }
    if (is.null(input$percentageBeamShrimpOffCoarseInput)) {
      percentageBeamShrimpOffCoarseInput <- 0
    } else {
      percentageBeamShrimpOffCoarseInput <-
        input$percentageBeamShrimpOffCoarseInput
    }
    
    newBeamShrimpInRockProp <-
      (input$inshorePercentageBeamShrimp * input$percentageBeamShrimpInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[8] <-
      newBeamShrimpInRockProp
    
    newBeamShrimpInFineProp <-
      (input$inshorePercentageBeamShrimp * percentageBeamShrimpInFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[8] <-
      newBeamShrimpInFineProp
    
    newBeamShrimpInMedProp <-
      (input$inshorePercentageBeamShrimp * percentageBeamShrimpInMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[8] <-
      newBeamShrimpInMedProp
    
    newBeamShrimpInCoarseProp <-
      (input$inshorePercentageBeamShrimp * percentageBeamShrimpInCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[8] <-
      newBeamShrimpInCoarseProp
    
    newBeamShrimpOffRockProp <-
      (input$offshorePercentageBeamShrimp * input$percentageBeamShrimpOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[8] <-
      newBeamShrimpOffRockProp
    
    newBeamShrimpOffFineProp <-
      (input$offshorePercentageBeamShrimp * percentageBeamShrimpOffFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[8] <-
      newBeamShrimpOffFineProp
    
    newBeamShrimpOffMedProp <-
      (input$offshorePercentageBeamShrimp * percentageBeamShrimpOffMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[8] <-
      newBeamShrimpOffMedProp
    
    newBeamShrimpOffCoarseProp <-
      (input$offshorePercentageBeamShrimp * percentageBeamShrimpOffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[8] <-
      newBeamShrimpOffCoarseProp
    
    #total <- newBeamShrimpInRockProp + newBeamShrimpInFineProp + newBeamShrimpInMedProp + newBeamShrimpInCoarseProp + newBeamShrimpOffRockProp + newBeamShrimpOffFineProp + newBeamShrimpOffMedProp + newBeamShrimpOffCoarseProp
    #cat("Total for beamshrimp: ",total)
  }
  
  if (!is.null(input$percentageNephropsTR2InRockInput)) {
    if (is.null(input$percentageNephropsTR2InFineInput)) {
      percentageNephropsTR2InFineInput <- 0
    } else {
      percentageNephropsTR2InFineInput <-
        input$percentageNephropsTR2InFineInput
    }
    
    if (is.null(input$percentageNephropsTR2InMedInput)) {
      percentageNephropsTR2InMedInput <- 0
    } else {
      percentageNephropsTR2InMedInput <-
        input$percentageNephropsTR2InMedInput
    }
    if (is.null(input$percentageNephropsTR2InCoarseInput)) {
      percentageNephropsTR2InCoarseInput <- 0
    } else {
      percentageNephropsTR2InCoarseInput <-
        input$percentageNephropsTR2InCoarseInput
    }
    
    if (is.null(input$percentageNephropsTR2OffFineInput)) {
      percentageNephropsTR2OffFineInput <- 0
    } else {
      percentageNephropsTR2OffFineInput <-
        input$percentageNephropsTR2OffFineInput
    }
    
    if (is.null(input$percentageNephropsTR2OffMedInput)) {
      percentageNephropsTR2OffMedInput <- 0
    } else {
      percentageNephropsTR2OffMedInput <-
        input$percentageNephropsTR2OffMedInput
    }
    if (is.null(input$percentageNephropsTR2OffCoarseInput)) {
      percentageNephropsTR2OffCoarseInput <- 0
    } else {
      percentageNephropsTR2OffCoarseInput <-
        input$percentageNephropsTR2OffCoarseInput
    }
    
    newNephropsTR2InRockProp <-
      (input$inshorePercentageNephropsTR2 * input$percentageNephropsTR2InRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[9] <-
      newNephropsTR2InRockProp
    
    newNephropsTR2InFineProp <-
      (input$inshorePercentageNephropsTR2 * percentageNephropsTR2InFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[9] <-
      newNephropsTR2InFineProp
    
    newNephropsTR2InMedProp <-
      (input$inshorePercentageNephropsTR2 * percentageNephropsTR2InMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[9] <-
      newNephropsTR2InMedProp
    
    newNephropsTR2InCoarseProp <-
      (input$inshorePercentageNephropsTR2 * percentageNephropsTR2InCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[9] <-
      newNephropsTR2InCoarseProp
    
    newNephropsTR2OffRockProp <-
      (
        input$offshorePercentageNephropsTR2 * input$percentageNephropsTR2OffRockInput
      ) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[9] <-
      newNephropsTR2OffRockProp
    
    newNephropsTR2OffFineProp <-
      (input$offshorePercentageNephropsTR2 * percentageNephropsTR2OffFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[9] <-
      newNephropsTR2OffFineProp
    
    newNephropsTR2OffMedProp <-
      (input$offshorePercentageNephropsTR2 * percentageNephropsTR2OffMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[9] <-
      newNephropsTR2OffMedProp
    
    newNephropsTR2OffCoarseProp <-
      (input$offshorePercentageNephropsTR2 * percentageNephropsTR2OffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[9] <-
      newNephropsTR2OffCoarseProp
    
    #total <- newNephropsTR2InRockProp + newNephropsTR2InFineProp + newNephropsTR2InMedProp + newNephropsTR2InCoarseProp + newNephropsTR2OffRockProp + newNephropsTR2OffFineProp + newNephropsTR2OffMedProp + newNephropsTR2OffCoarseProp
    #cat("Total for nephrops2: ",total)
  }
  
  if (!is.null(input$percentageNephropsTR3InRockInput)) {
    if (is.null(input$percentageNephropsTR3InFineInput)) {
      percentageNephropsTR3InFineInput <- 0
    } else {
      percentageNephropsTR3InFineInput <-
        input$percentageNephropsTR3InFineInput
    }
    
    if (is.null(input$percentageNephropsTR3InMedInput)) {
      percentageNephropsTR3InMedInput <- 0
    } else {
      percentageNephropsTR3InMedInput <-
        input$percentageNephropsTR3InMedInput
    }
    if (is.null(input$percentageNephropsTR3InCoarseInput)) {
      percentageNephropsTR3InCoarseInput <- 0
    } else {
      percentageNephropsTR3InCoarseInput <-
        input$percentageNephropsTR3InCoarseInput
    }
    
    if (is.null(input$percentageNephropsTR3OffFineInput)) {
      percentageNephropsTR3OffFineInput <- 0
    } else {
      percentageNephropsTR3OffFineInput <-
        input$percentageNephropsTR3OffFineInput
    }
    
    if (is.null(input$percentageNephropsTR3OffMedInput)) {
      percentageNephropsTR3OffMedInput <- 0
    } else {
      percentageNephropsTR3OffMedInput <-
        input$percentageNephropsTR3OffMedInput
    }
    if (is.null(input$percentageNephropsTR3OffCoarseInput)) {
      percentageNephropsTR3OffCoarseInput <- 0
    } else {
      percentageNephropsTR3OffCoarseInput <-
        input$percentageNephropsTR3OffCoarseInput
    }
    
    newNephropsTR3InRockProp <-
      (input$inshorePercentageNephropsTR3 * input$percentageNephropsTR3InRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[9] <-
      newNephropsTR3InRockProp
    
    newNephropsTR3InFineProp <-
      (input$inshorePercentageNephropsTR3 * percentageNephropsTR3InFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[9] <-
      newNephropsTR3InFineProp
    
    newNephropsTR3InMedProp <-
      (input$inshorePercentageNephropsTR3 * percentageNephropsTR3InMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[9] <-
      newNephropsTR3InMedProp
    
    newNephropsTR3InCoarseProp <-
      (input$inshorePercentageNephropsTR3 * percentageNephropsTR3InCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[9] <-
      newNephropsTR3InCoarseProp
    
    newNephropsTR3OffRockProp <-
      (
        input$offshorePercentageNephropsTR3 * input$percentageNephropsTR3OffRockInput
      ) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[9] <-
      newNephropsTR3OffRockProp
    
    newNephropsTR3OffFineProp <-
      (input$offshorePercentageNephropsTR3 * percentageNephropsTR3OffFineInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[9] <-
      newNephropsTR3OffFineProp
    
    newNephropsTR3OffMedProp <-
      (input$offshorePercentageNephropsTR3 * percentageNephropsTR3OffMedInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[9] <-
      newNephropsTR3OffMedProp
    
    newNephropsTR3OffCoarseProp <-
      (input$offshorePercentageNephropsTR3 * percentageNephropsTR3OffCoarseInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[9] <-
      newNephropsTR3OffCoarseProp
    
    #total <- newNephropsTR3InRockProp + newNephropsTR3InFineProp + newNephropsTR3InMedProp + newNephropsTR3InCoarseProp + newNephropsTR3OffRockProp + newNephropsTR3OffFineProp + newNephropsTR3OffMedProp + newNephropsTR3OffCoarseProp
    #cat("Total for nephrops3: ",total)
  }
  
  if (!is.null(input$percentageCreelsInRockInput)) {
    if (is.null(input$percentageCreelsInFineInput)) {
      percentageCreelsInFineInput <- 0
    } else {
      percentageCreelsInFineInput <- input$percentageCreelsInFineInput
    }
    
    if (is.null(input$percentageCreelsInMedInput)) {
      percentageCreelsInMedInput <- 0
    } else {
      percentageCreelsInMedInput <- input$percentageCreelsInMedInput
    }
    if (is.null(input$percentageCreelsInCoarseInput)) {
      percentageCreelsInCoarseInput <- 0
    } else {
      percentageCreelsInCoarseInput <- input$percentageCreelsInCoarseInput
    }
    
    if (is.null(input$percentageCreelsOffFineInput)) {
      percentageCreelsOffFineInput <- 0
    } else {
      percentageCreelsOffFineInput <- input$percentageCreelsOffFineInput
    }
    
    if (is.null(input$percentageCreelsOffMedInput)) {
      percentageCreelsOffMedInput <- 0
    } else {
      percentageCreelsOffMedInput <- input$percentageCreelsOffMedInput
    }
    if (is.null(input$percentageCreelsOffCoarseInput)) {
      percentageCreelsOffCoarseInput <- 0
    } else {
      percentageCreelsOffCoarseInput <-
        input$percentageCreelsOffCoarseInput
    }
    
    newCreelsInRockProp <-
      (input$inshorePercentageCreels * input$percentageCreelsInRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[10] <-
      newCreelsInRockProp
    
    newCreelsInFineProp <-
      (input$inshorePercentageCreels * percentageCreelsInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[10] <-
      newCreelsInFineProp
    
    newCreelsInMedProp <-
      (input$inshorePercentageCreels * percentageCreelsInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[10] <-
      newCreelsInMedProp
    
    newCreelsInCoarseProp <-
      (input$inshorePercentageCreels * percentageCreelsInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[10] <-
      newCreelsInCoarseProp
    
    newCreelsOffRockProp <-
      (input$offshorePercentageCreels * input$percentageCreelsOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[10] <-
      newCreelsOffRockProp
    
    newCreelsOffFineProp <-
      (input$offshorePercentageCreels * percentageCreelsOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[10] <-
      newCreelsOffFineProp
    
    newCreelsOffMedProp <-
      (input$offshorePercentageCreels * percentageCreelsOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[10] <-
      newCreelsOffMedProp
    
    newCreelsOffCoarseProp <-
      (input$offshorePercentageCreels * percentageCreelsOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[10] <-
      newCreelsOffCoarseProp
    
    #total <- newCreelsInRockProp + newCreelsInFineProp + newCreelsInMedProp + newCreelsInCoarseProp + newCreelsOffRockProp + newCreelsOffFineProp + newCreelsOffMedProp + newCreelsOffCoarseProp
    #cat("Total for creels: ",total)
  }
  
  if (!is.null(input$percentageMolluscInRockInput)) {
    if (is.null(input$percentageMolluscInFineInput)) {
      percentageMolluscInFineInput <- 0
    } else {
      percentageMolluscInFineInput <- input$percentageMolluscInFineInput
    }
    
    if (is.null(input$percentageMolluscInMedInput)) {
      percentageMolluscInMedInput <- 0
    } else {
      percentageMolluscInMedInput <- input$percentageMolluscInMedInput
    }
    if (is.null(input$percentageMolluscInCoarseInput)) {
      percentageMolluscInCoarseInput <- 0
    } else {
      percentageMolluscInCoarseInput <-
        input$percentageMolluscInCoarseInput
    }
    
    if (is.null(input$percentageMolluscOffFineInput)) {
      percentageMolluscOffFineInput <- 0
    } else {
      percentageMolluscOffFineInput <- input$percentageMolluscOffFineInput
    }
    
    if (is.null(input$percentageMolluscOffMedInput)) {
      percentageMolluscOffMedInput <- 0
    } else {
      percentageMolluscOffMedInput <- input$percentageMolluscOffMedInput
    }
    if (is.null(input$percentageMolluscOffCoarseInput)) {
      percentageMolluscOffCoarseInput <- 0
    } else {
      percentageMolluscOffCoarseInput <-
        input$percentageMolluscOffCoarseInput
    }
    
    newMolluscInRockProp <-
      (input$inshorePercentageMollusc * input$percentageMolluscInRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[11] <-
      newMolluscInRockProp
    
    newMolluscInFineProp <-
      (input$inshorePercentageMollusc * percentageMolluscInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[11] <-
      newMolluscInFineProp
    
    newMolluscInMedProp <-
      (input$inshorePercentageMollusc * percentageMolluscInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[11] <-
      newMolluscInMedProp
    
    newMolluscInCoarseProp <-
      (input$inshorePercentageMollusc * percentageMolluscInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[11] <-
      newMolluscInCoarseProp
    
    newMolluscOffRockProp <-
      (input$offshorePercentageMollusc * input$percentageMolluscOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[11] <-
      newMolluscOffRockProp
    
    newMolluscOffFineProp <-
      (input$offshorePercentageMollusc * percentageMolluscOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[11] <-
      newMolluscOffFineProp
    
    newMolluscOffMedProp <-
      (input$offshorePercentageMollusc * percentageMolluscOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[11] <-
      newMolluscOffMedProp
    
    newMolluscOffCoarseProp <-
      (input$offshorePercentageMollusc * percentageMolluscOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[11] <-
      newMolluscOffCoarseProp
    
    #total <- newMolluscInRockProp + newMolluscInFineProp + newMolluscInMedProp + newMolluscInCoarseProp + newMolluscOffRockProp + newMolluscOffFineProp + newMolluscOffMedProp + newMolluscOffCoarseProp
    #cat("Total for Mollusc: ",total)
  }
  
  
  if (!is.null(input$percentageWhalerInRockInput)) {
    if (is.null(input$percentageWhalerInFineInput)) {
      percentageWhalerInFineInput <- 0
    } else {
      percentageWhalerInFineInput <- input$percentageWhalerInFineInput
    }
    
    if (is.null(input$percentageWhalerInMedInput)) {
      percentageWhalerInMedInput <- 0
    } else {
      percentageWhalerInMedInput <- input$percentageWhalerInMedInput
    }
    if (is.null(input$percentageWhalerInCoarseInput)) {
      percentageWhalerInCoarseInput <- 0
    } else {
      percentageWhalerInCoarseInput <- input$percentageWhalerInCoarseInput
    }
    
    if (is.null(input$percentageWhalerOffFineInput)) {
      percentageWhalerOffFineInput <- 0
    } else {
      percentageWhalerOffFineInput <- input$percentageWhalerOffFineInput
    }
    
    if (is.null(input$percentageWhalerOffMedInput)) {
      percentageWhalerOffMedInput <- 0
    } else {
      percentageWhalerOffMedInput <- input$percentageWhalerOffMedInput
    }
    if (is.null(input$percentageWhalerOffCoarseInput)) {
      percentageWhalerOffCoarseInput <- 0
    } else {
      percentageWhalerOffCoarseInput <-
        input$percentageWhalerOffCoarseInput
    }
    
    newWhalerInRockProp <-
      (input$inshorePercentageWhaler * input$percentageWhalerInRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[12] <-
      newWhalerInRockProp
    
    newWhalerInFineProp <-
      (input$inshorePercentageWhaler * percentageWhalerInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[12] <-
      newWhalerInFineProp
    
    newWhalerInMedProp <-
      (input$inshorePercentageWhaler * percentageWhalerInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[12] <-
      newWhalerInMedProp
    
    newWhalerInCoarseProp <-
      (input$inshorePercentageWhaler * percentageWhalerInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[12] <-
      newWhalerInCoarseProp
    
    newWhalerOffRockProp <-
      (input$offshorePercentageWhaler * input$percentageWhalerOffRockInput) /
      10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[12] <-
      newWhalerOffRockProp
    
    newWhalerOffFineProp <-
      (input$offshorePercentageWhaler * percentageWhalerOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[12] <-
      newWhalerOffFineProp
    
    newWhalerOffMedProp <-
      (input$offshorePercentageWhaler * percentageWhalerOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[12] <-
      newWhalerOffMedProp
    
    newWhalerOffCoarseProp <-
      (input$offshorePercentageWhaler * percentageWhalerOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[12] <-
      newWhalerOffCoarseProp
    
    #total <- newWhalerInRockProp + newWhalerInFineProp + newWhalerInMedProp + newWhalerInCoarseProp + newWhalerOffRockProp + newWhalerOffFineProp + newWhalerOffMedProp + newWhalerOffCoarseProp
    #cat("Total for Whaler: ",total)
  }
  
  
  if (!is.null(input$percentageKelpInRockInput)) {
    if (is.null(input$percentageKelpInFineInput)) {
      percentageKelpInFineInput <- 0
    } else {
      percentageKelpInFineInput <- input$percentageKelpInFineInput
    }
    
    if (is.null(input$percentageKelpInMedInput)) {
      percentageKelpInMedInput <- 0
    } else {
      percentageKelpInMedInput <- input$percentageKelpInMedInput
    }
    if (is.null(input$percentageKelpInCoarseInput)) {
      percentageKelpInCoarseInput <- 0
    } else {
      percentageKelpInCoarseInput <- input$percentageKelpInCoarseInput
    }
    
    if (is.null(input$percentageKelpOffFineInput)) {
      percentageKelpOffFineInput <- 0
    } else {
      percentageKelpOffFineInput <- input$percentageKelpOffFineInput
    }
    
    if (is.null(input$percentageKelpOffMedInput)) {
      percentageKelpOffMedInput <- 0
    } else {
      percentageKelpOffMedInput <- input$percentageKelpOffMedInput
    }
    if (is.null(input$percentageKelpOffCoarseInput)) {
      percentageKelpOffCoarseInput <- 0
    } else {
      percentageKelpOffCoarseInput <- input$percentageKelpOffCoarseInput
    }
    newWKelpInRockProp <-
      (input$inshorePercentageKelp * input$percentageKelpInRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s0[12] <-
      newWKelpInRockProp
    
    newWKelpInFineProp <-
      (input$inshorePercentageKelp * percentageKelpInFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s1[12] <-
      newWKelpInFineProp
    
    newWKelpInMedProp <-
      (input$inshorePercentageKelp * percentageKelpInMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s2[12] <-
      newWKelpInMedProp
    
    newWKelpInCoarseProp <-
      (input$inshorePercentageKelp * percentageKelpInCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$s3[12] <-
      newWKelpInCoarseProp
    
    newKelpOffRockProp <-
      (input$offshorePercentageKelp * input$percentageKelpOffRockInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d0[12] <-
      newKelpOffRockProp
    
    newKelpOffFineProp <-
      (input$offshorePercentageKelp * percentageKelpOffFineInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d1[12] <-
      newKelpOffFineProp
    
    newKelpOffMedProp <-
      (input$offshorePercentageKelp * percentageKelpOffMedInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d2[12] <-
      newKelpOffMedProp
    
    newKelpOffCoarseProp <-
      (input$offshorePercentageKelp * percentageKelpOffCoarseInput) / 10000
    scenario_model$data$fleet.model$gear_habitat_activity$d3[12] <-
      newKelpOffCoarseProp
    #total <- newKelpInRockProp + newKelpInFineProp + newKelpInMedProp + newKelpInCoarseProp + newKelpOffRockProp + newKelpOffFineProp + newKelpOffMedProp + newKelpOffCoarseProp
    #cat("Total for Kelp: ",total)
  }
  
  return(scenario_model)
}