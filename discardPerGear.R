# Function to create discard per gear UI


createDiscardPerGearUI <- function(model,input) {
  gear1 <- model$data$fleet.model$gear_labels[1]
  gear2 <- model$data$fleet.model$gear_labels[2]
  gear3 <- model$data$fleet.model$gear_labels[3]
  gear4 <- model$data$fleet.model$gear_labels[4]
  gear5 <- model$data$fleet.model$gear_labels[5]
  gear6 <- model$data$fleet.model$gear_labels[6]
  gear7 <- model$data$fleet.model$gear_labels[7]
  gear8 <- model$data$fleet.model$gear_labels[8]
  gear9 <- model$data$fleet.model$gear_labels[9]
  gear10 <- model$data$fleet.model$gear_labels[10]
  gear11 <- model$data$fleet.model$gear_labels[11]
  gear12 <- model$data$fleet.model$gear_labels[12]
  switch(
    input$selectedParameter,
    "Planktivorous fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_pel",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_pel",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_pel",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_pel",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_pel",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_pel",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_pel",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_pel",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_pel",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_pel",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_pel",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_pel_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_pel",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$pelagic[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_pel_reset", "Reset")
                )
      )
    ),
    "Demersal fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_dem",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_dem",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_dem",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_dem",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_dem",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_dem_reset", "Reset")
                ),
                #column(
                #  width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_dem",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_dem",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_dem",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_dem",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_dem",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_dem",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_dem_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_dem",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$demersal[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_dem_reset", "Reset")
                )
      )
    ),
    "Migratory fish" = fluidRow(
      #column(
      #  width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_mig",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_mig",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_mig",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_mig",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_mig",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_mig_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_mig",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_mig",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_mig",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_mig",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_mig",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_mig",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_mig_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_mig",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$migratory[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_mig_reset", "Reset")
                )
      )
    ),
    "Suspension/deposit feeding benthos" = fluidRow(
      #       column(
      #         width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_fb",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_fb",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_fb",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_fb",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_fb",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_fb_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_fb",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_fb",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_fb",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_fb",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_fb",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_fb",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_fb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_fb",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$filtben[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_fb_reset", "Reset")
                )
      )
    ),
    "Carnivore/scavenge feeding benthos" = fluidRow(
      #       column(
      #         width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_cb",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_cb",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_cb",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_cb",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_cb",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_cb_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_cb",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_cb",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_cb",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_cb",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_cb",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_cb",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_cb_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_cb",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnben[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_cb_reset", "Reset")
                )
      )
    ),
    "Carnivorous zooplankton (e.g. squids)" = fluidRow(
      #       column(
      #         width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_cz",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_cz",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_cz",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[2],
                    step = 0.1,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_cz",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_cz",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_cz_reset", "Reset")
                  #)
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_cz",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_cz",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_cz",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_cz",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_cz",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_cz",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_cz_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_cz",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$carnzoo[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_cz_reset", "Reset")
                )
      )
    ),
    "Seabirds" = fluidRow(
      #       column(
      #         width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_b",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_b",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_b",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_b",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_b",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_b_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_b",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_b",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_b",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_b",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_b",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_b",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_b_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_b",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$bird[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_b_reset", "Reset")
                )
      )
    ),
    "Pinnipeds (seals)" = fluidRow(
      #        column(
      #          width = 5
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_s",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_s",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_s",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_s",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_s",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_s_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_s",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_s",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_s",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_s",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_s",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_s",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_s_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_s",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$seal[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_s_reset", "Reset")
                )
      )
    ),
    "Cetaceans" = fluidRow(
      #        column(
      #          width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_ceta",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_ceta",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_ceta",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_ceta",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_ceta",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_ceta_reset", "Reset")
                ),
                #        column(
                #          width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_ceta",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_ceta",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_ceta",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_ceta",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_ceta",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_ceta",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_ceta_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_ceta",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$ceta[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_ceta_reset", "Reset")
                )
      )
    ),
    "Macrophytes (kelp)" = fluidRow(
      #        column(
      #          width = 5,
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "pelagicTrawlDiscard_kelp",
                    paste(gear1, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[1],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("pelagicTrawlDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sanSpratTrawlDiscard_kelp",
                    paste(gear2, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[2],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sanSpratTrawlDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "llMackerelDiscard_kelp",
                    paste(gear3, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[3],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("llMackerelDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlDiscard_kelp",
                    paste(gear4, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[4],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "demersalSeineDiscard_kelp",
                    paste(gear5, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[5],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalSeineDiscard_kelp_reset", "Reset")
                ),
                #        column(
                #         width = 5,
                wellPanel(
                  sliderInput(
                    "demersalOtterTrawlDiscard_kelp",
                    paste(gear6, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[6],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("demersalOtterTrawlDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "gillLongDemersalDiscard_kelp",
                    paste(gear7, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[7],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("gillLongDemersalDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "beamTrawlShrimpDiscard_kelp",
                    paste(gear8, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[8],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("beamTrawlShrimpDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "nephropsTrawlDiscard_kelp",
                    paste(gear9, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[9],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("nephropsTrawlDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "creelsDiscard_kelp",
                    paste(gear10, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[10],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("creelsDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "molluscDredgeDiscard_kelp",
                    paste(gear11, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[11],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("molluscDredgeDiscard_kelp_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "whalerDiscard_kelp",
                    paste(gear12, "Discard", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = model$data$fleet.model$gear_group_discard$kelp[12],
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("whalerDiscard_kelp_reset", "Reset")
                )
      )
    )
  )
  
}