# Function to create seabed abrasian UI


createSeabedAbrasianUI <- function(model) {
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
  gear1Plough <- model$data$fleet.model$gear_ploughing_rate[1]
  gear2Plough <- model$data$fleet.model$gear_ploughing_rate[2]
  gear3Plough <- model$data$fleet.model$gear_ploughing_rate[3]
  gear4Plough <- model$data$fleet.model$gear_ploughing_rate[4]
  gear5Plough <- model$data$fleet.model$gear_ploughing_rate[5]
  gear6Plough <- model$data$fleet.model$gear_ploughing_rate[6]
  gear7Plough <- model$data$fleet.model$gear_ploughing_rate[7]
  gear8Plough <- model$data$fleet.model$gear_ploughing_rate[8]
  gear9Plough <- model$data$fleet.model$gear_ploughing_rate[9]
  gear10Plough <- model$data$fleet.model$gear_ploughing_rate[10]
  gear11Plough <- model$data$fleet.model$gear_ploughing_rate[11]
  gear12Plough <- model$data$fleet.model$gear_ploughing_rate[12]
  
  fluidRow(
    column(width = 5,
           wellPanel(style = "overflow-y:scroll; max-height: 600px",
                     if (gear1Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "pelTrawlPlough",
                           paste(gear1, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 0.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear2Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "sanSpratTrawlPlough",
                           paste(gear2, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear3Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "llMackerelPlough",
                           paste(gear3, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear4Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "beamTrawlPlough",
                           paste(gear4, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear5Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "demersalSeinePlough",
                           paste(gear5, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear6Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "demersalOtterTrawlPlough",
                           paste(gear6, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear7Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "gillLongDemersalPlough",
                           paste(gear7, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear8Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "beamTrawlShrimpPlough",
                           paste(gear8, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear9Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "nephropsTrawlPlough",
                           paste(gear9, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear10Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "creelsPlough",
                           paste(gear10, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear11Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "molluscDredgePlough",
                           paste(gear11, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     },
                     if (gear12Plough > 0) {
                       wellPanel(
                         sliderInput(
                           "whalerPlough",
                           paste(gear12, "seabed abrasion", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
                     }
           )
    ),
    column(width = 5,
           wellPanel(
             HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the seabed abrasion rate of each fishing gear."),
             HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model rates; 0.5 means that abrasion rates are halved; 2 means that rates are doubled."),
             HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Seabed abrasion rate of each gear is measured in m<sup>2</sup> of seabed abraded per second of gear deployment."),
             HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">In the model, seabed abrasion re-suspends organic matter and releases nutrients from the sediment, and causes collateral mortality of benthos guilds.")
           )
    )
  )
}