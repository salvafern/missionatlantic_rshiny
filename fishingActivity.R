# Function to create fishing activity UI


createFishingActivityUI <- function(model) {
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
    fluidRow(
      # wellPanel(
      #   HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the activity rates of each fishing gear."),
      #   HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model rates; 0.5 means that fishing rates are halved; 2 means that rates are doubled."),
      #   HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Activity rate of each gear is measured in seconds of gear deployment per m<sup>2</sup per day")
      # ),
      column(width = 5,
             wellPanel(style = "overflow-y:scroll; max-height: 600px",
                       wellPanel(
                         sliderInput(
                           "pelTrawlAct",
                           paste(gear1, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "sanSpratTrawlAct",
                           paste(gear2, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "llMackerel",
                           paste(gear3, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "beamTrawl",
                           paste(gear4, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "demersalSeine",
                           paste(gear5, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "demersalOtterTrawl",
                           paste(gear6, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "gillLongDemersal",
                           paste(gear7, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "beamTrawlShrimp",
                           paste(gear8, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "nephropsTrawl",
                           paste(gear9, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "creels",
                           paste(gear10, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "molluscDredge",
                           paste(gear11, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       ),
                       wellPanel(
                         sliderInput(
                           "whaler",
                           paste(gear12, "activity", sep=" "),
                           min = 0,
                           max = 2.0,
                           value = 1.0,
                           step = 0.2,
                           width = "100%"
                         )
                       )
             )
      ),
      column(width = 5,
             wellPanel(
               HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the activity rates of each fishing gear."),
               HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model rates; 0.5 means that fishing rates are halved; 2 means that rates are doubled."),
               HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Activity rate of each gear is measured in seconds of gear deployment per m<sup>2</sup per day")
             )
      )
    )
}