library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(shinyjs)

# Define UI for miles per gallon app ----
ui <- navbarPage("StrathE2E App",
  theme = shinytheme("cerulean"),
  
  #tabsetPanel(
  # id = "inTabset",
  navbarMenu("Overview",
             
    tabPanel(title = "Model Overview", fluidRow(h4(
      "Overview of StrathE2E model to go here"
    ))),
    tabPanel(title = "Model Setup", fluidRow(h4(
      "Png's to be supplied by Jack here"
    )))),
    tabPanel(title = "Location",   sidebarLayout(
      sidebarPanel(
        selectInput(
          "selectedlocation",
          h4("Location"),
          choices
          = list("North_Sea"),
          selected = "North_Sea"
        ),
        selectInput(
          "selectedVariant",
          h4("Model Variant"),
          choices
          = list("1970-1999"),
          selected = "1970-1999"
        ),
        sliderInput(
          "year",
          "Year:",
          min = 1,
          max = 50,
          value = 5,
          width = "100%"
        ),
        
        actionButton("runBaseline", "Run Baseline Model"),
        width = 3
      ),
      #Main Panel: plot map here in the future
      mainPanel (
      )
    )),
    navbarMenu("Parameters",
            tabPanel(title = "Temperature",
              fluidRow(column(
                 width = 5,
                 offset = 2,
                 wellPanel(
                   sliderInput(
                     "temperature",
                     "Temperature to add:",
                     min = -3,
                     max = 3,
                     value = 0.5,
                     width = "100%"
                   ),
                   helpText("Additional temperature")
                 ),
               ))),
            tabPanel(title = "Nutrients",
                     fluidRow(column(
                       width = 5,
                       offset = 2,
                       wellPanel(
                         sliderInput(
                           "si_atmnitrate",
                           "SI ATM Nitrate:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("SI ATM Nitrate")
                       ),
                       wellPanel(
                         sliderInput(
                           "si_atmammonia",
                           "SI Atm Ammonia:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("Additional SI Atm Ammonia")
                       ),
                       wellPanel(
                         sliderInput(
                           "so_atmnitrate",
                           "SO ATM Nitrate:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("Additional SO ATM Nitrate")
                       ),
                       wellPanel(
                         sliderInput(
                           "so_atmammonia",
                           "SO Atm Ammonia:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("Additional SO Atm Ammonia")
                       ),
                       wellPanel(
                         sliderInput(
                           "rivnitrate",
                           "River Nitrate:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("Additional River Nitrate")
                       ),
                       wellPanel(
                         sliderInput(
                           "rivammonia",
                           "River Ammonia:",
                           min = 0,
                           max = 50,
                           value = 0,
                           width = "100%"
                         ),
                         helpText("Additional River Ammonia")
                       ),
                     ))),
                  tabPanel(title = "Fishing Activity",
                     fluidRow(
                       column(
                         width = 5,
                         wellPanel(
                           sliderInput(
                             "pelTrawlAct",
                             "Pelagic Trawl+Seine activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Pelagic Trawl+Seine activity help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "sanSpratTrawlAct",
                             "Sandeel sprat trawl activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Sandeel sprat trawl activity help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "llMackerel",
                             "Longline mackerel activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Longline mackerel activity help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "beamTrawl",
                             "Beam Trawl BT1+BT2 activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Beam Trawl BT1+BT2 activity help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "demersalSeine",
                             "Demersal Seine activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Demersal Seine activity help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "demersalOtterTrawl",
                             "Demersal Otter Trawl TR1 activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Demersal Otter Trawl TR1 help notes here")
                         )
                       ),
                       column(
                         width = 5,
                         wellPanel(
                           sliderInput(
                             "gillLongDemersal",
                             "Gill Nets+Longline demersal activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Gill Nets+Longline demersal activity notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "beamTrawlShrimp",
                             "Beam Trawl shrimp activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Beam Trawl shrimp activity notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "nephropsTrawl",
                             "Nephrops Trawl TR2 activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Nephrops Trawl TR2 activity notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "creels",
                             "Creels activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Creels activity notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "molluscDredge",
                             "Mollusc Dredge activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Mollusc Dredge activity notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "whaler",
                             "Whaler activity:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Whaler activity notes here")
                         )
                       )
                     )),
            tabPanel(title = "Seabed Abrasion",
                     fluidRow(
                       column(
                         width = 5,
                         wellPanel(
                           sliderInput(
                             "pelTrawlPlough",
                             "Pelagic Trawl+Seine seabed abrasion:",
                             min = 0,
                             max = 0.0,
                             value = 0.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Pelagic Trawl+Seine seabed abrasion help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "sanSpratTrawlPlough",
                             "Sandeel sprat trawl seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Sandeel sprat trawl seabed abrasion help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "llMackerelPlough",
                             "Longline mackerel seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Longline mackerel seabed abrasion help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "beamTrawlPlough",
                             "Beam Trawl BT1+BT2 seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Beam Trawl BT1+BT2 seabed abrasion help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "demersalSeinePlough",
                             "Demersal Seine seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Demersal Seine seabed abrasion help notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "demersalOtterTrawlPlough",
                             "Demersal Otter Trawl TR1 seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Demersal Otter Trawl TR1 seabed abrasion help notes here")
                         )
                       ),
                       column(
                         width = 5,
                         wellPanel(
                           sliderInput(
                             "gillLongDemersalPlough",
                             "Gill Nets+Longline demersal seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Gill Nets+Longline demersal seabed abrasion notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "beamTrawlShrimpPlough",
                             "Beam Trawl shrimp seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Beam Trawl shrimp seabed abrasion notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "nephropsTrawlPlough",
                             "Nephrops Trawl TR2 seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Nephrops Trawl TR2 seabed abrasion notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "creelsPlough",
                             "Creels seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Creels seabed abrasion notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "molluscDredgePlough",
                             "Mollusc Dredge seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Mollusc Dredge seabed abrasion notes here")
                         ),
                         wellPanel(
                           sliderInput(
                             "whalerPlough",
                             "Whaler seabed abrasion:",
                             min = 0,
                             max = 2.0,
                             value = 1.0,
                             step = 0.2,
                             width = "100%"
                           ),
                           helpText("Whaler seabed abrasion notes here")
                         )
                       )
                     )),
            tabPanel(title = "Guild Discard Per Gear",   sidebarLayout(
              sidebarPanel(
                selectInput(
                  "selectedParameter",
                  h4("Guilds"),
                  choices
                  = list(
                    "Pelagic",
                    "Demersal",
                    "Migratory",
                    "Filtben",
                    "Carnben",
                    "Carnzoo",
                    "Bird",
                    "Seal",
                    "Ceta",
                    "Kelp"
                  ),
                  selected = "Pelagic"
                ),
                width = 3
              ),
              mainPanel (uiOutput("ui"))
            )),
            tabPanel(title = "Run Scenario",
                     actionButton("runScenario", "Run Scenario")
            )),
  navbarMenu("Plots",
    tabPanel(
      title = "Baseline versus Scenario plots",
      h3("Compare baseline and scenario"),
      # actionButton("runScenario", "Run Model"),
      fluidRow(column(
        6,
        h3("Baseline"),
        plotOutput("baselinePlot")
      ),
      column(
        6,
        h3("Scenario"),
        plotOutput("scenarioPlot")
      )),
      fluidRow(
        column(
          6,
          h3("Baseline output"),
          useShinyjs(),
          div(id="dwnbutton_b", 
              downloadButton("downloadData_baseline1", "Download Baseline output", disabled = "disabled")
          )
        ),
        column(
          6,
          h3("Scenario output"),
          useShinyjs(),
          div(id="dwnbutton_s", 
              downloadButton("downloadData_scenario1", "Download Scenario output", disabled = "disabled")
          )
        )
      )
    ),
    tabPanel(title = "Yield curves", fluidRow(h4(
      "Add info here about yield curves - maybe this will become sub menu under plots tab using navbar"
    ))))
)

server <- function(input, output) {

  output$ui <- renderUI({
    switch(
      input$selectedParameter,
      "Pelagic" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_pel",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_pel",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_pel",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_pel",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_pel",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_pel",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_pel",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_pel",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_pel",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_pel",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_pel",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_pel",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Demersal" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_dem",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_dem",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_dem",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_dem",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_dem",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_dem",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_dem",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_dem",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_dem",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_dem",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_dem",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_dem",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Migratory" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_mig",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_mig",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_mig",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_mig",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_mig",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_mig",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_mig",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_mig",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_mig",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_mig",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_mig",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_mig",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Filtben" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_fb",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_fb",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_fb",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_fb",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_fb",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_fb",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_fb",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_fb",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_fb",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_fb",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_fb",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_fb",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Carnben" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_cb",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_cb",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_cb",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_cb",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_cb",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_cb",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_cb",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_cb",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_cb",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_cb",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_cb",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_cb",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Carnzoo" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_cz",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_cz",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_cz",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_cz",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_cz",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_cz",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_cz",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_cz",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_cz",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_cz",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_cz",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_cz",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Bird" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_b",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_b",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_b",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_b",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_b",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_b",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_b",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_b",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_b",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_b",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_b",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_b",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Seal" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_s",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_s",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_s",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_s",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_s",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_s",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_s",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_s",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_s",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_s",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_s",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_s",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Ceta" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_ceta",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_ceta",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_ceta",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_ceta",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_ceta",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_ceta",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_ceta",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_ceta",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_ceta",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_ceta",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_ceta",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_ceta",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      ),
      "Kelp" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard_kelp",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_kelp",
              "San spart trawl discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_kelp",
              "llMackerel Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_kelp",
              "Beam Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_kelp",
              "Demersal Seine Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal Seine discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_kelp",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_kelp",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_kelp",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_kelp",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_kelp",
              "Creels Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_kelp",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_kelp",
              "Whaler Discard:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Whaler discard notes here")
          )
        )
      )
    )
  })
  
  observeEvent(input$runBaseline, {
    showModal(modalDialog("Please wait whilst model runs baseline. See baseline plot in plots tab once run completed", footer = NULL))
    # Run baseline
    model <<- e2e_read(input$selectedlocation, input$selectedVariant)
    View(model)

    results_baseline <-
      e2e_run(model, nyears = input$year, csv.output = TRUE)
    output$baselinePlot <-
      renderPlot({
        e2e_plot_ts(model, results_baseline)
      })
    removeModal()
    resultDirBaseline <- toString(model$setup$resultsdir)
    output$downloadData_baseline1 <- downloadHandler(
      filename = function() {  'baseline.tar' },
      content = function(file) {
        tar(file,resultDirBaseline)
      }
    )
    if(!is.null(model)){
      enable("downloadData_baseline1")
      runjs("$('#dwnbutton_b').removeAttr('title');")
    }else{
      disable("downloadData_baseline1")
      runjs("$('#dwnbutton_b').attr('title', 'Data not available');")
    }
  })

  observeEvent(input$runScenario, {
    showModal(modalDialog("Please wait whilst model runs scenario .... once completed plots available on plots tab", footer = NULL))
    # Run scenario
    model <- e2e_read(input$selectedlocation, input$selectedVariant)
    scenario_model <- model
    # Temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[1] <- scenario_model$data$physics.drivers$so_temp[1] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[2] <- scenario_model$data$physics.drivers$so_temp[2] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[3] <- scenario_model$data$physics.drivers$so_temp[3] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[4] <- scenario_model$data$physics.drivers$so_temp[4] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[5] <- scenario_model$data$physics.drivers$so_temp[5] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[6] <- scenario_model$data$physics.drivers$so_temp[6] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[7] <- scenario_model$data$physics.drivers$so_temp[7] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[8] <- scenario_model$data$physics.drivers$so_temp[8] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[9] <- scenario_model$data$physics.drivers$so_temp[9] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[10] <- scenario_model$data$physics.drivers$so_temp[10] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[11] <- scenario_model$data$physics.drivers$so_temp[11] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$so_temp[12] <- scenario_model$data$physics.drivers$so_temp[12] + input$temperature

    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[1] <- scenario_model$data$physics.drivers$d_temp[1] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[2] <- scenario_model$data$physics.drivers$d_temp[2] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[3] <- scenario_model$data$physics.drivers$d_temp[3] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[4] <- scenario_model$data$physics.drivers$d_temp[4] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[5] <- scenario_model$data$physics.drivers$d_temp[5] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[6] <- scenario_model$data$physics.drivers$d_temp[6] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[7] <- scenario_model$data$physics.drivers$d_temp[7] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[8] <- scenario_model$data$physics.drivers$d_temp[8] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[9] <- scenario_model$data$physics.drivers$d_temp[9] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[10] <- scenario_model$data$physics.drivers$d_temp[10] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[11] <- scenario_model$data$physics.drivers$d_temp[11] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$d_temp[12] <- scenario_model$data$physics.drivers$d_temp[12] + input$temperature

    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[1] <- scenario_model$data$physics.drivers$si_temp[1] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[2] <- scenario_model$data$physics.drivers$si_temp[2] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[3] <- scenario_model$data$physics.drivers$si_temp[3] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[4] <- scenario_model$data$physics.drivers$si_temp[4] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[5] <- scenario_model$data$physics.drivers$si_temp[5] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[6] <- scenario_model$data$physics.drivers$si_temp[6] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[7] <- scenario_model$data$physics.drivers$si_temp[7] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[8] <- scenario_model$data$physics.drivers$si_temp[8] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[9] <- scenario_model$data$physics.drivers$si_temp[9] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[10] <- scenario_model$data$physics.drivers$si_temp[10] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[11] <- scenario_model$data$physics.drivers$si_temp[11] + input$temperature
    if (!is.null(input$temperature)) scenario_model$data$physics.drivers$si_temp[12] <- scenario_model$data$physics.drivers$si_temp[12] + input$temperature    

    # Nutrients 
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[1] <- scenario_model$data$chemistry.drivers$si_atmnitrate[1]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[2] <- scenario_model$data$chemistry.drivers$si_atmnitrate[2]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[3] <- scenario_model$data$chemistry.drivers$si_atmnitrate[3]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[4] <- scenario_model$data$chemistry.drivers$si_atmnitrate[4]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[5] <- scenario_model$data$chemistry.drivers$si_atmnitrate[5]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[6] <- scenario_model$data$chemistry.drivers$si_atmnitrate[6]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[7] <- scenario_model$data$chemistry.drivers$si_atmnitrate[7]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[8] <- scenario_model$data$chemistry.drivers$si_atmnitrate[8]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[9] <- scenario_model$data$chemistry.drivers$si_atmnitrate[9]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[10] <- scenario_model$data$chemistry.drivers$si_atmnitrate[10]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[11] <- scenario_model$data$chemistry.drivers$si_atmnitrate[11]*input$si_atmnitrate
    if (!is.null(input$si_atmnitrate)) scenario_model$data$chemistry.drivers$si_atmnitrate[12] <- scenario_model$data$chemistry.drivers$si_atmnitrate[12]*input$si_atmnitrate

    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[1] <- scenario_model$data$chemistry.drivers$si_atmammonia[1]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[2] <- scenario_model$data$chemistry.drivers$si_atmammonia[2]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[3] <- scenario_model$data$chemistry.drivers$si_atmammonia[3]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[4] <- scenario_model$data$chemistry.drivers$si_atmammonia[4]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[5] <- scenario_model$data$chemistry.drivers$si_atmammonia[5]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[6] <- scenario_model$data$chemistry.drivers$si_atmammonia[6]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[7] <- scenario_model$data$chemistry.drivers$si_atmammonia[7]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[8] <- scenario_model$data$chemistry.drivers$si_atmammonia[8]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[9] <- scenario_model$data$chemistry.drivers$si_atmammonia[9]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[10] <- scenario_model$data$chemistry.drivers$si_atmammonia[10]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[11] <- scenario_model$data$chemistry.drivers$si_atmammonia[11]*input$si_atmammonia
    if (!is.null(input$si_atmammonia)) scenario_model$data$chemistry.drivers$si_atmammonia[12] <- scenario_model$data$chemistry.drivers$si_atmammonia[12]*input$si_atmammonia

    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[1] <- scenario_model$data$chemistry.drivers$so_atmnitrate[1]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[2] <- scenario_model$data$chemistry.drivers$so_atmnitrate[2]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[3] <- scenario_model$data$chemistry.drivers$so_atmnitrate[3]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[4] <- scenario_model$data$chemistry.drivers$so_atmnitrate[4]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[5] <- scenario_model$data$chemistry.drivers$so_atmnitrate[5]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[6] <- scenario_model$data$chemistry.drivers$so_atmnitrate[6]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[7] <- scenario_model$data$chemistry.drivers$so_atmnitrate[7]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[8] <- scenario_model$data$chemistry.drivers$so_atmnitrate[8]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[9] <- scenario_model$data$chemistry.drivers$so_atmnitrate[9]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[10] <- scenario_model$data$chemistry.drivers$so_atmnitrate[10]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[11] <- scenario_model$data$chemistry.drivers$so_atmnitrate[11]*input$so_atmnitrate
    if (!is.null(input$so_atmnitrate)) scenario_model$data$chemistry.drivers$so_atmnitrate[12] <- scenario_model$data$chemistry.drivers$so_atmnitrate[12]*input$so_atmnitrate
    
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[1] <- scenario_model$data$chemistry.drivers$so_atmammonia[1]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[2] <- scenario_model$data$chemistry.drivers$so_atmammonia[2]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[3] <- scenario_model$data$chemistry.drivers$so_atmammonia[3]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[4] <- scenario_model$data$chemistry.drivers$so_atmammonia[4]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[5] <- scenario_model$data$chemistry.drivers$so_atmammonia[5]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[6] <- scenario_model$data$chemistry.drivers$so_atmammonia[6]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[7] <- scenario_model$data$chemistry.drivers$so_atmammonia[7]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[8] <- scenario_model$data$chemistry.drivers$so_atmammonia[8]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[9] <- scenario_model$data$chemistry.drivers$so_atmammonia[9]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[10] <- scenario_model$data$chemistry.drivers$so_atmammonia[10]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[11] <- scenario_model$data$chemistry.drivers$so_atmammonia[11]*input$so_atmammonia
    if (!is.null(input$so_atmammonia)) scenario_model$data$chemistry.drivers$so_atmammonia[12] <- scenario_model$data$chemistry.drivers$so_atmammonia[12]*input$so_atmammonia
    
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[1] <- scenario_model$data$chemistry.drivers$rivnitrate[1]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[2] <- scenario_model$data$chemistry.drivers$rivnitrate[2]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[3] <- scenario_model$data$chemistry.drivers$rivnitrate[3]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[4] <- scenario_model$data$chemistry.drivers$rivnitrate[4]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[5] <- scenario_model$data$chemistry.drivers$rivnitrate[5]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[6] <- scenario_model$data$chemistry.drivers$rivnitrate[6]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[7] <- scenario_model$data$chemistry.drivers$rivnitrate[7]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[8] <- scenario_model$data$chemistry.drivers$rivnitrate[8]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[9] <- scenario_model$data$chemistry.drivers$rivnitrate[9]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[10] <- scenario_model$data$chemistry.drivers$rivnitrate[10]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[11] <- scenario_model$data$chemistry.drivers$rivnitrate[11]*input$rivnitrate
    if (!is.null(input$rivnitrate)) scenario_model$data$chemistry.drivers$rivnitrate[12] <- scenario_model$data$chemistry.drivers$rivnitrate[12]*input$rivnitrate
    
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[1] <- scenario_model$data$chemistry.drivers$rivammonia[1]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[2] <- scenario_model$data$chemistry.drivers$rivammonia[2]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[3] <- scenario_model$data$chemistry.drivers$rivammonia[3]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[4] <- scenario_model$data$chemistry.drivers$rivammonia[4]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[5] <- scenario_model$data$chemistry.drivers$rivammonia[5]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[6] <- scenario_model$data$chemistry.drivers$rivammonia[6]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[7] <- scenario_model$data$chemistry.drivers$rivammonia[7]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[8] <- scenario_model$data$chemistry.drivers$rivammonia[8]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[9] <- scenario_model$data$chemistry.drivers$rivammonia[9]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[10] <- scenario_model$data$chemistry.drivers$rivammonia[10]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[11] <- scenario_model$data$chemistry.drivers$rivammonia[11]*input$rivammonia
    if (!is.null(input$rivammonia)) scenario_model$data$chemistry.drivers$rivammonia[12] <- scenario_model$data$chemistry.drivers$rivammonia[12]*input$rivammonia
    
        # Gear Mult
    if (!is.null(input$pelTrawlAct)) scenario_model$data$fleet.model$gear_mult[1] <- input$pelTrawlAct
    if (!is.null(input$sanSpratTrawlAct)) scenario_model$data$fleet.model$gear_mult[2] <- input$sanSpratTrawlAct
    if (!is.null(input$llMackerel)) scenario_model$data$fleet.model$gear_mult[3] <- input$llMackerel
    if (!is.null(input$beamTrawl)) scenario_model$data$fleet.model$gear_mult[4] <- input$beamTrawl
    if (!is.null(input$demersalSeine)) scenario_model$data$fleet.model$gear_mult[5] <- input$demersalSeine
    if (!is.null(input$demersalOtterTrawl)) scenario_model$data$fleet.model$gear_mult[6] <- input$demersalOtterTrawl
    if (!is.null(input$gillLongDemersal)) scenario_model$data$fleet.model$gear_mult[7] <- input$gillLongDemersal
    if (!is.null(input$beamTrawlShrimp)) scenario_model$data$fleet.model$gear_mult[8] <- input$beamTrawlShrimp
    if (!is.null(input$nephropsTrawl)) scenario_model$data$fleet.model$gear_mult[9] <- input$nephropsTrawl
    if (!is.null(input$creels)) scenario_model$data$fleet.model$gear_mult[10] <- input$creels
    if (!is.null(input$molluscDredge)) scenario_model$data$fleet.model$gear_mult[11] <- input$molluscDredge
    if (!is.null(input$whaler)) scenario_model$data$fleet.model$gear_mult[12] <- input$whaler

        # Seabed abrasian
    if (!is.null(input$pelTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[1] <- scenario_model$data$fleet.model$gear_ploughing_rate[1]*input$pelTrawlPlough # TODO Should this be a multiplier on baseline?
    if (!is.null(input$sanSpratTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[2] <- scenario_model$data$fleet.model$gear_ploughing_rate[2]*input$sanSpratTrawlPlough
    if (!is.null(input$llMackerelPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[3] <- scenario_model$data$fleet.model$gear_ploughing_rate[3]*input$llMackerelPlough
    if (!is.null(input$beamTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[4] <- scenario_model$data$fleet.model$gear_ploughing_rate[4]*input$beamTrawlPlough
    if (!is.null(input$demersalSeinePlough)) scenario_model$data$fleet.model$gear_ploughing_rate[5] <- scenario_model$data$fleet.model$gear_ploughing_rate[5]*input$demersalSeinePlough
    if (!is.null(input$demersalOtterTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[6] <- scenario_model$data$fleet.model$gear_ploughing_rate[6]*input$demersalOtterTrawlPlough
    if (!is.null(input$gillLongDemersalPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[7] <- scenario_model$data$fleet.model$gear_ploughing_rate[7]*input$gillLongDemersalPlough
    if (!is.null(input$beamTrawlShrimpPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[8] <- scenario_model$data$fleet.model$gear_ploughing_rate[8]*input$beamTrawlShrimpPlough
    if (!is.null(input$nephropsTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[9] <- scenario_model$data$fleet.model$gear_ploughing_rate[9]*input$nephropsTrawlPlough
    if (!is.null(input$creelsPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[10] <- scenario_model$data$fleet.model$gear_ploughing_rate[10]*input$creelsPlough
    if (!is.null(input$molluscDredgePlough)) scenario_model$data$fleet.model$gear_ploughing_rate[11] <- scenario_model$data$fleet.model$gear_ploughing_rate[11]*input$molluscDredgePlough
    if (!is.null(input$whalerPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[12] <- scenario_model$data$fleet.model$gear_ploughing_rate[12]*input$whalerPlough

        # Discard per gear
    if (!is.null(input$pelagicTrawlDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][1] <-  scenario_model$data$fleet.model$gear_group_discard[1][1]*input$pelagicTrawlDiscard_pel
    if (!is.null(input$sanSpratTrawlDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][2] <- scenario_model$data$fleet.model$gear_group_discard[1][2]*input$sanSpratTrawlDiscard_pel
    if (!is.null(input$llMackerelDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][3] <- scenario_model$data$fleet.model$gear_group_discard[1][3]*input$llMackerelDiscard_pel
    if (!is.null(input$beamTrawlDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][4] <- scenario_model$data$fleet.model$gear_group_discard[1][4]*input$beamTrawlDiscard_pel
    if (!is.null(input$demersalSeineDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][5] <- scenario_model$data$fleet.model$gear_group_discard[1][5]*input$demersalSeineDiscard_pel
    if (!is.null(input$demersalOtterTrawlDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][6] <- scenario_model$data$fleet.model$gear_group_discard[1][6]*input$demersalOtterTrawlDiscard_pel
    if (!is.null(input$gillLongDemersalDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][7] <- scenario_model$data$fleet.model$gear_group_discard[1][7]*input$gillLongDemersalDiscard_pel
    if (!is.null(input$beamTrawlShrimpDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][8] <- scenario_model$data$fleet.model$gear_group_discard[1][8]*input$beamTrawlShrimpDiscard_pel
    if (!is.null(input$nephropsTrawlDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][9] <- scenario_model$data$fleet.model$gear_group_discard[1][9]*input$nephropsTrawlDiscard_pel
    if (!is.null(input$creelsDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][10] <- scenario_model$data$fleet.model$gear_group_discard[1][10]*input$creelsDiscard_pel
    if (!is.null(input$molluscDredgeDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][11] <- scenario_model$data$fleet.model$gear_group_discard[1][11]*input$molluscDredgeDiscard_pel
    if (!is.null(input$whalerDiscard_pel)) scenario_model$data$fleet.model$gear_group_discard[1][12] <- scenario_model$data$fleet.model$gear_group_discard[1][12]*input$whalerDiscard_pel
    
    if (!is.null(input$pelagicTrawlDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][1] <-  scenario_model$data$fleet.model$gear_group_discard[2][1]*input$pelagicTrawlDiscard_dem
    if (!is.null(input$sanSpratTrawlDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][2] <- scenario_model$data$fleet.model$gear_group_discard[2][2]*input$sanSpratTrawlDiscard_dem
    if (!is.null(input$llMackerelDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][3] <- scenario_model$data$fleet.model$gear_group_discard[2][3]*input$llMackerelDiscard_dem
    if (!is.null(input$beamTrawlDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][4] <- scenario_model$data$fleet.model$gear_group_discard[2][4]*input$beamTrawlDiscard_dem
    if (!is.null(input$demersalSeineDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][5] <- scenario_model$data$fleet.model$gear_group_discard[2][5]*input$demersalSeineDiscard_dem
    if (!is.null(input$demersalOtterTrawlDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][6] <- scenario_model$data$fleet.model$gear_group_discard[2][6]*input$demersalOtterTrawlDiscard_dem
    if (!is.null(input$gillLongDemersalDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][7] <- scenario_model$data$fleet.model$gear_group_discard[2][7]*input$gillLongDemersalDiscard_dem
    if (!is.null(input$beamTrawlShrimpDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][8] <- scenario_model$data$fleet.model$gear_group_discard[2][8]*input$beamTrawlShrimpDiscard_dem
    if (!is.null(input$nephropsTrawlDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][9] <- scenario_model$data$fleet.model$gear_group_discard[2][9]*input$nephropsTrawlDiscard_dem
    if (!is.null(input$creelsDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][10] <- scenario_model$data$fleet.model$gear_group_discard[2][10]*input$creelsDiscard_dem
    if (!is.null(input$molluscDredgeDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][11] <- scenario_model$data$fleet.model$gear_group_discard[2][11]*input$molluscDredgeDiscard_dem
    if (!is.null(input$whalerDiscard_dem)) scenario_model$data$fleet.model$gear_group_discard[2][12] <- scenario_model$data$fleet.model$gear_group_discard[2][12]*input$whalerDiscard_dem
    
    if (!is.null(input$pelagicTrawlDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][1] <-  scenario_model$data$fleet.model$gear_group_discard[3][1]*input$pelagicTrawlDiscard_mig
    if (!is.null(input$sanSpratTrawlDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][2] <- scenario_model$data$fleet.model$gear_group_discard[3][2]*input$sanSpratTrawlDiscard_mig
    if (!is.null(input$llMackerelDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][3] <- scenario_model$data$fleet.model$gear_group_discard[3][3]*input$llMackerelDiscard_mig
    if (!is.null(input$beamTrawlDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][4] <- scenario_model$data$fleet.model$gear_group_discard[3][4]*input$beamTrawlDiscard_mig
    if (!is.null(input$demersalSeineDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][5] <- scenario_model$data$fleet.model$gear_group_discard[3][5]*input$demersalSeineDiscard_mig
    if (!is.null(input$demersalOtterTrawlDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][6] <- scenario_model$data$fleet.model$gear_group_discard[3][6]*input$demersalOtterTrawlDiscard_mig
    if (!is.null(input$gillLongDemersalDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][7] <- scenario_model$data$fleet.model$gear_group_discard[3][7]*input$gillLongDemersalDiscard_mig
    if (!is.null(input$beamTrawlShrimpDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][8] <- scenario_model$data$fleet.model$gear_group_discard[3][8]*input$beamTrawlShrimpDiscard_mig
    if (!is.null(input$nephropsTrawlDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][9] <- scenario_model$data$fleet.model$gear_group_discard[3][9]*input$nephropsTrawlDiscard_mig
    if (!is.null(input$creelsDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][10] <- scenario_model$data$fleet.model$gear_group_discard[3][10]*input$creelsDiscard_mig
    if (!is.null(input$molluscDredgeDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][11] <- scenario_model$data$fleet.model$gear_group_discard[3][11]*input$molluscDredgeDiscard_mig
    if (!is.null(input$whalerDiscard_mig)) scenario_model$data$fleet.model$gear_group_discard[3][12] <- scenario_model$data$fleet.model$gear_group_discard[3][12]*input$whalerDiscard_mig
    
    if (!is.null(input$pelagicTrawlDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][1] <-  scenario_model$data$fleet.model$gear_group_discard[4][1]*input$pelagicTrawlDiscard_fb
    if (!is.null(input$sanSpratTrawlDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][2] <- scenario_model$data$fleet.model$gear_group_discard[4][2]*input$sanSpratTrawlDiscard_fb
    if (!is.null(input$llMackerelDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][3] <- scenario_model$data$fleet.model$gear_group_discard[4][3]*input$llMackerelDiscard_fb
    if (!is.null(input$beamTrawlDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][4] <- scenario_model$data$fleet.model$gear_group_discard[4][4]*input$beamTrawlDiscard_fb
    if (!is.null(input$demersalSeineDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][5] <- scenario_model$data$fleet.model$gear_group_discard[4][5]*input$demersalSeineDiscard_fb
    if (!is.null(input$demersalOtterTrawlDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][6] <- scenario_model$data$fleet.model$gear_group_discard[4][6]*input$demersalOtterTrawlDiscard_fb
    if (!is.null(input$gillLongDemersalDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][7] <- scenario_model$data$fleet.model$gear_group_discard[4][7]*input$gillLongDemersalDiscard_fb
    if (!is.null(input$beamTrawlShrimpDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][8] <- scenario_model$data$fleet.model$gear_group_discard[4][8]*input$beamTrawlShrimpDiscard_fb
    if (!is.null(input$nephropsTrawlDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][9] <- scenario_model$data$fleet.model$gear_group_discard[4][9]*input$nephropsTrawlDiscard_fb
    if (!is.null(input$creelsDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][10] <- scenario_model$data$fleet.model$gear_group_discard[4][10]*input$creelsDiscard_fb
    if (!is.null(input$molluscDredgeDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][11] <- scenario_model$data$fleet.model$gear_group_discard[4][11]*input$molluscDredgeDiscard_fb
    if (!is.null(input$whalerDiscard_fb)) scenario_model$data$fleet.model$gear_group_discard[4][12] <- scenario_model$data$fleet.model$gear_group_discard[4][12]*input$whalerDiscard_fb
    
    if (!is.null(input$pelagicTrawlDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][1] <-  scenario_model$data$fleet.model$gear_group_discard[5][1]*input$pelagicTrawlDiscard_cb
    if (!is.null(input$sanSpratTrawlDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][2] <- scenario_model$data$fleet.model$gear_group_discard[5][2]*input$sanSpratTrawlDiscard_cb
    if (!is.null(input$llMackerelDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][3] <- scenario_model$data$fleet.model$gear_group_discard[5][3]*input$llMackerelDiscard_cb
    if (!is.null(input$beamTrawlDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][4] <- scenario_model$data$fleet.model$gear_group_discard[5][4]*input$beamTrawlDiscard_cb
    if (!is.null(input$demersalSeineDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][5] <- scenario_model$data$fleet.model$gear_group_discard[5][5]*input$demersalSeineDiscard_cb
    if (!is.null(input$demersalOtterTrawlDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][6] <- scenario_model$data$fleet.model$gear_group_discard[5][6]*input$demersalOtterTrawlDiscard_cb
    if (!is.null(input$gillLongDemersalDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][7] <- scenario_model$data$fleet.model$gear_group_discard[5][7]*input$gillLongDemersalDiscard_cb
    if (!is.null(input$beamTrawlShrimpDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][8] <- scenario_model$data$fleet.model$gear_group_discard[5][8]*input$beamTrawlShrimpDiscard_cb
    if (!is.null(input$nephropsTrawlDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][9] <- scenario_model$data$fleet.model$gear_group_discard[5][9]*input$nephropsTrawlDiscard_cb
    if (!is.null(input$creelsDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][10] <- scenario_model$data$fleet.model$gear_group_discard[5][10]*input$creelsDiscard_cb
    if (!is.null(input$molluscDredgeDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][11] <- scenario_model$data$fleet.model$gear_group_discard[5][11]*input$molluscDredgeDiscard_cb
    if (!is.null(input$whalerDiscard_cb)) scenario_model$data$fleet.model$gear_group_discard[5][12] <- scenario_model$data$fleet.model$gear_group_discard[5][12]*input$whalerDiscard_cb
    
    if (!is.null(input$pelagicTrawlDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][1] <-  scenario_model$data$fleet.model$gear_group_discard[6][1]*input$pelagicTrawlDiscard_cz
    if (!is.null(input$sanSpratTrawlDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][2] <- scenario_model$data$fleet.model$gear_group_discard[6][2]*input$sanSpratTrawlDiscard_cz
    if (!is.null(input$llMackerelDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][3] <- scenario_model$data$fleet.model$gear_group_discard[6][3]*input$llMackerelDiscard_cz
    if (!is.null(input$beamTrawlDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][4] <- scenario_model$data$fleet.model$gear_group_discard[6][4]*input$beamTrawlDiscard_cz
    if (!is.null(input$demersalSeineDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][5] <- scenario_model$data$fleet.model$gear_group_discard[6][5]*input$demersalSeineDiscard_cz
    if (!is.null(input$demersalOtterTrawlDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][6] <- scenario_model$data$fleet.model$gear_group_discard[6][6]*input$demersalOtterTrawlDiscard_cz
    if (!is.null(input$gillLongDemersalDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][7] <- scenario_model$data$fleet.model$gear_group_discard[6][7]*input$gillLongDemersalDiscard_cz
    if (!is.null(input$beamTrawlShrimpDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][8] <- scenario_model$data$fleet.model$gear_group_discard[6][8]*input$beamTrawlShrimpDiscard_cz
    if (!is.null(input$nephropsTrawlDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][9] <- scenario_model$data$fleet.model$gear_group_discard[6][9]*input$nephropsTrawlDiscard_cz
    if (!is.null(input$creelsDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][10] <- scenario_model$data$fleet.model$gear_group_discard[6][10]*input$creelsDiscard_cz
    if (!is.null(input$molluscDredgeDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][11] <- scenario_model$data$fleet.model$gear_group_discard[6][11]*input$molluscDredgeDiscard_cz
    if (!is.null(input$whalerDiscard_cz)) scenario_model$data$fleet.model$gear_group_discard[6][12] <- scenario_model$data$fleet.model$gear_group_discard[6][12]*input$whalerDiscard_cz
    
    if (!is.null(input$pelagicTrawlDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][1] <-  scenario_model$data$fleet.model$gear_group_discard[7][1]*input$pelagicTrawlDiscard_b
    if (!is.null(input$sanSpratTrawlDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][2] <- scenario_model$data$fleet.model$gear_group_discard[7][2]*input$sanSpratTrawlDiscard_b
    if (!is.null(input$llMackerelDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][3] <- scenario_model$data$fleet.model$gear_group_discard[7][3]*input$llMackerelDiscard_b
    if (!is.null(input$beamTrawlDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][4] <- scenario_model$data$fleet.model$gear_group_discard[7][4]*input$beamTrawlDiscard_b
    if (!is.null(input$demersalSeineDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][5] <- scenario_model$data$fleet.model$gear_group_discard[7][5]*input$demersalSeineDiscard_b
    if (!is.null(input$demersalOtterTrawlDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][6] <- scenario_model$data$fleet.model$gear_group_discard[7][6]*input$demersalOtterTrawlDiscard_b
    if (!is.null(input$gillLongDemersalDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][7] <- scenario_model$data$fleet.model$gear_group_discard[7][7]*input$gillLongDemersalDiscard_b
    if (!is.null(input$beamTrawlShrimpDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][8] <- scenario_model$data$fleet.model$gear_group_discard[7][8]*input$beamTrawlShrimpDiscard_b
    if (!is.null(input$nephropsTrawlDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][9] <- scenario_model$data$fleet.model$gear_group_discard[7][9]*input$nephropsTrawlDiscard_b
    if (!is.null(input$creelsDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][10] <- scenario_model$data$fleet.model$gear_group_discard[7][10]*input$creelsDiscard_b
    if (!is.null(input$molluscDredgeDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][11] <- scenario_model$data$fleet.model$gear_group_discard[7][11]*input$molluscDredgeDiscard_b
    if (!is.null(input$whalerDiscard_b)) scenario_model$data$fleet.model$gear_group_discard[7][12] <- scenario_model$data$fleet.model$gear_group_discard[7][12]*input$whalerDiscard_b
    
    if (!is.null(input$pelagicTrawlDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][1] <-  scenario_model$data$fleet.model$gear_group_discard[8][1]*input$pelagicTrawlDiscard_s
    if (!is.null(input$sanSpratTrawlDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][2] <- scenario_model$data$fleet.model$gear_group_discard[8][2]*input$sanSpratTrawlDiscard_s
    if (!is.null(input$llMackerelDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][3] <- scenario_model$data$fleet.model$gear_group_discard[8][3]*input$llMackerelDiscard_s
    if (!is.null(input$beamTrawlDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][4] <- scenario_model$data$fleet.model$gear_group_discard[8][4]*input$beamTrawlDiscard_s
    if (!is.null(input$demersalSeineDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][5] <- scenario_model$data$fleet.model$gear_group_discard[8][5]*input$demersalSeineDiscard_s
    if (!is.null(input$demersalOtterTrawlDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][6] <- scenario_model$data$fleet.model$gear_group_discard[8][6]*input$demersalOtterTrawlDiscard_s
    if (!is.null(input$gillLongDemersalDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][7] <- scenario_model$data$fleet.model$gear_group_discard[8][7]*input$gillLongDemersalDiscard_s
    if (!is.null(input$beamTrawlShrimpDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][8] <- scenario_model$data$fleet.model$gear_group_discard[8][8]*input$beamTrawlShrimpDiscard_s
    if (!is.null(input$nephropsTrawlDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][9] <- scenario_model$data$fleet.model$gear_group_discard[8][9]*input$nephropsTrawlDiscard_s
    if (!is.null(input$creelsDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][10] <- scenario_model$data$fleet.model$gear_group_discard[8][10]*input$creelsDiscard_s
    if (!is.null(input$molluscDredgeDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][11] <- scenario_model$data$fleet.model$gear_group_discard[8][11]*input$molluscDredgeDiscard_s
    if (!is.null(input$whalerDiscard_s)) scenario_model$data$fleet.model$gear_group_discard[8][12] <- scenario_model$data$fleet.model$gear_group_discard[8][12]*input$whalerDiscard_s
    
    if (!is.null(input$pelagicTrawlDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][1] <-  scenario_model$data$fleet.model$gear_group_discard[9][1]*input$pelagicTrawlDiscard_ceta
    if (!is.null(input$sanSpratTrawlDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][2] <- scenario_model$data$fleet.model$gear_group_discard[9][2]*input$sanSpratTrawlDiscard_ceta
    if (!is.null(input$llMackerelDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][3] <- scenario_model$data$fleet.model$gear_group_discard[9][3]*input$llMackerelDiscard_ceta
    if (!is.null(input$beamTrawlDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][4] <- scenario_model$data$fleet.model$gear_group_discard[9][4]*input$beamTrawlDiscard_ceta
    if (!is.null(input$demersalSeineDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][5] <- scenario_model$data$fleet.model$gear_group_discard[9][5]*input$demersalSeineDiscard_ceta
    if (!is.null(input$demersalOtterTrawlDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][6] <- scenario_model$data$fleet.model$gear_group_discard[9][6]*input$demersalOtterTrawlDiscard_ceta
    if (!is.null(input$gillLongDemersalDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][7] <- scenario_model$data$fleet.model$gear_group_discard[9][7]*input$gillLongDemersalDiscard_ceta
    if (!is.null(input$beamTrawlShrimpDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][8] <- scenario_model$data$fleet.model$gear_group_discard[9][8]*input$beamTrawlShrimpDiscard_ceta
    if (!is.null(input$nephropsTrawlDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][9] <- scenario_model$data$fleet.model$gear_group_discard[9][9]*input$nephropsTrawlDiscard_ceta
    if (!is.null(input$creelsDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][10] <- scenario_model$data$fleet.model$gear_group_discard[9][10]*input$creelsDiscard_ceta
    if (!is.null(input$molluscDredgeDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][11] <- scenario_model$data$fleet.model$gear_group_discard[9][11]*input$molluscDredgeDiscard_ceta
    if (!is.null(input$whalerDiscard_ceta)) scenario_model$data$fleet.model$gear_group_discard[9][12] <- scenario_model$data$fleet.model$gear_group_discard[9][12]*input$whalerDiscard_ceta
    
    if (!is.null(input$pelagicTrawlDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][1] <-  scenario_model$data$fleet.model$gear_group_discard[10][1]*input$pelagicTrawlDiscard_kelp
    if (!is.null(input$sanSpratTrawlDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][2] <- scenario_model$data$fleet.model$gear_group_discard[10][2]*input$sanSpratTrawlDiscard_kelp
    if (!is.null(input$llMackerelDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][3] <- scenario_model$data$fleet.model$gear_group_discard[10][3]*input$llMackerelDiscard_kelp
    if (!is.null(input$beamTrawlDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][4] <- scenario_model$data$fleet.model$gear_group_discard[10][4]*input$beamTrawlDiscard_kelp
    if (!is.null(input$demersalSeineDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][5] <- scenario_model$data$fleet.model$gear_group_discard[10][5]*input$demersalSeineDiscard_kelp
    if (!is.null(input$demersalOtterTrawlDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][6] <- scenario_model$data$fleet.model$gear_group_discard[10][6]*input$demersalOtterTrawlDiscard_kelp
    if (!is.null(input$gillLongDemersalDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][7] <- scenario_model$data$fleet.model$gear_group_discard[10][7]*input$gillLongDemersalDiscard_kelp
    if (!is.null(input$beamTrawlShrimpDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][8] <- scenario_model$data$fleet.model$gear_group_discard[10][8]*input$beamTrawlShrimpDiscard_kelp
    if (!is.null(input$nephropsTrawlDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][9] <- scenario_model$data$fleet.model$gear_group_discard[10][9]*input$nephropsTrawlDiscard_kelp
    if (!is.null(input$creelsDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][10] <- scenario_model$data$fleet.model$gear_group_discard[10][10]*input$creelsDiscard_kelp
    if (!is.null(input$molluscDredgeDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][11] <- scenario_model$data$fleet.model$gear_group_discard[10][11]*input$molluscDredgeDiscard_kelp
    if (!is.null(input$whalerDiscard_kelp)) scenario_model$data$fleet.model$gear_group_discard[9][12] <- scenario_model$data$fleet.model$gear_group_discard[10][12]*input$whalerDiscard_kelp
    
    results_scenario <-
      e2e_run(scenario_model,
              nyears = input$year,
              csv.output = TRUE)
    output$scenarioPlot <-
      renderPlot({
        e2e_plot_ts(scenario_model, results_scenario)
      })
    removeModal()
    resultDirScenario <- toString(scenario_model$setup$resultsdir)
    output$downloadData_scenario1 <- downloadHandler(
      filename = function() {  'scenario.tar' },
      content = function(file) {
        tar(file,resultDirScenario)
      }
    )
    if(!is.null(model)){
      enable("downloadData_scenario1")
      runjs("$('#dwnbutton_s').removeAttr('title');")
    }else{
      disable("downloadData_scenario1")
      runjs("$('#dwnbutton_s').attr('title', 'Data not available');")
    }
  })
}

shinyApp(ui, server)