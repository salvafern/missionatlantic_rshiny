library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(shinyjs)

# Define UI for miles per gallon app ----
ui <- navbarPage(
  "StrathE2E App",
  theme = shinytheme("cerulean"),
  navbarMenu(
    "Overview",
    tabPanel(title = "Model Overview",
             fluidRow(
               img(src = "strathe2e_2.jpg", width = '30%'),
               h6(
                 "Schematic showing the North Sea food web model components and driving variables, in relation to physical structures (surface water layer, deep water layer, and sediments)."
               )
             ),
             fluidRow(column(
               8,
               h4("StrathE2E model"),
               p(
                 "StrathE2E is a marine food web model of intermediate complexity which represents a spatially aggregated shelf sea region. The model simulates the fluxes of nutrient (nitrogen) through the ecosystem from dissolved inorganic (nitrate and ammonia), through plankton, benthos and fish, to birds and mammals, its regeneration through excretion and mineralization of detritus in the water column and sediment, and the physical oceanographic, land-sea, and air-sea exchanges across the geographic boundaries. The model incorporates Holling II function relationships between each predator-prey couplet in the food web, and density dependent mortality control of the upper trophic levels (top-levels plus most interior levels). External drivers include sea surface irradiance, temperature, hydrodynamic fluxes, freshwater input, river and atmospheric nitrate and ammonia inputs, ocean boundary nitrate, ammonia and suspended particulate concentrations, and density independent fishery harvesting rates of shellfish, pelagic and demersal fish. Outputs from the model include time-series of the nitrogen mass of all state variables components, fluxes between components due to e.g. feeding, excretion, and exports to the ocean, atmosphere and to fishery landings."
               )
             )))),
    navbarMenu(
      "Setup",
      tabPanel(
        title = "Location",
        h4("Some text about setup to go here"),
        sidebarLayout(
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
            )
          ),
          #Main Panel: plot map here in the future
          mainPanel(fluidRow(h4(
            "Png's to be supplied by Jack here"
          )))
        )
      ),
      tabPanel(
        title = "Temperature",
        h4("Some text about Temperature to go here"),
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
        ))
      ),
      tabPanel(
        title = "Nutrients",
        h4("Some text about Nutrients to go here"),
        fluidRow(
          column(
            width = 5,
            offset = 2,
            wellPanel(
              sliderInput(
                "atmnitrate",
                "Atmospheric Nitrate:",
                min = 0,
                max = 2.0,
                value = 1.0,
                step = 0.2,
                width = "100%"
              ),
              helpText("Atmospheric Nitrate")
            ),
            wellPanel(
              sliderInput(
                "atmammonia",
                "Atmospheric Ammonia:",
                min = 0,
                max = 2.0,
                value = 1.0,
                step = 0.2,
                width = "100%"
              ),
              helpText("Atmospheric Ammonia")
            ),
            wellPanel(
              sliderInput(
                "rivnitrate",
                "River Nitrate:",
                min = 0,
                max = 2.0,
                value = 1.0,
                step = 0.2,
                width = "100%"
              ),
              helpText("Additional River Nitrate")
            ),
            wellPanel(
              sliderInput(
                "rivammonia",
                "River Ammonia:",
                min = 0,
                max = 2.0,
                value = 1.0,
                step = 0.2,
                width = "100%"
              ),
              helpText("Additional River Ammonia")
            ),
          )
        )
      ),
      tabPanel(
        title = "Fishing Activity",
        h4("Some text about fishing activity to go here"),
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
        )
      ),
      tabPanel(
        title = "Seabed Abrasion",
        h4("Some text about seabed abrasion to go here"),
        fluidRow(
          column(
            width = 5,
            conditionalPanel(condition = "input.pelTrawlPlough > 0",
                             wellPanel(
                               sliderInput(
                                 "pelTrawlPlough",
                                 "Pelagic Trawl+Seine seabed abrasion:",
                                 min = 0,
                                 max = 2.0,
                                 value = 0.0,
                                 step = 0.2,
                                 width = "100%"
                               ),
                               helpText("Pelagic Trawl+Seine seabed abrasion help notes here")
                             ),),
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
        )
      ),
      tabPanel(
        title = "Guild Discard Per Gear",
        h4("Some text about discards per gear to go here"),
        sidebarLayout(sidebarPanel(
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
        mainPanel (uiOutput("ui")))
      )
    ),
    navbarMenu("Run",
               tabPanel(
                 title = "Run",
                 h4("Some text about running baseline and scenario to go here"),
                 sliderInput(
                   "year",
                   "Year:",
                   min = 1,
                   max = 50,
                   value = 5,
                   width = "100%"
                 ),
                 fluidRow(column(
                   6,
                   h3("Run Baseline"),
                   actionButton("runBaseline", "Run Baseline Model")
                 ),
                 column(
                   6,
                   h3("Run Scenario"),
                   actionButton("runScenario", "Run Scenario Model")
                 ))
               )),
    navbarMenu(
      "Results",
      tabPanel(
        title = "Baseline versus Scenario plots",
        h3("Compare baseline and scenario"),
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
            div(
              id = "dwnbutton_b",
              downloadButton(
                "downloadData_baseline1",
                "Download Baseline output",
                disabled = "disabled"
              )
            )
          ),
          column(
            6,
            h3("Scenario output"),
            useShinyjs(),
            div(
              id = "dwnbutton_s",
              downloadButton(
                "downloadData_scenario1",
                "Download Scenario output",
                disabled = "disabled"
              )
            )
          )
        )
      ),
      tabPanel(title = "Yield curves", fluidRow(
        h4(
          "Add info here about yield curves - maybe this will become sub menu under plots tab using navbar"
        )
      ))
    )
  )
  
  server <- function(input, output, session) {
    output$ui <- renderUI({
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      switch(
        input$selectedParameter,
        "Pelagic" = fluidRow(
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_pel",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_pel_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_pel",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_pel_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_pel",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_pel_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_pel",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_pel_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_pel",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_pel_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_pel",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_pel_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_pel",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_pel_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_pel",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_pel_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_pel",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_pel_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_pel",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_pel_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_pel",
              "Mollusc Dredge Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_pel_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_pel",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$pelagic[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_pel_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Demersal" = fluidRow(
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_dem",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_dem_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_dem",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_dem_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_dem",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_dem_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_dem",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_dem_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_dem",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_dem_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #column(
          #  width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_dem",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_dem_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_dem",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_dem_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_dem",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_dem_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_dem",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_dem_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_dem",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_dem_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_dem",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_dem_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_dem",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$demersal[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_dem_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Migratory" = fluidRow(
          #column(
          #  width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_mig",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_mig_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_mig",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_mig_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_mig",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_mig_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_mig",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_mig_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_mig",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_mig_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_mig",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_mig_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_mig",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_mig_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_mig",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_mig_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_mig",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_mig_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_mig",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_mig_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_mig",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_mig_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_mig",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$migratory[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_mig_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Filtben" = fluidRow(
          #       column(
          #         width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_fb",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_fb_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_fb",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_fb_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_fb",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_fb_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_fb",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_fb_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_fb",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_fb_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_fb",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_fb_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_fb",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_fb_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_fb",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_fb_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_fb",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_fb_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_fb",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_fb_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_fb",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_fb_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_fb",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$filtben[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_fb_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Carnben" = fluidRow(
          #       column(
          #         width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_cb",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_cb_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_cb",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_cb_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_cb",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_cb_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_cb",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_cb_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_cb",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_cb_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_cb",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_cb_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_cb",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_cb_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_cb",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_cb_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_cb",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_cb_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_cb",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_cb_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_cb",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_cb_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_cb",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnben[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_cb_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Carnzoo" = fluidRow(
          #       column(
          #         width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_cz",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_cz_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_cz",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_cz_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_cz",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[2],
              step = 0.1,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_cz_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_cz",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_cz_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_cz",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_cz_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
            #)
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_cz",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_cz_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_cz",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_cz_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_cz",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_cz_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_cz",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_cz_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_cz",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_cz_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_cz",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_cz_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_cz",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$carnzoo[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_cz_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Bird" = fluidRow(
          #       column(
          #         width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_b",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_b_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_b",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_b_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_b",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_b_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_b",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_b_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_b",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_b_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_b",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_b_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_b",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_b_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_b",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_b_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_b",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_b_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_b",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_b_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_b",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_b_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_b",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$bird[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_b_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Seal" = fluidRow(
          #        column(
          #          width = 5
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_s",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_s_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_s",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_s_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_s",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_s_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_s",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_s_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_s",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_s_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_s",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_s_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_s",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_s_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_s",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_s_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_s",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_s_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_s",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_s_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_s",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_s_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_s",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$seal[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_s_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Ceta" = fluidRow(
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_ceta",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_ceta_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_ceta",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_ceta_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_ceta",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_ceta_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_ceta",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_ceta_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_ceta",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_ceta_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_ceta",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_ceta_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_ceta",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_ceta_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_ceta",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_ceta_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_ceta",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_ceta_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_ceta",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_ceta_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_ceta",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_ceta_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_ceta",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$ceta[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_ceta_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        ),
        "Kelp" = fluidRow(
          #        column(
          #          width = 5,
          wellPanel(
            sliderInput(
              "pelagicTrawlDiscard_kelp",
              "Pelagic Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[1],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("pelagicTrawlDiscard_kelp_reset", "Reset"),
            helpText("Pelgic discard notes here")
          ),
          wellPanel(
            sliderInput(
              "sanSpratTrawlDiscard_kelp",
              "San spart trawl discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[2],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("sanSpratTrawlDiscard_kelp_reset", "Reset"),
            helpText("San spart trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "llMackerelDiscard_kelp",
              "llMackerel Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[3],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("llMackerelDiscard_kelp_reset", "Reset"),
            helpText("llMackeral discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlDiscard_kelp",
              "Beam Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[4],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlDiscard_kelp_reset", "Reset"),
            helpText("Beam Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "demersalSeineDiscard_kelp",
              "Demersal Seine Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[5],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalSeineDiscard_kelp_reset", "Reset"),
            helpText("Demersal Seine discard notes here")
          ),
          #        column(
          #         width = 5,
          wellPanel(
            sliderInput(
              "demersalOtterTrawlDiscard_kelp",
              "Demersal OtterTrawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[6],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("demersalOtterTrawlDiscard_kelp_reset", "Reset"),
            helpText("Demersal OtterTrawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "gillLongDemersalDiscard_kelp",
              "Gill Long Demersal Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[7],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("gillLongDemersalDiscard_kelp_reset", "Reset"),
            helpText("Gill Long Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "beamTrawlShrimpDiscard_kelp",
              "Beam Trawl Shrimp Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[8],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("beamTrawlShrimpDiscard_kelp_reset", "Reset"),
            helpText("Beam Trawl Shrimp discard notes here")
          ),
          wellPanel(
            sliderInput(
              "nephropsTrawlDiscard_kelp",
              "Nephrops Trawl Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[9],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("nephropsTrawlDiscard_kelp_reset", "Reset"),
            helpText("Nephrops Trawl discard notes here")
          ),
          wellPanel(
            sliderInput(
              "creelsDiscard_kelp",
              "Creels Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[10],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("creelsDiscard_kelp_reset", "Reset"),
            helpText("Creels discard notes here")
          ),
          wellPanel(
            sliderInput(
              "molluscDredgeDiscard_kelp",
              "Mollusc Dredge Discard Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[11],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("molluscDredgeDiscard_kelp_reset", "Reset"),
            helpText("Mollusc Dredge discard notes here")
          ),
          wellPanel(
            sliderInput(
              "whalerDiscard_kelp",
              "Whaler Discard:",
              min = 0,
              max = 1.0,
              value = model$data$fleet.model$gear_group_discard$kelp[12],
              step = 0.000001,
              width = "100%"
            ),
            actionButton("whalerDiscard_kelp_reset", "Reset"),
            helpText("Whaler discard notes here")
          )
        )
      )
    })
    
    #Pelagic discard reset
    
    observeEvent(input$pelagicTrawlDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[9]
      )
    })
    
    observeEvent(input$creelsDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[11]
      )
    })
    
    observeEvent(input$whalerDiscard_pel_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_pel",
        value = model$data$fleet.model$gear_group_discard$pelagic[12]
      )
    })
    
    #Demersal discard reset
    
    observeEvent(input$pelagicTrawlDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[9]
      )
    })
    
    observeEvent(input$creelsDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[11]
      )
    })
    
    observeEvent(input$whalerDiscard_dem_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_dem",
        value = model$data$fleet.model$gear_group_discard$demersal[12]
      )
    })
    
    #Migratory discard reset
    
    observeEvent(input$pelagicTrawlDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[9]
      )
    })
    
    observeEvent(input$creelsDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[11]
      )
    })
    
    observeEvent(input$whalerDiscard_mig_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_mig",
        value = model$data$fleet.model$gear_group_discard$migratory[12]
      )
    })
    
    #Filtben discard reset
    
    observeEvent(input$pelagicTrawlDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[9]
      )
    })
    
    observeEvent(input$creelsDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_fb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_fb",
        value = model$data$fleet.model$gear_group_discard$filtben[11]
      )
    })
    
    #Carnben discard reset
    
    observeEvent(input$pelagicTrawlDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[9]
      )
    })
    
    observeEvent(input$creelsDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[11]
      )
    })
    
    observeEvent(input$whalerDiscard_cb_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_cb",
        value = model$data$fleet.model$gear_group_discard$carnben[12]
      )
    })
    
    #Carnzoo discard reset
    
    observeEvent(input$pelagicTrawlDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[9]
      )
    })
    
    observeEvent(input$creelsDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[11]
      )
    })
    
    observeEvent(input$whalerDiscard_cz_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_cz",
        value = model$data$fleet.model$gear_group_discard$carnzoo[12]
      )
    })
    
    #Bird discard reset
    
    observeEvent(input$pelagicTrawlDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[9]
      )
    })
    
    observeEvent(input$creelsDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(session,
                        "creelsDiscard_b",
                        value = model$data$fleet.model$gear_group_discard$bird[10])
    })
    
    observeEvent(input$molluscDredgeDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_b",
        value = model$data$fleet.model$gear_group_discard$bird[11]
      )
    })
    
    observeEvent(input$whalerDiscard_b_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(session,
                        "whalerDiscard_b",
                        value = model$data$fleet.model$gear_group_discard$bird[12])
    })
    
    #Seal discard reset
    
    observeEvent(input$pelagicTrawlDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[9]
      )
    })
    
    observeEvent(input$creelsDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(session,
                        "creelsDiscard_s",
                        value = model$data$fleet.model$gear_group_discard$seal[10])
    })
    
    observeEvent(input$molluscDredgeDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_s",
        value = model$data$fleet.model$gear_group_discard$seal[11]
      )
    })
    
    observeEvent(input$whalerDiscard_s_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(session,
                        "whalerDiscard_s",
                        value = model$data$fleet.model$gear_group_discard$seal[12])
    })
    
    #Ceta discard reset
    
    observeEvent(input$pelagicTrawlDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[9]
      )
    })
    
    observeEvent(input$creelsDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[11]
      )
    })
    
    observeEvent(input$whalerDiscard_ceta_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_ceta",
        value = model$data$fleet.model$gear_group_discard$ceta[12]
      )
    })
    
    #Kelp discard reset
    
    observeEvent(input$pelagicTrawlDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "pelagicTrawlDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[1]
      )
    })
    
    observeEvent(input$sanSpratTrawlDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "sanSpratTrawlDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[2]
      )
    })
    
    observeEvent(input$llMackerelDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "llMackerelDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[3]
      )
    })
    
    observeEvent(input$beamTrawlDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[4]
      )
    })
    
    observeEvent(input$demersalSeineDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalSeineDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[5]
      )
    })
    
    observeEvent(input$demersalOtterTrawlDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "demersalOtterTrawlDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[6]
      )
    })
    
    observeEvent(input$gillLongDemersalDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "gillLongDemersalDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[7]
      )
    })
    
    observeEvent(input$beamTrawlShrimpDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "beamTrawlShrimpDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[8]
      )
    })
    
    observeEvent(input$nephropsTrawlDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "nephropsTrawlDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[9]
      )
    })
    
    observeEvent(input$creelsDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "creelsDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[10]
      )
    })
    
    observeEvent(input$molluscDredgeDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "molluscDredgeDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[11]
      )
    })
    
    observeEvent(input$whalerDiscard_kelp_reset, {
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      updateSliderInput(
        session,
        "whalerDiscard_kelp",
        value = model$data$fleet.model$gear_group_discard$kelp[12]
      )
    })
    
    observeEvent(input$runBaseline, {
      showModal(
        modalDialog(
          "Please wait whilst model runs baseline. See baseline plot in results tab once run completed",
          footer = NULL
        )
      )
      # Run baseline
      model <<-
        e2e_read(input$selectedlocation, input$selectedVariant)
      #View(model)
      
      results_baseline <-
        e2e_run(model, nyears = input$year, csv.output = TRUE)
      output$baselinePlot <-
        renderPlot({
          e2e_plot_ts(model, results_baseline)
        })
      removeModal()
      resultDirBaseline <- toString(model$setup$resultsdir)
      output$downloadData_baseline1 <- downloadHandler(
        filename = function() {
          'baseline.tar'
        },
        content = function(file) {
          tar(file, resultDirBaseline)
        }
      )
      if (!is.null(model)) {
        enable("downloadData_baseline1")
        runjs("$('#dwnbutton_b').removeAttr('title');")
      } else{
        disable("downloadData_baseline1")
        runjs("$('#dwnbutton_b').attr('title', 'Data not available');")
      }
    })
    
    observeEvent(input$runScenario, {
      showModal(
        modalDialog(
          "Please wait whilst model runs scenario .... once completed plots available on results tab",
          footer = NULL
        )
      )
      # Run scenario
      model <- e2e_read(input$selectedlocation, input$selectedVariant)
      scenario_model <- model
      #print("Got this far 1")
      # Temperature
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
        scenario_model$data$fleet.model$gear_mult[10] <- input$creels
      if (!is.null(input$molluscDredge))
        scenario_model$data$fleet.model$gear_mult[11] <-
        input$molluscDredge
      if (!is.null(input$whaler))
        scenario_model$data$fleet.model$gear_mult[12] <- input$whaler
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
      #print("Got this far 15")
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
        filename = function() {
          'scenario.tar'
        },
        content = function(file) {
          tar(file, resultDirScenario)
        }
      )
      if (!is.null(model)) {
        enable("downloadData_scenario1")
        runjs("$('#dwnbutton_s').removeAttr('title');")
      } else{
        disable("downloadData_scenario1")
        runjs("$('#dwnbutton_s').attr('title', 'Data not available');")
      }
    })
  }
  
  shinyApp(ui, server
  )