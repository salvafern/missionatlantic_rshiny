library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(shinyjs)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # App title ----
  h4("Strath E2E RShiny App"),
  tabsetPanel(
    id = "inTabset",
    tabPanel(title = "Overview", fluidRow(h4(
      "Overview of StrathE2E model to go here"
    ))),
    tabPanel(title = "Model Setup", fluidRow(h4(
      "Png's to be supplied by Jack here"
    ))),
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
    tabPanel(title = "Scenario parameter setup",   sidebarLayout(
      sidebarPanel(
        selectInput(
          "selectedParameter",
          h4("Parameter"),
          choices
          = list(
            "Fishing Activity",
            "Gear Group Discard",
            "Temperature",
            "Nutrients",
            "Seabed Abrasion"
          ),
          selected = "Fishing Activity"
        ),
        actionButton("runScenario", "Run Model"),
        width = 3
      ),
      mainPanel (uiOutput("ui"))
    )),
    tabPanel(
      title = "Plots",
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
    )))
  )
)

server <- function(input, output) {

  output$ui <- renderUI({
    switch(
      input$selectedParameter,
      "Temperature" = fluidRow(column(
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
      )),
      "Nutrients" = fluidRow(column(
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
      )),
      "Fishing Activity" = fluidRow(
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
      ),
      "Seabed Abrasion" = fluidRow(
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
      ),
      "Gear Group Discard" = fluidRow(
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "pelTrawlDiscard",
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
              "sanSpratTrawlDiscard",
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
              "llMackerelDiscard",
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
              "beamTrawlDiscard",
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
              "demersalSeineDiscard",
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
              "demersalOtterTrawlDiscar",
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
              "gillLongDemersalDiscard",
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
              "beamTrawlShrimpDiscard",
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
              "nephropsTrawlDiscard",
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
              "creelsDiscard",
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
              "molluscDredgeDiscard",
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
              "whalerDiscard",
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
    # flow_matrix_all_fluxes_base <-
    #   read.csv(paste(
    #     resultDirBaseline,
    #     "flow_matrix_all_fluxes-base.csv",
    #     sep = '/'
    #   ))
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
    if (!is.null(input$pelTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[1] <- input$pelTrawlPlough
    if (!is.null(input$sanSpratTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[2] <- input$sanSpratTrawlPlough
    if (!is.null(input$llMackerelPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[3] <- input$llMackerelPlough
    if (!is.null(input$beamTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[4] <- input$beamTrawlPlough
    if (!is.null(input$demersalSeinePlough)) scenario_model$data$fleet.model$gear_ploughing_rate[5] <- input$demersalSeinePlough
    if (!is.null(input$demersalOtterTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[6] <- input$demersalOtterTrawlPlough
    if (!is.null(input$gillLongDemersalPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[7] <- input$gillLongDemersalPlough
    if (!is.null(input$beamTrawlShrimpPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[8] <- input$beamTrawlShrimpPlough
    if (!is.null(input$nephropsTrawlPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[9] <- input$nephropsTrawlPlough
    if (!is.null(input$creelsPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[10] <- input$creelsPlough
    if (!is.null(input$molluscDredgePlough)) scenario_model$data$fleet.model$gear_ploughing_rate[11] <- input$molluscDredgePlough
    if (!is.null(input$whalerPlough)) scenario_model$data$fleet.model$gear_ploughing_rate[12] <- input$whalerPlough

        # Discard per gear
    if (!is.null(input$pelagicTrawlDiscard)) scenario_model$data$fleet.model$gear_group_discard[1] <- input$pelagicTrawlDiscard
    if (!is.null(input$sanSpratTrawlDiscard)) scenario_model$data$fleet.model$gear_group_discard[2] <- input$sanSpratTrawlDiscard
    if (!is.null(input$llMackerelDiscard)) scenario_model$data$fleet.model$gear_group_discard[3] <- input$llMackerelDiscard
    if (!is.null(input$beamTrawlDiscard)) scenario_model$data$fleet.model$gear_group_discard[4] <- input$beamTrawlDiscard
    if (!is.null(input$demersalSeineDiscard)) scenario_model$data$fleet.model$gear_group_discard[5] <- input$demersalSeineDiscard
    if (!is.null(input$demersalOtterTrawlDiscard)) scenario_model$data$fleet.model$gear_group_discard[6] <- input$demersalOtterTrawlDiscard
    if (!is.null(input$gillLongDemersalDiscard)) scenario_model$data$fleet.model$gear_group_discard[7] <- input$gillLongDemersalDiscard
    if (!is.null(input$beamTrawlShrimpDiscard)) scenario_model$data$fleet.model$gear_group_discard[8] <- input$beamTrawlShrimpDiscard
    if (!is.null(input$nephropsTrawlDiscard)) scenario_model$data$fleet.model$gear_group_discard[9] <- input$nephropsTrawlDiscard
    if (!is.null(input$creelsDiscard)) scenario_model$data$fleet.model$gear_group_discard[10] <- input$creelsDiscard
    if (!is.null(input$molluscDredgeDiscard)) scenario_model$data$fleet.model$gear_group_discard[11] <- input$molluscDredgeDiscard
    if (!is.null(input$whalerDiscard)) scenario_model$data$fleet.model$gear_group_discard[12] <- input$whalerDiscard
    
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