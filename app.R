library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(zip)

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
           h3("Baseline"),
           # plotOutput("baselinePlot")
        # h3("Baseline output"),
        # downloadButton(
        #   "downloadData_baseline1",
        #   "Download flow_matrix_all_fluxes_base"
        # )
      )
    )),
    tabPanel(title = "Parameter setup",   sidebarLayout(
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
        width = 3
      ),
      #Main Panel: plot map here in the future
      mainPanel (uiOutput("ui"))
    )),
    tabPanel(
      title = "Run Scenario and plot",
      h3("Run model to compare baseline and scenario"),
      actionButton("runScenario", "Run Model"),
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
          downloadButton(
            "downloadData_baseline1",
            "Download flow_matrix_all_fluxes_base"
          )
        ),
        column(
          6,
          h3("Scenario output"),
          downloadButton(
            "downloadData_scenario1",
            "Download flow_matrix_all_fluxes_base"
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  # Defaults if parameters not changed
  # input$temperature_so <- 0
  # input$temperature_d <- 0
  # input$temperature_si <- 0
  # input$si_othernitrate <- 0
  # input$si_otherammonia <- 0
  # input$pelTrawlAct<- 1.0
  # input$sanSpratTrawlAct <- 1.0
  # input$llMackerel <- 1.0
  # input$beamTrawl <- 1.0
  # input$demersalSeine <- 1.0
  # input$demersalOtterTrawl <- 1.0
  # input$gillLongDemersal <- 1.0
  # input$beamTrawlShrimp <- 1.0
  # input$nephropsTrawl <- 1.0
  # input$creels <- 1.0
  # input$molluscDredge <- 1.0
  # input$whaler <- 1.0
  # 
  # input$pelTrawlPlough <- 0
  # input$sanSpratTrawlPlough <- 1.0
  # input$llMackerelPlough <- 1.0
  # input$beamTrawlPlough <- 1.0
  # input$demersalSeinePlough <- 1.0
  # input$demersalOtterTrawlPlough <- 1.0
  # input$gillLongDemersalPlough <- 1.0
  # input$beamTrawlShrimpPlough <- 1.0
  # input$nephropsTrawlPlough <- 1.0
  # input$creelsPlough <- 1.0
  # input$molluscDredgePlough <- 1.0
  # input$whalerPlough <- 1.0
  # 
  # input$pelagic <- 1.0
  # input$demersal <- 1.0
  # input$migratory <- 1.0
  # input$filtben <- 1.0
  # input$carnben <- 1.0
  # input$carnzoo <- 1.0
  # input$bird <- 1.0
  # input$seal <- 1.0
  # input$ceta <- 1.0
  # input$kelp <- 1.0
  
  output$ui <- renderUI({
    switch(
      input$selectedParameter,
      "Temperature" = fluidRow(column(
        width = 5,
        offset = 2,
        wellPanel(
          sliderInput(
            "temperature_so",
            "Temperature SO:",
            min = 0,
            max = 10,
            value = 0,
            width = "100%"
          ),
          helpText("Additional SO temperature")
        ),
        wellPanel(
          sliderInput(
            "temperature_d",
            "Temperature D:",
            min = 0,
            max = 10,
            value = 0,
            width = "100%"
          ),
          helpText("Additional D temperature")
        ),
        wellPanel(
          sliderInput(
            "temperature_si",
            "Temperature SI:",
            min = 0,
            max = 10,
            value = 0,
            width = "100%"
          ),
          helpText("Additional SI temperature")
        ),
      )),
      "Nutrients" = fluidRow(column(
        width = 5,
        offset = 2,
        wellPanel(
          sliderInput(
            "si_othernitrate",
            "SI Other Nitrate:",
            min = 0,
            max = 50,
            value = 0,
            width = "100%"
          ),
          helpText("Additional SI Other Nitrate")
        ),
        wellPanel(
          sliderInput(
            "si_otherammonia",
            "SI Other Ammonia:",
            min = 0,
            max = 50,
            value = 0,
            width = "100%"
          ),
          helpText("Additional SI Other Ammonia")
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
              "pelagic",
              "Pelagic:",
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
              "demersal",
              "Demersal:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Demersal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "migratory",
              "Migratory:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Migratory discard notes here")
          ),
          wellPanel(
            sliderInput(
              "filtben",
              "Filtben:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Filtben discard notes here")
          ),
          wellPanel(
            sliderInput(
              "carnben",
              "Carnben:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Carnben discard notes here")
          )
        ),
        column(
          width = 5,
          wellPanel(
            sliderInput(
              "carnzoo",
              "Carnzoo:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Carnzoo discard notes here")
          ),
          wellPanel(
            sliderInput(
              "bird",
              "Bird:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Bird discard notes here")
          ),
          wellPanel(
            sliderInput(
              "seal",
              "Seal:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Seal discard notes here")
          ),
          wellPanel(
            sliderInput(
              "ceta",
              "Ceta:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Ceta discard notes here")
          ),
          wellPanel(
            sliderInput(
              "kelp",
              "Kelp:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            ),
            helpText("Kelp discard notes here")
          )
        )
      )
    )
  })
  
  observeEvent(input$runBaseline, {
    showModal(modalDialog("Please wait whilst model runs baseline ....", footer = NULL))
    # Run baseline
    model <- e2e_read(input$selectedlocation, input$selectedVariant)
    View(model)
    print("Number of years is: ",input$year)
    results_baseline <-
      e2e_run(model, nyears = input$year, csv.output = TRUE)
    output$baselinePlot <-
      renderPlot({
        e2e_plot_ts(model, results_baseline)
      })
    removeModal()
    resultDirBaseline <- toString(model$setup$resultsdir)
    flow_matrix_all_fluxes_base <-
      read.csv(paste(
        resultDirBaseline,
        "flow_matrix_all_fluxes-base.csv",
        sep = '/'
      ))
    output$downloadData_baseline1 <- downloadHandler(
      filename = function() {
        paste("flow_matrix_all_fluxes-base_baseline.csv")
      },
      content <- function(file) {
        write.csv(flow_matrix_all_fluxes_base, file)
      }
    )
  })
  observeEvent(input$runScenario, {
    showModal(modalDialog("Please wait whilst model runs scenario ....", footer = NULL))
    # Run scenario
    model <- e2e_read(input$selectedlocation, input$selectedVariant)
    scenario_model <- model
    # Temperature
    print("Got this far 1")
    scenario_model$data$physics.drivers$so_temp[1] <- scenario_model$data$physics.drivers$so_temp[1] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[2] <- scenario_model$data$physics.drivers$so_temp[2] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[3] <- scenario_model$data$physics.drivers$so_temp[3] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[4] <- scenario_model$data$physics.drivers$so_temp[4] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[5] <- scenario_model$data$physics.drivers$so_temp[5] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[6] <- scenario_model$data$physics.drivers$so_temp[6] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[7] <- scenario_model$data$physics.drivers$so_temp[7] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[8] <- scenario_model$data$physics.drivers$so_temp[8] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[9] <- scenario_model$data$physics.drivers$so_temp[9] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[10] <- scenario_model$data$physics.drivers$so_temp[10] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[11] <- scenario_model$data$physics.drivers$so_temp[11] + input$temperature_so
    scenario_model$data$physics.drivers$so_temp[12] <- scenario_model$data$physics.drivers$so_temp[12] + input$temperature_so
    print("Got this far 2")
    scenario_model$data$physics.drivers$d_temp[1] <- scenario_model$data$physics.drivers$d_temp[1] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[2] <- scenario_model$data$physics.drivers$d_temp[2] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[3] <- scenario_model$data$physics.drivers$d_temp[3] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[4] <- scenario_model$data$physics.drivers$d_temp[4] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[5] <- scenario_model$data$physics.drivers$d_temp[5] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[6] <- scenario_model$data$physics.drivers$d_temp[6] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[7] <- scenario_model$data$physics.drivers$d_temp[7] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[8] <- scenario_model$data$physics.drivers$d_temp[8] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[9] <- scenario_model$data$physics.drivers$d_temp[9] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[10] <- scenario_model$data$physics.drivers$d_temp[10] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[11] <- scenario_model$data$physics.drivers$d_temp[11] + input$temperature_d
    scenario_model$data$physics.drivers$d_temp[12] <- scenario_model$data$physics.drivers$d_temp[12] + input$temperature_d
    print("Got this far 2")
    scenario_model$data$physics.drivers$si_temp[1] <- scenario_model$data$physics.drivers$si_temp[1] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[2] <- scenario_model$data$physics.drivers$si_temp[2] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[3] <- scenario_model$data$physics.drivers$si_temp[3] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[4] <- scenario_model$data$physics.drivers$si_temp[4] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[5] <- scenario_model$data$physics.drivers$si_temp[5] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[6] <- scenario_model$data$physics.drivers$si_temp[6] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[7] <- scenario_model$data$physics.drivers$si_temp[7] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[8] <- scenario_model$data$physics.drivers$si_temp[8] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[9] <- scenario_model$data$physics.drivers$si_temp[9] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[10] <- scenario_model$data$physics.drivers$si_temp[10] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[11] <- scenario_model$data$physics.drivers$si_temp[11] + input$temperature_si
    scenario_model$data$physics.drivers$si_temp[12] <- scenario_model$data$physics.drivers$si_temp[12] + input$temperature_si    
    print("Got this far 4")
    # Nutrients 
    scenario_model$data$chemistry.drivers$si_othernitrate[1] <- scenario_model$data$chemistry.drivers$si_othernitrate[1] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[2] <- scenario_model$data$chemistry.drivers$si_othernitrate[2] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[3] <- scenario_model$data$chemistry.drivers$si_othernitrate[3] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[4] <- scenario_model$data$chemistry.drivers$si_othernitrate[4] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[5] <- scenario_model$data$chemistry.drivers$si_othernitrate[5] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[6] <- scenario_model$data$chemistry.drivers$si_othernitrate[6] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[7] <- scenario_model$data$chemistry.drivers$si_othernitrate[7] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[8] <- scenario_model$data$chemistry.drivers$si_othernitrate[8] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[9] <- scenario_model$data$chemistry.drivers$si_othernitrate[9] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[10] <- scenario_model$data$chemistry.drivers$si_othernitrate[10] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[11] <- scenario_model$data$chemistry.drivers$si_othernitrate[11] + input$si_othernitrate
    scenario_model$data$chemistry.drivers$si_othernitrate[12] <- scenario_model$data$chemistry.drivers$si_othernitrate[12] + input$si_othernitrate
    print("Got this far 5")
    scenario_model$data$chemistry.drivers$si_otherammonia[1] <- scenario_model$data$chemistry.drivers$si_otherammonia[1] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[2] <- scenario_model$data$chemistry.drivers$si_otherammonia[2] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[3] <- scenario_model$data$chemistry.drivers$si_otherammonia[3] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[4] <- scenario_model$data$chemistry.drivers$si_otherammonia[4] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[5] <- scenario_model$data$chemistry.drivers$si_otherammonia[5] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[6] <- scenario_model$data$chemistry.drivers$si_otherammonia[6] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[7] <- scenario_model$data$chemistry.drivers$si_otherammonia[7] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[8] <- scenario_model$data$chemistry.drivers$si_otherammonia[8] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[9] <- scenario_model$data$chemistry.drivers$si_otherammonia[9] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[10] <- scenario_model$data$chemistry.drivers$si_otherammonia[10] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[11] <- scenario_model$data$chemistry.drivers$si_otherammonia[11] + input$si_otherammonia
    scenario_model$data$chemistry.drivers$si_otherammonia[12] <- scenario_model$data$chemistry.drivers$si_otherammonia[12] + input$si_otherammonia
    print("Got this far 6")
    # Gear Mult
    scenario_model$data$fleet.model$gear_mult[1] <-
      input$pelTrawlAct
    scenario_model$data$fleet.model$gear_mult[2] <-
      input$sanSpratTrawlAct
    scenario_model$data$fleet.model$gear_mult[3] <- input$llMackerel
    scenario_model$data$fleet.model$gear_mult[4] <- input$beamTrawl
    scenario_model$data$fleet.model$gear_mult[5] <-
      input$demersalSeine
    scenario_model$data$fleet.model$gear_mult[6] <-
      input$demersalOtterTrawl
    scenario_model$data$fleet.model$gear_mult[7] <-
      input$gillLongDemersal
    scenario_model$data$fleet.model$gear_mult[8] <-
      input$beamTrawlShrimp
    scenario_model$data$fleet.model$gear_mult[9] <-
      input$nephropsTrawl
    scenario_model$data$fleet.model$gear_mult[10] <- input$creels
    scenario_model$data$fleet.model$gear_mult[11] <-
      input$molluscDredge
    scenario_model$data$fleet.model$gear_mult[12] <- input$whaler
    print("Got this far 7")
    # Seabed abrasian
    scenario_model$data$fleet.model$gear_ploughing_rate[1] <-
      input$pelTrawlPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[2] <-
      input$sanSpratTrawlPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[3] <- input$llMackerelPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[4] <- input$beamTrawlPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[5] <-
      input$demersalSeinePlough
    scenario_model$data$fleet.model$gear_ploughing_rate[6] <-
      input$demersalOtterTrawlPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[7] <-
      input$gillLongDemersalPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[8] <-
      input$beamTrawlShrimpPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[9] <-
      input$nephropsTrawlPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[10] <- input$creelsPlough
    scenario_model$data$fleet.model$gear_ploughing_rate[11] <-
      input$molluscDredgePlough
    scenario_model$data$fleet.model$gear_ploughing_rate[12] <- input$whalerPlough
    print("Got this far 8")
    # Discard per gear
    scenario_model$data$fleet.model$gear_group_discard[1] <-
      input$pelagic
    scenario_model$data$fleet.model$gear_group_discard[2] <-
      input$demersal
    scenario_model$data$fleet.model$gear_group_discard[3] <-
      input$migratory
    scenario_model$data$fleet.model$gear_group_discard[4] <-
      input$filtben
    scenario_model$data$fleet.model$gear_group_discard[5] <-
      input$carnben
    scenario_model$data$fleet.model$gear_group_discard[6] <-
      input$carnzoo
    scenario_model$data$fleet.model$gear_group_discard[7] <-
      input$bird
    scenario_model$data$fleet.model$gear_group_discard[8] <-
      input$seal
    scenario_model$data$fleet.model$gear_group_discard[9] <-
      input$ceta
    scenario_model$data$fleet.model$gear_group_discard[10] <-
      input$kelp
    
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
    flow_matrix_all_fluxes_scenario <-
      read.csv(paste(
        resultDirScenario,
        "flow_matrix_all_fluxes-base.csv",
        sep = '/'
      ))
    output$downloadData_scenario1 <- downloadHandler(
      filename = function() {
        paste("flow_matrix_all_fluxes-base_scenario.csv")
      },
      content <- function(file) {
        write.csv(flow_matrix_all_fluxes_scenario, file)
      }
    )
  })
}

shinyApp(ui, server)