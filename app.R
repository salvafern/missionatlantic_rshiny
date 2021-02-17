library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(zip)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  theme = shinytheme("superhero"),
  
  # App title ----
  h4("Strath E2E RShiny App"),
  tabsetPanel(
    id = "inTabset",
    tabPanel(title = "Overview", fluidRow(h4(
      "Overview of project here"
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
          "selectedTime",
          h4("Time period"),
          choices
          = list("1970-1999"),
          selected = "1970-1999"
        ),
        width = 3
      ),
      #Main Panel: plot map here in the future
      mainPanel (
        plotOutput(
          outputId = "Plotmap",
          width = "500px",
          height = "300px"
        )
      )
    )),
    tabPanel(title = "Fishing Activity",
             fluidRow(
               column(
                 width = 10,
                 offset = 2,
                 h5("Adjust fishing activity per gear"),
                 sliderInput(
                   "year",
                   "Year:",
                   min = 1,
                   max = 50,
                   value = 5,
                   width = "25%"
                 ),
                 sliderInput(
                   "pelTrawlAct",
                   "Pelagic Trawl+Seine activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "sanSpratTrawlAct",
                   "Sandeel sprat trawl activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "llMackerel",
                   "Longline mackerel activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "beamTrawl",
                   "Beam Trawl BT1+BT2 activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "demersalSeine",
                   "Demersal Seine activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "demersalOtterTrawl",
                   "Demersal Otter Trawl TR1 activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "gillLongDemersal",
                   "Gill Nets+Longline demersal activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "beamTrawlShrimp",
                   "Beam Trawl shrimp activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "nephropsTrawl",
                   "Nephrops Trawl TR2 activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "creels",
                   "Creels activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "molluscDredge",
                   "Mollusc Dredge activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "whaler",
                   "Whaler activity:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 )
               )
             )),
    tabPanel(title = "Harvest ratios",
             fluidRow(
               column(
                 width = 10,
                 offset = 2,
                 h5("Adjust harvest ratios"),
                 sliderInput(
                   "pelagic",
                   "Pelagic:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "demersal",
                   "Demersal:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "migratory",
                   "Migratory:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "filtben",
                   "Filtben:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "carnben",
                   "Carnben:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "carnzoo",
                   "Carnzoo:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "bird",
                   "Bird:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "seal",
                   "Seal:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "ceta",
                   "Ceta:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 ),
                 sliderInput(
                   "kelp",
                   "Kelp:",
                   min = 0,
                   max = 2.0,
                   value = 1.0,
                   step = 0.2,
                   width = "25%"
                 )
               )
             )),
    tabPanel(
      title = "Run and plot",
      h3("Run model to compare baseline and scenario"),
      actionButton("run", "Run Model"),
      fluidRow(column(
        6,
        h3("Baseline"),
        plotOutput("baselinePlot")# %>% withSpinner(color="#0dc5c1")
      ),
      column(
        6,
        h3("Scenario"),
        plotOutput("scenarioPlot")# %>% withSpinner(color="#0dc5c1")
      )),
      fluidRow(column(
        6,
        h3("Baseline output"),
        downloadButton("downloadData_baseline1", "Download flow_matrix_all_fluxes_base")
      ),
      column(
        6,
        h3("Scenario output"),
        downloadButton("downloadData_scenario1", "Download flow_matrix_all_fluxes_base")
      ))
    )
  )
)

server <- function(input, output) {
  observeEvent(input$run, {
    showModal(modalDialog("Please wait whilst model runs....", footer = NULL))
    # Run baseline
    model <- e2e_read(input$selectedlocation, input$selectedTime)
    results_baseline <- e2e_run(model, nyears = input$year, csv.output = TRUE)
    output$baselinePlot <-
      renderPlot({
        e2e_plot_ts(model, results_baseline)
      })
    # Run scenario
    scenario_model <- model

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
    
    # Harvest Ratio
    scenario_model$data$fleet.model$HRscale_vector_multiplier[1] <-
      input$pelagic
    scenario_model$data$fleet.model$HRscale_vector_multiplier[2] <-
      input$demersal
    scenario_model$data$fleet.model$HRscale_vector_multiplier[3] <-
      input$migratory
    scenario_model$data$fleet.model$HRscale_vector_multiplier[4] <-
      input$filtben
    scenario_model$data$fleet.model$HRscale_vector_multiplier[5] <-
      input$carnben
    scenario_model$data$fleet.model$HRscale_vector_multiplier[6] <-
      input$carnzoo
    scenario_model$data$fleet.model$HRscale_vector_multiplier[7] <-
      input$bird
    scenario_model$data$fleet.model$HRscale_vector_multiplier[8] <-
      input$seal
    scenario_model$data$fleet.model$HRscale_vector_multiplier[9] <-
      input$ceta
    scenario_model$data$fleet.model$HRscale_vector_multiplier[10] <-
      input$kelp
    
    results_scenario <- e2e_run(scenario_model, nyears = input$year, csv.output = TRUE)
    output$scenarioPlot <-
      renderPlot({
        e2e_plot_ts(scenario_model, results_scenario)
      })
    removeModal()
    resultDirBaseline <- toString(model$setup$resultsdir)
    resultDirScenario <- toString(scenario_model$setup$resultsdir)
    flow_matrix_all_fluxes_base <- read.csv(paste(resultDirBaseline, "flow_matrix_all_fluxes-base.csv", sep='/'))
    flow_matrix_all_fluxes_scenario <- read.csv(paste(resultDirScenario, "flow_matrix_all_fluxes-base.csv", sep='/'))
    
    output$downloadData_baseline1 <- downloadHandler(
      filename = function() {
        paste("flow_matrix_all_fluxes-base_baseline.csv")
      },
      content <- function(file) {
        write.csv(flow_matrix_all_fluxes_base, file)
      }
    )
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