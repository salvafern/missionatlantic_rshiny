# Function to create mortality rate UI


createMortalityRateUI <- function(model,input) {
  switch(
    input$selectedMortGuild,
    "Planktivorous fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "plank_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("plank_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "plank_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("plank_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Demersal fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "dem_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("dem_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "dem_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("dem_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Migratory fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "mig_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("mig_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "mig_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("mig_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Suspension/deposit feeding benthos" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(style = "overflow-y:scroll; max-height: 600px",
                          wellPanel(
                            sliderInput(
                              "sus_mort_inshore",
                              paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                              min = 0,
                              max = 1.0,
                              value = 0.0,
                              step = 0.000001,
                              width = "100%"
                            ),
                            actionButton("sus_mort_inshore_reset", "Reset")
                          ),
                          wellPanel(
                            sliderInput(
                              "sus_mort_offshore",
                              paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                              min = 0,
                              max = 1.0,
                              value = 0.0,
                              step = 0.000001,
                              width = "100%"
                            ),
                            actionButton("sus_mort_offshore_reset", "Reset")
                          )
                )
      )
    ),
    "Carnivore/scavenge feeding benthos" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "cb_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cb_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "cb_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cb_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Carnivorous zooplankton (e.g. squids)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "cz_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cz_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "cz_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cz_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Seabirds" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "sb_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sb_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sb_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sb_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Pinnipeds (seals)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "seal_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("seal_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "seal_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("seal_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Cetaceans" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "ceta_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("ceta_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "ceta_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("ceta_mort_offshore_reset", "Reset")
                )
      )
    ),
    "Macrophytes (kelp)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "kelp_mort_inshore",
                    paste(input$selectedMortGuild, " Onshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("kelp_mort_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "kelp_mort_offshore",
                    paste(input$selectedMortGuild, " Offshore Mortality Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 0.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("kelp_mort_offshore_reset", "Reset")
                )
    )
  )
)
    
}