# Function to create uptake rate UI


createUptakeRateUI <- function(model,input) {
  switch(
    input$selectedUptakeGuild,
    "Planktivorous fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "plank_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("plank_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "plank_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("plank_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Demersal fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "dem_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("dem_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "dem_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("dem_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Migratory fish" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "mig_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("mig_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "mig_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("mig_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Suspension/deposit feeding benthos" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(style = "overflow-y:scroll; max-height: 600px",
                          wellPanel(
                            sliderInput(
                              "sus_uptake_inshore",
                              paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                              min = 0,
                              max = 1.0,
                              value = 1.0,
                              step = 0.000001,
                              width = "100%"
                            ),
                            actionButton("sus_uptake_inshore_reset", "Reset")
                          ),
                          wellPanel(
                            sliderInput(
                              "sus_uptake_offshore",
                              paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                              min = 0,
                              max = 1.0,
                              value = 1.0,
                              step = 0.000001,
                              width = "100%"
                            ),
                            actionButton("sus_uptake_offshore_reset", "Reset")
                          )
                )
      )
    ),
    "Carnivore/scavenge feeding benthos" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "cb_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cb_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "cb_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cb_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Carnivorous zooplankton (e.g. squids)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "cz_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cz_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "cz_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("cz_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Seabirds" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "sb_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sb_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "sb_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("sb_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Pinnipeds (seals)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "seal_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("seal_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "seal_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("seal_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Cetaceans" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "ceta_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("ceta_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "ceta_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("ceta_uptake_offshore_reset", "Reset")
                )
      )
    ),
    "Macrophytes (kelp)" = fluidRow(
      wellPanel(style = "overflow-y:scroll; max-height: 600px",
                wellPanel(
                  sliderInput(
                    "kelp_uptake_inshore",
                    paste(input$selectedUptakeGuild, " Onshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("kelp_uptake_inshore_reset", "Reset")
                ),
                wellPanel(
                  sliderInput(
                    "kelp_uptake_offshore",
                    paste(input$selectedUptakeGuild, " Offshore Uptake Rate", sep=" "),
                    min = 0,
                    max = 1.0,
                    value = 1.0,
                    step = 0.000001,
                    width = "100%"
                  ),
                  actionButton("kelp_uptake_offshore_reset", "Reset")
                )
    )
  )
)
    
}