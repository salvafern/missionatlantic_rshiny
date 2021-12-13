
createGearDistributionPerHabitat	<- function( 
  region, 
  gearType, 
  jsCode,
  percentageInDefault, 
  percentageOutDefault, 
  percentageInRockDefault, 
  percentageInFineDefault, 
  percentageInMedDefault,
  percentageInCoarseDefault,
  percentageOffRockDefault, 
  percentageOffFineDefault, 
  percentageOffMedDefault,
  percentageOffCoarseDefault) {
  
  if(region=='North_Sea' || region=='Celtic_Sea'){
    switch(
    gearType,
    "Pelagic_Trawl+Seine" = fluidRow(  box(width = 10, title = "Habitats", style = "font-size:9px;",
                                           wellPanel(
                                             h5("Inshore-Offshore Split"),
                                             splitLayout( cellWidths = c("50%", "50%"),
                                                          numericInput("inshorePercentagePel", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                          numericInput("offshorePercentagePel", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                             )),
                                           splitLayout(
                                             wellPanel(
                                               verticalLayout(
                                                 useShinyjs(),
                                                 extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                 h5("Inshore Habitat Split"),
                                                 numericInput("percentagePelInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                                 numericInput("percentagePelInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                                 numericInput("percentagePelInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                                 verticalLayout(
                                                   useShinyjs(),
                                                   extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                   uiOutput("percentagePelInCoarse")
                                                 )
                                               )),
                                             wellPanel(
                                               h5("Offshore Habitat Split"),
                                               verticalLayout(
                                                 useShinyjs(),
                                                 extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                 numericInput("percentagePelOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                                 numericInput("percentagePelOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                                 numericInput("percentagePelOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                                 verticalLayout(
                                                   useShinyjs(),
                                                   extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                   uiOutput("percentagePelOffCoarse")
                                                 )
                                               )),
                                             actionButton("pelGearPerHab_reset", "Reset")
                                           ),
    )),
    "Sandeel+sprat_trawl(Otter30-70mm+TR3)" = fluidRow( box(width = 12, title = "Habitats", style = "font-size:9px;",
                                                            wellPanel(
                                                              h5("Inshore-Offshore Split"),
                                                              splitLayout( cellWidths = c("50%", "50%"),
                                                                           numericInput("inshorePercentageSandeel", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                                           numericInput("offshorePercentageSandeel", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                                              )),
                                                            splitLayout(
                                                              wellPanel(
                                                                verticalLayout(
                                                                  h5("Inshore Habitat Split"),
                                                                  numericInput("percentageSandeelInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                                                  numericInput("percentageSandeelInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                                                  numericInput("percentageSandeelInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                                                  verticalLayout(
                                                                    useShinyjs(),
                                                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                                    uiOutput("percentageSandeelInCoarse")
                                                                  )
                                                                )),
                                                              wellPanel(
                                                                h5("Offshore Habitat Split"),
                                                                verticalLayout(
                                                                  numericInput("percentageSandeelOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                                                  numericInput("percentageSandeelOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                                                  numericInput("percentageSandeelOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                                                  verticalLayout(
                                                                    useShinyjs(),
                                                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                                    uiOutput("percentageSandeelOffCoarse")
                                                                  )
                                                                )),
                                                              actionButton("sandeelGearPerHab_reset", "Reset")
                                                            ),
    )),
    "Otter30-70mm+TR3(sandeel+sprat)" = fluidRow( box(width = 12, title = "Habitats", style = "font-size:9px;",
                                                      wellPanel(
                                                        h5("Inshore-Offshore Split"),
                                                        splitLayout( cellWidths = c("50%", "50%"),
                                                                     numericInput("inshorePercentageOtter", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                                     numericInput("offshorePercentageOtter", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                                        )),
                                                      splitLayout(
                                                        wellPanel(
                                                          verticalLayout(
                                                            h5("Inshore Habitat Split"),
                                                            numericInput("percentageOtterInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                                            numericInput("percentageOtterInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                                            numericInput("percentageOtterInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                                            verticalLayout(
                                                              useShinyjs(),
                                                              extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                              uiOutput("percentageOtterInCoarse")
                                                            )
                                                          )),
                                                        wellPanel(
                                                          h5("Offshore Habitat Split"),
                                                          verticalLayout(
                                                            numericInput("percentageOtterOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                                            numericInput("percentageOtterOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                                            numericInput("percentageOtterOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                                            verticalLayout(
                                                              useShinyjs(),
                                                              extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                              uiOutput("percentageOtterOffCoarse")
                                                            )
                                                          )),
                                                        actionButton("otterGearPerHab_reset", "Reset")
                                                      ),
                                                      
    )),     
    
    
    "Longline_mackerel" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                       wellPanel(
                                         h5("Inshore-Offshore Split"),
                                         splitLayout( cellWidths = c("50%", "50%"),
                                                      numericInput("inshorePercentageLonMack", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                      numericInput("offshorePercentageLonMack", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                         )),
                                       splitLayout(
                                         wellPanel(
                                           verticalLayout(
                                             h5("Inshore Habitat Split"),
                                             numericInput("percentageLonMackInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                             numericInput("percentageLonMackInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                             numericInput("percentageLonMackInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                             verticalLayout(
                                               useShinyjs(),
                                               extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                               uiOutput("percentageLonMackInCoarse")
                                             )
                                           )),
                                         wellPanel(
                                           h5("Offshore Habitat Split"),
                                           verticalLayout(
                                             numericInput("percentageLonMackOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                             numericInput("percentageLonMackOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                             numericInput("percentageLonMackOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                             verticalLayout(
                                               useShinyjs(),
                                               extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                               uiOutput("percentageLonMackOffCoarse")
                                             )
                                           )),
                                         actionButton("lonMackGearPerHab_reset", "Reset")
                                       ),
    )),
    "Beam_Trawl_BT1+BT2" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                        wellPanel(
                                          h5("Inshore-Offshore Split"),
                                          splitLayout( cellWidths = c("50%", "50%"),
                                                       numericInput("inshorePercentageBeamTrawl", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                       numericInput("offshorePercentageBeamTrawl", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                          )),
                                        splitLayout(
                                          wellPanel(
                                            verticalLayout(
                                              h5("Inshore Habitat Split"),
                                              numericInput("percentageBeamTrawlInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                              numericInput("percentageBeamTrawlInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                              numericInput("percentageBeamTrawlInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageBeamTrawlInCoarse")
                                              )
                                            )),
                                          wellPanel(
                                            h5("Offshore Habitat Split"),
                                            verticalLayout(
                                              numericInput("percentageBeamTrawlOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                              numericInput("percentageBeamTrawlOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                              numericInput("percentageBeamTrawlOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageBeamTrawlOffCoarse")
                                              )
                                            )),
                                          actionButton("beamTrawlGearPerHab_reset", "Reset")
                                        ),
    )),
    "Demersal_Seine" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                    wellPanel(
                                      h5("Inshore-Offshore Split"),
                                      splitLayout( cellWidths = c("50%", "50%"),
                                                   numericInput("inshorePercentageDemSeine", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                   numericInput("offshorePercentageDemSeine", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                      )),
                                    splitLayout(
                                      wellPanel(
                                        verticalLayout(
                                          h5("Inshore Habitat Split"),
                                          numericInput("percentageDemSeineInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                          numericInput("percentageDemSeineInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                          numericInput("percentageDemSeineInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                          verticalLayout(
                                            useShinyjs(),
                                            extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                            uiOutput("percentageDemSeineInCoarse")
                                          )
                                        )),
                                      wellPanel(
                                        h5("Offshore Habitat Split"),
                                        verticalLayout(
                                          numericInput("percentageDemSeineOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                          numericInput("percentageDemSeineOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                          numericInput("percentageDemSeineOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                          verticalLayout(
                                            useShinyjs(),
                                            extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                            uiOutput("percentageDemSeineOffCoarse")
                                          )
                                        )),
                                      actionButton("demSeineGearPerHab_reset", "Reset")
                                    ),
                                    
    )),
    "Demersal_Otter_Trawl_TR1" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                              wellPanel(
                                                h5("Inshore-Offshore Split"),
                                                splitLayout( cellWidths = c("50%", "50%"),
                                                             numericInput("inshorePercentageDemOtter", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                             numericInput("offshorePercentageDemOtter", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                                )),
                                              splitLayout(
                                                wellPanel(
                                                  verticalLayout(
                                                    h5("Inshore Habitat Split"),
                                                    numericInput("percentageDemOtterInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                                    numericInput("percentageDemOtterInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                                    numericInput("percentageDemOtterInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                                    verticalLayout(
                                                      useShinyjs(),
                                                      extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                      uiOutput("percentageDemOtterInCoarse")
                                                    )
                                                  )),
                                                wellPanel(
                                                  h5("Offshore Habitat Split"),
                                                  verticalLayout(
                                                    numericInput("percentageDemOtterOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                                    numericInput("percentageDemOtterOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                                    numericInput("percentageDemOtterOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                                    verticalLayout(
                                                      useShinyjs(),
                                                      extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                      uiOutput("percentageDemOtterOffCoarse")
                                                    )
                                                  )),
                                                actionButton("demOtterGearPerHab_reset", "Reset")
                                              ),
                                              
    )),
    "Gill_Nets+Longline_demersal" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                                 wellPanel(
                                                   h5("Inshore-Offshore Split"),
                                                   splitLayout( cellWidths = c("50%", "50%"),
                                                                numericInput("inshorePercentageGillNet", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                                numericInput("offshorePercentageGillNet", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                                   )),
                                                 splitLayout(
                                                   wellPanel(
                                                     verticalLayout(
                                                       h5("Inshore Habitat Split"),
                                                       numericInput("percentageGillNetInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                                       numericInput("percentageGillNetInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                                       numericInput("percentageGillNetInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                                       verticalLayout(
                                                         useShinyjs(),
                                                         extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                         uiOutput("percentageGillNetInCoarse")
                                                       )
                                                     )),
                                                   wellPanel(
                                                     h5("Offshore Habitat Split"),
                                                     verticalLayout(
                                                       numericInput("percentageGillNetOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                                       numericInput("percentageGillNetOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                                       numericInput("percentageGillNetOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                                       verticalLayout(
                                                         useShinyjs(),
                                                         extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                         uiOutput("percentageGillNetOffCoarse")
                                                       )
                                                     )),
                                                   actionButton("gillNetGearPerHab_reset", "Reset")
                                                 ),
    )),
    "Beam_Trawl_shrimp" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                       wellPanel(
                                         h5("Inshore-Offshore Split"),
                                         splitLayout( cellWidths = c("50%", "50%"),
                                                      numericInput("inshorePercentageBeamShrimp", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                      numericInput("offshorePercentageBeamShrimp", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                         )),
                                       splitLayout(
                                         wellPanel(
                                           verticalLayout(
                                             h5("Inshore Habitat Split"),
                                             numericInput("percentageBeamShrimpInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                             numericInput("percentageBeamShrimpInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                             numericInput("percentageBeamShrimpInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                             verticalLayout(
                                               useShinyjs(),
                                               extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                               uiOutput("percentageBeamShrimpInCoarse")
                                             )
                                           )),
                                         wellPanel(
                                           h5("Offshore Habitat Split"),
                                           verticalLayout(
                                             numericInput("percentageBeamShrimpOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                             numericInput("percentageBeamShrimpOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                             numericInput("percentageBeamShrimpOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                             verticalLayout(
                                               useShinyjs(),
                                               extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                               uiOutput("percentageBeamShrimpOffCoarse")
                                             )
                                           )),
                                         actionButton("beamShrimpGearPerHab_reset", "Reset")
                                       ),
                                       
    )),
    "Nephrops_Trawl_TR2" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                        wellPanel(
                                          h5("Inshore-Offshore Split"),
                                          splitLayout( cellWidths = c("50%", "50%"),
                                                       numericInput("inshorePercentageNephropsTR2", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                       numericInput("offshorePercentageNephropsTR2", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                          )),
                                        splitLayout(
                                          wellPanel(
                                            verticalLayout(
                                              h5("Inshore Habitat Split"),
                                              numericInput("percentageNephropsTR2InRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                              numericInput("percentageNephropsTR2InFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                              numericInput("percentageNephropsTR2InMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageNephropsTR2InCoarse")
                                              )
                                            )),
                                          wellPanel(
                                            h5("Offshore Habitat Split"),
                                            verticalLayout(
                                              numericInput("percentageNephropsTR2OffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                              numericInput("percentageNephropsTR2OffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                              numericInput("percentageNephropsTR2OffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageNephropsTR2OffCoarse")
                                              )
                                            )),
                                          actionButton("nephropsTR2GearPerHab_reset", "Reset")
                                        ),
                                        
    )),
    "Nephrops_Trawl_TR3" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                        wellPanel(
                                          h5("Inshore-Offshore Split"),
                                          splitLayout( cellWidths = c("50%", "50%"),
                                                       numericInput("inshorePercentageNephropsTR3", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                       numericInput("offshorePercentageNephropsTR3", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                          )),
                                        splitLayout(
                                          wellPanel(
                                            verticalLayout(
                                              h5("Inshore Habitat Split"),
                                              numericInput("percentageNephropsTR3InRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                              numericInput("percentageNephropsTR3InFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                              numericInput("percentageNephropsTR3InMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageNephropsTR3InCoarse")
                                              )
                                            )),
                                          wellPanel(
                                            h5("Offshore Habitat Split"),
                                            verticalLayout(
                                              numericInput("percentageNephropsTR3OffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                              numericInput("percentageNephropsTR3OffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                              numericInput("percentageNephropsTR3OffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                              verticalLayout(
                                                useShinyjs(),
                                                extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                                uiOutput("percentageNephropsTR3OffCoarse")
                                              )
                                            )),
                                          actionButton("nephropsTR3GearPerHab_reset", "Reset")
                                        ),
                                        
    )),
    "Creels" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                            wellPanel(
                              h5("Inshore-Offshore Split"),
                              splitLayout( cellWidths = c("50%", "50%"),
                                           numericInput("inshorePercentageCreels", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                           numericInput("offshorePercentageCreels", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                              )),
                            splitLayout(
                              wellPanel(
                                verticalLayout(
                                  h5("Inshore Habitat Split"),
                                  numericInput("percentageCreelsInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                  numericInput("percentageCreelsInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                  numericInput("percentageCreelsInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                  verticalLayout(
                                    useShinyjs(),
                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                    uiOutput("percentageCreelsInCoarse")
                                  )
                                )),
                              wellPanel(
                                h5("Offshore Habitat Split"),
                                verticalLayout(
                                  numericInput("percentageCreelsOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                  numericInput("percentageCreelsOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                  numericInput("percentageCreelsOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                  verticalLayout(
                                    useShinyjs(),
                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                    uiOutput("percentageCreelsOffCoarse")
                                  )
                                )),
                              actionButton("creelsGearPerHab_reset", "Reset")
                            ),
                            
    )),
    "Mollusc_Dredge" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                    wellPanel(
                                      h5("Inshore-Offshore Split"),
                                      splitLayout( cellWidths = c("50%", "50%"),
                                                   numericInput("inshorePercentageMollusc", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                   numericInput("offshorePercentageMollusc", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                      )),
                                    splitLayout(
                                      wellPanel(
                                        verticalLayout(
                                          h5("Inshore Habitat Split"),
                                          numericInput("percentageMolluscInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                          numericInput("percentageMolluscInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                          numericInput("percentageMolluscInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                          verticalLayout(
                                            useShinyjs(),
                                            extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                            uiOutput("percentageMolluscInCoarse")
                                          )
                                        )),
                                      wellPanel(
                                        h5("Offshore Habitat Split"),
                                        verticalLayout(
                                          numericInput("percentageMolluscOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                          numericInput("percentageMolluscOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                          numericInput("percentageMolluscOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                          verticalLayout(
                                            useShinyjs(),
                                            extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                            uiOutput("percentageMolluscOffCoarse")
                                          )
                                        )),
                                      actionButton("molluscGearPerHab_reset", "Reset")
                                    ),
                                    
    )),
    "Whaler" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                            wellPanel(
                              h5("Inshore-Offshore Split"),
                              splitLayout( cellWidths = c("50%", "50%"),
                                           numericInput("inshorePercentageWhaler", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                           numericInput("offshorePercentageWhaler", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                              )),
                            splitLayout(
                              wellPanel(
                                verticalLayout(
                                  h5("Inshore Habitat Split"),
                                  numericInput("percentageWhalerInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                  numericInput("percentageWhalerInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                  numericInput("percentageWhalerInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                  verticalLayout(
                                    useShinyjs(),
                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                    uiOutput("percentageWhalerInCoarse")
                                  )
                                )),
                              wellPanel(
                                h5("Offshore Habitat Split"),
                                verticalLayout(
                                  numericInput("percentageWhalerOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                  numericInput("percentageWhalerOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                  numericInput("percentageWhalerOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                  verticalLayout(
                                    useShinyjs(),
                                    extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                    uiOutput("percentageWhalerOffCoarse")
                                  )
                                )),
                              actionButton("whalerGearPerHab_reset", "Reset")
                            ),
                            
    )),
    "KelpHarvester" = fluidRow(box(width = 12, title = "Habitats", style = "font-size:9px;",
                                   wellPanel(
                                     h5("Inshore-Offshore Split"),
                                     splitLayout( cellWidths = c("50%", "50%"),
                                                  numericInput("inshorePercentageKelp", "InShore %",percentageInDefault,min = 0, max = 100, step = 0.01, width = '40%'),
                                                  numericInput("offshorePercentageKelp", "OffShore %",percentageOutDefault,min = 0, max = 100, step = 0.01, width = '40%')
                                     )),
                                   splitLayout(
                                     wellPanel(
                                       verticalLayout(
                                         h5("Inshore Habitat Split"),
                                         numericInput("percentageKelpInRockInput", "Inshore rock %",percentageInRockDefault,min = 0, max = 100, step = 0.01, width = '65%'),
                                         numericInput("percentageKelpInFineInput", "Inshore fine %",percentageInFineDefault,width = '50%'),
                                         numericInput("percentageKelpInMedInput", "Inshore Medium %",percentageInMedDefault,width = '50%'),
                                         verticalLayout(
                                           useShinyjs(),
                                           extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                           uiOutput("percentageKelpInCoarse")
                                         )
                                       )),
                                     wellPanel(
                                       h5("Offshore Habitat Split"),
                                       verticalLayout(
                                         numericInput("percentageKelpOffRockInput", "Offshore rock %",percentageOffRockDefault,min = 0, max = 100, step = 0.01,width = '65%'),
                                         numericInput("percentageKelpOffFineInput", "Offshore fine %",percentageOffFineDefault,width = '50%'),
                                         numericInput("percentageKelpOffMedInput", "Offshore Medium %",percentageOffMedDefault,width = '50%'),
                                         verticalLayout(
                                           useShinyjs(),
                                           extendShinyjs(text = jsCode,functions = c("backgroundCol")),
                                           uiOutput("percentageKelpOffCoarse")
                                         )
                                       )),
                                     actionButton("kelpGearPerHab_reset", "Reset")
                                   ),
    ))
    )
  } else {
  ## Not other regions yet
  }
}