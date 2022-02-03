library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(tidyr)
library(uuid)
library(ggpubr)
theme_set(theme_pubr())
#library(shinyAce)
#library(sendmailR)

#library(promises)
#library(future)
#plan(multisession)

source("createEcoplots.R")
source("createCatchPlotPerGuild.R")
source("createCatchPlotPerGear.R")
source("createEDriversPlots.R")
source("e2e_compare_runs_bar_gg.R")
source("compareTwoRunsAAM.R")
source("compareTwoRunsCatch.R")
source("createGearDistributionPerHabitat.R")
source("fishingActivity.R")
source("seabedAbrasian.R")
source("discardPerGear.R")
source("makeScenarioAdjustments.R")
source("gearHabitatDist.R")
options(shiny.fullstacktrace = TRUE)
jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

ui <- navbarPage(
  "StrathE2E-App",
  theme = shinytheme("cerulean"), #"strathe2e.css", 
             tabPanel(
               title = "Home",
               tags$style(HTML(".irs-grid-text {font-size: 10pt;}")),
               tags$style(HTML(".sup {vertical-align: super; font-size: smaller;}")),
               #tags$style(HTML('#runBaseline{background-color: #008CBA;}')),
               tags$style(HTML(".mytooltip {position: relative; display: inline-block;}")),
               tags$style(HTML(".mytooltip .tooltiptext {visibility: hidden;width: 200px;background-color: #008CBA;color: #fff;text-align: center;border-radius: 6px;padding: 5px 0;position: absolute;z-index: 1;}")),
               tags$style(HTML(".mytooltip:hover .tooltiptext {visibility: visible;}")),
               tags$style(HTML(".mytooltip {text-decoration:underline; text-decoration-style: dotted;}")),
               tags$style(HTML("input[type=number] {-moz-appearance:textfield;} input[type=number]::{-moz-appearance:textfield;} input[type=number]::-webkit-outer-spin-button, input[type=number]::-webkit-inner-spin-button { -webkit-appearance: none; margin: 0;}")),
               
                fluidRow(
                  column(
                    6,
                    h3("Marine Ecosystem Modelling Tool"),
                    br(),
                    #HTML("<p style = \"font-family: 'calibri'; font-si16pt \" class=\"mytooltip\">TEXT<span class=\"tooltiptext\">Tooltip text</span></p>"),
                    p(
                     "Our seas and oceans are being subjected to a wide range of pressures, from warming, fishing, and pollution by nutrients, plastic particles and litter. However, these pressures act together in complex ways. How can we work out the most effective strategies for alleviating their impacts on the sea while still being able to harvest the food that we need?",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                   p(
                     "One way is through 'what-if?' experiments with computer simulation models. Computer models of the oceans and seas underpin advice on fisheries management, regulation of pollution, and making the case for reducing fossil fuel consumption. These models are highly technical, but we'd like everyone to have the chance to play with one and do their own experiments. That's what this website is all about.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                   p(
                     "We've designed a model of food webs in continental shelf seas, called \"StrathE2E\". It's free for anyone to access, but it needs some specialist knowledge to use it. So we've built this website which makes it possible for anyone to be a marine modeller and do their own experiments.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                     p(
                       "Just follow the workflow  through the site - pick a geographic region from the ones we've configured (we'll be adding more over time), explore the results from a model run, then change the inputs and see how they affect the outputs.",
                       style = "font-family: 'calibri'; font-si16pt"
                     )
                  ),
                 column(
                   6,
                   img(src = "strathe2e_collage.png", width = '85%',style = "text-align: center; padding:10px")
                   # h6(
                   #   "Text for pic here"
                   # )
                 )),
               fluidRow(
                 HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:10px;color:blue \"><b>For enquiries please contact</b>: Michael Heath  &nbsp;&nbsp;&nbsp;&nbsp; <b>Email Address</b> m.heath@strath.ac.uk"),
               ),
               fluidRow(
                 column(
                   4,
                   tags$a(img(src = "Mission Atlantic Logo Col-01.png", width = '60%',style = "text-align: center; padding:10px;10px;10px;10px;"),href="https://missionatlantic.eu/", target="_blank")
                 ),
                 column(
                   4,
                   img(src = "Mission Atlantic _EC_disclaimer.png", width = '100%', style = "padding:25px;1px;")
                 ),
                  column(
                    4,
                    img(src = "strath_fullcolour.jpg", width = '45%',style = "padding:1px;1px;1px;30px;")
                  ) 
               )
               ),
          navbarMenu(
                 "About",
             tabPanel(
               title = "About StrathE2E",
               fluidRow(
                 column(
                   7,
                   h3("Background to StrathE2E"),
                   HTML("<br><p style = \"font-family: 'calibri'; font-si16pt \">StrathE2E is a computer simulation model of marine ecosystems. A first version of the model was developed during an EU 6th Framework project (RECLAM, <a href='https://cordis.europa.eu/project/id/44133/reporting'>REsolving CLimAtic IMpacts on fish stocks, 2007-2009</a>). The prototype was further developed and elaborated during a succession of EU and UK nationally funded projects:</p>"),
                   HTML("<ul><li>7th Framework <a href='https://cordis.europa.eu/project/id/264933/reporting'>EURO_BASIN (European Union Basin-scale Analysis, Synthesis and Integration, 2010-2014)</a>.</li></ul>"),
                   HTML("<ul><li>UK Natural Environment Research Council <a href='https://www.marine-ecosystems.org.uk/Home'>MERP (Marine Ecosystems Research Programme, 2014-2019)</a>.</li></ul>"),
                   HTML("<ul><li>Fisheries Innovation Scotland project FIS003 (<a href='https://fiscot.org/wp-content/uploads/2019/06/FIS003.pdf'>Modelling the whole-ecosystem impacts of trawling</a> , 2015).</li></ul>"),
                   HTML("<ul><li>Horizon 2020 <a href='http://www.discardless.eu/'>DiscardLess (Strategies for the gradual elimination of discards in European fisheries</a>, 2015-2019).</li></ul>"),
                   HTML("<p style = \"font-family: 'calibri'; font-si16pt \">The culmination of this development effort was release of the open-access software package <a href='https://CRAN.R-project.org/package=StrathE2E2'>StrathE2E2</a> for the <a href='https://www.r-project.org/'>R statistical programming environment</a> in 2020. You can download an open-access article about the package from <a href='https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13510'>here</a>.</p>"),
                   HTML("<p style = \"font-family: 'calibri'; font-si16pt \">An inventory of publications about or using StrathE2E is available <a href='https://marineresourcemodelling.gitlab.io/resources/StrathE2E2/documents/publications/StrathE2E_Publications.pdf'>here</a>.</p>"),
                   HTML("<p style = \"font-family: 'calibri'; font-si16pt \">This website is an interactive interface to the StrathE2E2 software package, built using <a href='https://shiny.rstudio.com/'>Rshiny</a>. Following a 4-step process you can select a model region, run the model and explore the results. Then, build your own scenario of fishing and environmental conditions, and discover how these affect the ecosystem</p>"),
                   )
               )
             ),
             tabPanel(
               title = "Model Overview",
               fluidRow(
                 column(
                   12,
                  h3("StrathE2E - some details of what it includes"),
                  br(),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \">StrathE2E is a so-called end-to-end model – it aims to represent the entire interconnected marine ecosystem from physics and chemistry, through microbes and plankton to whales and fisheries in continental shelf regions. To make this feasible, we simplify the ecology - all the plants and animals in the sea are grouped together into what we call <a href='#diagram2'>'guilds'</a> of species that have similar properties.</p>"),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \">The region covered by each model is divided into a shallow inshore and a deeper offshore zone. The water column in the offshore zone is further divided into an upper (surface) layer, and a lower (deep) layer. The seabed in each zone is divided into up to four different sediment habitat types e.g.muddy, sandy, gravel, rocky. See <a href='#diagram3'>spatial structure</a> diagram"),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Fisheries in StrathE2E are represented by a separate sub-model which is <a href='#diagram1'>connected</a> to the ecology part. In the sub-model, all of fishing gears used in a region are grouped together into up to 12 different types defined by their effectiveness at catching each of the ecology guilds, the spatial distribution of their activity, <span class=\"mytooltip\">seabed abrasion<span class=\"tooltiptext\">“Seabed abrasion” refers to the scraping or ploughing effects of dragging fishing gear along the seabed. The disturbance re-suspends sediment, releases nutrients and causes mortality  of seabed animals.</span></span> rates, and <span class=\"mytooltip\">discarding<span class=\"tooltiptext\">“Discards” are the components of catch which are returned to the sea due to being of no commercial value, or legal restrictions on size or catch limits. In the model, all discards are assumed to be dead.</span></span> patterns.</p>"),
                  br(),
                  br(),
                  img(src = "guilds3.svg", width = '75%', style = "display: block !important; margin: 0 auto !important; float:none !important;" ,id="diagram2"),
                  br(),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt; \"><b>Ecological guilds:</b> <i>Ecological guilds or classes of dead and living material included in the StrathE2E model</i>"),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  img(src = "schematic2.svg", width = '70%', style = "display: block; margin-left: auto; margin-right: auto;" ,id="diagram3"),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \"><b>The spatial structure:</b> <i>Ocean volumes and seafloor habitats in the model. StrathE2E is built around a simplified spatial structure which represents shelf seas. The exact geographical extent of the volumes and habitats will vary for each implementation depending on local conditions. You can see the locations of the habitats and shore zones under the “Setup Model” tab for each model implementation. These spatial units are connected to each other and to boundaries as shown to the right. The volumes connected to each spatial component are highlighted in blue.</i>"),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  br(),
                  img(src = "Temperate model joined2.svg", width = '50%', style = "display: block; margin-left: auto; margin-right: auto;",id="diagram1"),
                  br(),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \"><b>The sub-models:</b> <i>StrathE2E comprises two parts – a model of the marine ecology, and a model of fishing fleets.</i>")
                  )
                 )
               ),
             tabPanel(
               title = "How the Model Works",
               fluidRow(
                 column(
                   8,
                   h3("StathE2E - some technical details"),
                   p(
                     "StrathE2E is a set of interconnected mathematical equations which are solved by the computer programme to calculate at daily intervals:",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),

                   tags$ul(
                     HTML("<li style = \"font-family: 'calibri'; font-si16pt \">The quantities of <span class=\"mytooltip\">dissolved nutrients<span class=\"tooltiptext\">“Dissolved nutrients” refers are naturally occurring inorganic compounds such as nitrate and ammonia which are essential for the growth of algae.</span></span>, detritus, the guilds of plants and animals in each of the spatial zones and layers.</li>"),
                     tags$li("How much each of the guilds consumes as nutrient or food by preying on other guilds.",style = "font-family: 'calibri'; font-si16pt"), 
                     tags$li("How much nutrient is returned to the sea by excretion",style = "font-family: 'calibri'; font-si16pt"),
                     tags$li("How much nutrient and plankton is carried into and out of the model region, and between inshore and offshore zones, by water currents, and between inshore and offshore zones",style = "font-family: 'calibri'; font-si16pt"),
                     tags$li("The movements of fish, birds, seals and whales between inshore and offshore zones by active migration, motivated by the concentrations of their food.",style = "font-family: 'calibri'; font-si16pt"),
                     tags$li("How much nutrient and sediment are stirred up from the seabed by fishing gears",style = "font-family: 'calibri'; font-si16pt"),
                     HTML("<li style = \"font-family: 'calibri'; font-si16pt \">How much of each guild is caught by the fishing gears, and what proportion is <span class=\"mytooltip\">landed<span class=\"tooltiptext\">“Landings” are the components of catch which is brought ashore to be sold</span></span> or <span class=\"mytooltip\">discarded<span class=\"tooltiptext\">“Discards” are the components of catch which are returned to the sea due to being of no commercial value, or legal restrictions on size or catch limits. In the model, all discards are assumed to be dead.</span></span></li>")
                   ),
                   HTML("<p style = \"font-family: 'calibri'; font-si16pt \">The model tracks the changes in quantities of all the guilds and the flows between them in terms of nitrogen content. The units of the output quantities are milli-Moles (mM) of nitrogen per m<sup>2</sup> or per m<sup>3</sup>. Roughly, 1 mM nitrogen per m<sup>2</sup> is equivalent to 500 kg of live weight per km<sup>2</sup>, depending on the guilds. </p>"),
                   p("The inputs to these calculations are:", style = "font-family: 'calibri'; font-si16pt"),
                   p("1) environmental data to the ecology part of the model: temperature, sunlight intensity, water currents, mixing and waves, and external nutrient inputs from rivers and in rainfall.",style = "font-family: 'calibri'; font-si16pt"),
                   HTML("<p style = \"font-family: 'calibri'; font-si16pt \">2) fishing gear properties to the fishing fleet sub-model: <span class=\"mytooltip\">selectivity<span class=\"tooltiptext\">“Selectivity” refers to the efficiency with which a given gear catches each of the guilds in the ecology model.</span></span> patterns and activity rates of each of the fishing gears, and their distribution over different seabed sediment types, <span class=\"mytooltip\">seabed abrasion<span class=\"tooltiptext\">“Seabed abrasion” refers to the scraping or ploughing effects of dragging fishing gear along the seabed. The disturbance re-suspends sediment, releases nutrients and causes mortality  of seabed animals.</span></span> rates, and <span class=\"mytooltip\">discarding<span class=\"tooltiptext\">“Discards” are the components of catch which are returned to the sea due to being of no commercial value, or legal restrictions on size or catch limits. In the model, all discards are assumed to be dead.</span></span> patterns."),
                   p(
                     "The equations in the model also have many \"parameters\" – these are constants that set the reactivity of the connections between all the components of the model.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                   p(
                     "The combination of spatial and seabed properties in the model setup, the parameters, and the environmental and fishery input data, are what makes the models for different regions different from one another.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                   p(
                     "The seabed sediment properties, environmental and fishing input data for a model region are gathered together from a range of sources including ocean circulation models, satellite observations, survey and monitoring databases, and research literature. Some of the parameter values can be set from experimental data, but others we have to estimate statistically so that model results agree as closely as possible to independent monitoring observations.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                   p(
                     "Operationally, we use the model to simulate the average annual cycles of changes in everything in the model, for a decade-or-so period of years. We do this by inputting annual cycles of average monthly values of the environmental data, and running the model over and over with these input data until the annual cycles in the outputs are stable from one year to the next. We call this a \"steady state\" which represents the \"climatology\" of the system for the period of years covered by the inputs, and should match the average conditions in the sea for any given day of the year.",
                     style = "font-family: 'calibri'; font-si16pt"
                   )
                 )
               )
             )
             # tabPanel(
             #   title = "How To Use This Website",
             #   fluidRow(
             #     column(
             #       6,
             #       h5("There are four stages to using this website:"),
             #       img(src = "Workflow2.svg", width = '150%'),#, style = "display: block; margin-left: auto; margin-right: auto;"),
             #       )
             #   )
             # )
             ),
  tabPanel(
    title = "How to...",
    fluidRow(
        h3("Navigating this website to generate your results"),
        img(src = "Workflow2.svg", width = '75%', style = "display: block; margin-left: auto; margin-right: auto;"),
    )
  ),
  navbarMenu(
    "Setup Model",
    tabPanel(
      title = "Select model region & time period",
      value="region",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "selectedlocation",
            h4("Select Region and Time Period"),
            choices
            = list("North_Sea","Celtic_Sea"),
            selected = "North_Sea"
          ),
          uiOutput("variant_dropdown"),
          uiOutput("textSpecificRegion"), 
          # h4("Run Model"),
          # actionButton("runBaseline", "Run Model",style="background-image: linear-gradient(#39a8e8, #39a8e8, 60%, #39a8e8);color: white;"),
          # uiOutput("textRunBaselineModel"),
          width = 3
        ),
        #Main Panel: plot map here in the future
        mainPanel(
          uiOutput("model_map"),  
          textOutput("textSelectRegion"),
          width = 9
        )
      )
    ),
    "----",
    "View Model Inputs:",
    tabPanel(
      title = "Environmental",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "edriverType",
            h4("Environmental input type"),
            choices
            = list(
              "Surface irradiance",
              "Susp.partic. matter",
              "Temperature",
              "Diffusivity gradient",
              "External Inflow",
              "River discharge",
              "Wave height",
              "Sediment disturbance",
              "Boundary nitrate",
              "Boundary ammonia",
              "Boundary phytoplankton",
              "Boundary detritus",
              "River nitrate",
              "River ammonia",
              "Atmospheric nitrate",
              "Atmospheric ammonia"
            ),
            selected = "Surface irradiance"
          ) , uiOutput("textEnvironmental")
        ),
        mainPanel(
          uiOutput("UiEdriver"))
      )
    ),
    tabPanel(
      title = "Fishery",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "fdriverType",
            h4("Fishery input type"),
            choices
            = list(
              "Activity",
              "Abrasion",
              "HarvestR",
              "Discards",
              "Offal"
            ),
            selected = "Activity"
          ),  uiOutput("textFishery")
        ),
        mainPanel(
          uiOutput("UiFdriver"))
      )),
    "----",
    #"Run Model",
    tabPanel(
      title = "Run Model",
      fluidRow(
        style='padding:10px;',
          column(
          6,
          h3("Run Baseline"),
          actionButton("runBaseline", "Run Baseline Model",style="background-image: linear-gradient(#39a8e8, #39a8e8, 60%, #39a8e8);color: white;"),
          tags$br(),
          tags$br(),
          uiOutput("textRunBaselineModel"),
        )
      )
    )
    ), 
  navbarMenu(
    "Model Results",
    tabPanel(
      title = "Download outputs",
      fluidRow(
        style='padding:10px;',
      #   column(
      #   6,
      #   h3("Run Baseline"),
      #   actionButton("runBaseline", "Run Baseline Model",style="background-image: linear-gradient(#39a8e8, #39a8e8, 60%, #39a8e8);color: white;"),
      #   tags$br(),
      #   tags$br(),
      #   uiOutput("textRunBaselineModel"),
      # ),
      column(
        6,
        h3("Download output from the baseline run"),
        useShinyjs(),
        div(
          id = "dwnbutton_b",
          downloadButton(
            "downloadData_baseline1",
            "Download output as csv files",
            disabled = "disabled"
          )
        ),
        tags$br(),
        uiOutput("textRunBaselineFiles"),
      ),
      column(
        6,
        h3("Download plots from the baseline run"),
        useShinyjs(),
        div(
          id = "dwnbutton_bp",
          downloadButton(
            "downloadData_baselinePlots",
            "Download plots as png files",
            disabled = "disabled"
          )
        ),
        tags$br(),
        uiOutput("textRunBaselinePlots")
      )
      )
    ),
    "----",
    "Plot Outputs:",
    tabPanel(
      title = "Ecological",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "outputEcoType",
            h4("Ecological Output Type"),
            choices
            = list("Nutrient_Phytoplankton", "Sediment", "Zooplankton" , 
                   "Fish" , "Benthos" , "Predators", "Corpse_Discard" , "Macrophyte"),
            selected = "Nutrient_Phytoplankton"
          ), uiOutput("textEco")
        ),
        mainPanel(
          uiOutput("uiEco"))
        )
    ),
    tabPanel(
      title = "Catch per guild",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "outputCatchType",
            h4("Guild"),
            choices
            = list(
              "Planktivorous fish",
              "Quota limited Demersal fish",
              "Non quota demersal fish",
              "Migratory fish",
              "Susp/deposit feeding benthos",
              "Carn/scavebge feeding benthos",
              "Pelagic invertebrates",
              "Birds",
              "Pinnipeds",
              "Cetaceans",
              "Macrophytes",
              "All guilds combined"
            ),
            selected = "Planktivorous fish"
          ), uiOutput("textCatchGuild")
        ),
        mainPanel(
          uiOutput("uiCatchGuild"))
      )
    ),
    tabPanel(
      title = "Catch per gear",
      sidebarLayout(
        sidebarPanel(
          uiOutput("gear_dropdown"),
          uiOutput("textCatchGear")
        ), 
        mainPanel(
          uiOutput("uiCatchGear"))
      )
    )
  ), 
  navbarMenu(
    "Setup Scenario ",
    "----",
    "Build your own scenario model by modifying the inputs:",
    tabPanel(
      title = "Temperature",
      fluidRow(column(
        width = 5,
        #offset = 2,
        wellPanel(
          sliderInput(
            "temperature",
            "Temperature to add/subtract:",
            min = -3,
            max = 3,
            value = 0.0,
            width = "100%"
          )
        )
      ),
      column(width = 5, wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Used the slider bar to add or subtract a temperature increment (°C) throughout the year in all layers and zones of the model."),
                                  HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">0 (zero) represents no change from the baseline model temperatures."))))
    ),
    tabPanel(
      title = "River and atmosphere nutrients",
      # fluidRow(column(width = 5, wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the atmospheric and river nutrient inputs by a constant factor throughout the year."),
      #                                      HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model inputs; 0.5 means that inputs are halved; 2 means that inputs are doubled."),
      #                                      HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Atmospheric inputs are deposited as dust or in rainfall, and originate from both natural processes (lightning and volcanic activity) and emissions from industry and burning of fossil fuels."),
      #                                      HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">River inputs are dissolved in river waters and originate from geological process and waste water discharges.")))),
      fluidRow(
        column(
          width = 5,
         # offset = 2,
         wellPanel(
          wellPanel(
            sliderInput(
              "atmnitrate",
              "Atmospheric Nitrate:",
              min = 0,
              max = 2.0,
              value = 1.0,
              step = 0.2,
              width = "100%"
            )
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
            )
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
            )
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
            )
          ))
        ),
        column(width = 5, wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the atmospheric and river nutrient inputs by a constant factor throughout the year."),
                                    HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model inputs; 0.5 means that inputs are halved; 2 means that inputs are doubled."),
                                    HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Atmospheric inputs are deposited as dust or in rainfall, and originate from both natural processes (lightning and volcanic activity) and emissions from industry and burning of fossil fuels."),
                                    HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">River inputs are dissolved in river waters and originate from geological process and waste water discharges.")))
      )
    ),
    tabPanel(
      title = "Fishing Activity",
      uiOutput("uiFishingActivity")
    ),
    tabPanel(title = "Seabed abrasion rate per gear",
      uiOutput("uiSeabedAbrasian")),
    tabPanel(
      title = "Guild discard rate per gear",
      sidebarLayout(sidebarPanel(
        selectInput(
          "selectedParameter",
          h4("Guilds"),
          choices
          = list(
            "Planktivorous fish",
            "Demersal fish",
            "Migratory fish",
            "Suspension/deposit feeding benthos",
            "Carnivore/scavenge feeding benthos",
            "Carnivorous zooplankton (e.g. squids)",
            "Seabirds",
            "Pinnipeds (seals)",
            "Cetaceans",
            "Macrophytes (kelp)"
          ),
          selected = "Pelagic"
        ), uiOutput("textDiscardGuild"),
        width = 3
      ), 
      mainPanel (uiOutput("ui")))
    ),
    tabPanel(
      title = "Gear activity distribution per habitat",
      h4("Gears"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("gear_forhabitat_dropdown"),
          uiOutput("textCatchPerHabitatGear"),
          width = 3
        ),
      mainPanel (uiOutput("uiGearHabDist"))
      )
  ),
    "----",
    #"Run scenario",
    tabPanel(
      title = "Run your scenario model",
      sliderInput(
        "year",
        "Year:",
        min = 1,
        max = 50,
        value = 5,
        width = "100%"
      ),
      fluidRow(
        style='padding:10px;',
        column(
          6,
          h3("Run Scenario"),
          actionButton("runScenario", "Run Scenario Model",style="background-image: linear-gradient(#39a8e8, #39a8e8, 60%, #39a8e8);color: white;"),
          tags$br(),
          tags$br(),
          uiOutput("textRunScenarioModel"),
        )
      ))
  ),
  navbarMenu(
    "Scenario Results",
    tabPanel(
      title = "Download your scenario outputs",
      # sliderInput(
      #   "year",
      #   "Year:",
      #   min = 1,
      #   max = 50,
      #   value = 5,
      #   width = "100%"
      # ),
      fluidRow(
        style='padding:10px;',
      # column(
      #   6,
      #   h3("Run Scenario"),
      #   actionButton("runScenario", "Run Scenario Model",style="background-image: linear-gradient(#39a8e8, #39a8e8, 60%, #39a8e8);color: white;"),
      #     tags$br(),
      #     tags$br(),
      #   uiOutput("textRunScenarioModel"),
      # ),
      column(
        6,
        h3("Scenario output"),
        useShinyjs(),
        div(
          id = "dwnbutton_s",
          downloadButton(
            "downloadData_scenario1",
            "Download output as csv files",
            disabled = "disabled"
          )
        ),
          tags$br(),
        uiOutput("textRunScenarioFiles"),
      )
      )),
    "----",
    "Compare scenario results with baseline",
    tabPanel(
      title = "Ecological",
      fluidRow(
      column(5,
      uiOutput("textCompAAM"),
      wellPanel(
        sliderInput(
          "axisMaxPos",
          "x axis scale range:",
          min = -300,
          max = 300,
          value = c(-40, 40),
          width = "100%"
        )
      )),
      column(
        6,
        plotOutput("e2e_compare_runs_bar_aam"),
        useShinyjs(),
        div(
          id = "dwnbutton_bm",
          downloadButton(
            "downloadData_biomassComp",
            "Download biomass comparison plot",
            disabled = "disabled"
          )
        ),
        div(
          id = "dwnbutton_bmd",
          downloadButton(
            "downloadData_biomassCompData",
            "Download biomass comparison data",
            disabled = "disabled"
          )
        )
      )
      )
    ),
    tabPanel(
      title = "Fishery catches",
      fluidRow(
        column(5,
        uiOutput("textCompFishCatch"),
        wellPanel(
          sliderInput(
            "axisMaxPosCatch",
            "x axis scale range:",
            min = -300,
            max = 300,
            value = c(-40, 40),
            width = "100%"
          )
        )),
        column(
          5,
          plotOutput("e2e_compare_runs_bar_catch"),
          useShinyjs(),
          div(
            id = "dwnbutton_catch",
            downloadButton(
              "downloadData_catchComp",
              "Download catch comparison plot",
              disabled = "disabled"
            )
          ),
          div(
            id = "dwnbutton_catchdata",
            downloadButton(
              "downloadData_catchCompData",
              "Download catch comparison data",
              disabled = "disabled"
            )
          )
        )
      )
    )
    # tabPanel(title = "Yield curves - example only", fluidRow(
    #   column(
    #     width = 11,
    #     offset = 0.5,
    #     fluidRow(
    #       h4("Run a set of models to generate fishery yield curve data for either planktivorous or demersal fish.",style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       p(
    #         "Perform a set of StrathE2E model runs along a sequence of values of either planktivorous or demersal fish harvest ratio multiplier, saving the annual average whole-domain biomass, annual landings and annual discards of all exploitable food web guilds from each run plus the annual average biomasses of all the other living components of the food web.",
    #         style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"
    #       ),
    #       h5("Usage",style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       code("e2e_run_ycurve( model,selection, nyears = 50, HRvector = c(0, 0.5, 1, 1.5, 2, 2.5, 3), HRfixed = 1, csv.output = FALSE)"
    #       ,style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       h5("Details", style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       p(
    #         "The baseline for the sequence of runs (harvest ratio multiplier = 1.0) is a model name and variant as loaded by the e2e_read() function. The planktivorous or demersal fish yield curve can be generated for a given fixed setting of the other (demersal or planktivorous) fish harvest ratio multiplier (default = 1.0). All other conditions are held constant as in the baseline model configuration. The yield curve represents the catch that would be generated from the stationary state of the model attained with long-term repeating annual cycles of all driving data. Hence it is important that each simulation is run for long enough that the model attains its stationary state, which may be some distance from the baseline model initial conditions. It is recommended that each run is at least 50 years. The data on annual average biomass and annual integrated catches stored in the returned data object (and optionally a csv file) can subsequently be plotted using the function e2e_plot_ycurve(). Users can easily plot any of the other saved data using their own plotting code. If csv output is selected then the resulting files in the current user results folder have names Yield_curve_data_PFHRmult-*.csv or Yield_curve_data_DFHRmult-*.csv, depending on the 'selection' argument, and * represents the model.ident text set in the prior e2e_read() function call.",
    #         style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"
    #       ),
    #       h5("Example",style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       br(),
    #       code("# Load the 1970-1999 version of the North Sea model supplied with the package :"),
    #       br(),
    #       code("model <- e2e_read(\"North_Sea\", \"1970-1999\", model.ident=\"70-99base\")"),
    #       br(),
    #       code("# In this example csv output is directed to a temporary folder since results.path os not set."),
    #       br(),
    #       code("# In this illustrative example the StrathE2E() model is run for only 3 years to enable quick"),
    #       br(),
    #       code("# return of results. In a real simulation nyear would be at least 50."),
    #       br(),
    #       code("# This example illustrates that the vector of planktivorous fish harvest ratio multiplers"),
    #       br(),
    #       code("# does not have to be evenly spaced."),
    #       br(),
    #       code("hr <- c(0,0.5,0.75,1.0,1.25,2.0,3.0)"),
    #       br(),
    #       code("pf_yield_data <- e2e_run_ycurve(model,selection=\"PLANKTIV\", nyears=3, HRvector=hr, HRfixed=1, csv.output=FALSE)"),
    #       br(),
    #       code("names(pf_yield_data)"),
    #       br(),
    #       code("# Plotting the results..."),
    #       br(),
    #       code("# The planktivorous fish yield curve can be plotted using the function:"),
    #       br(),
    #       code("e2e_plot_ycurve(model, selection=\"PLANKTIV\", results=pf_yield_data,"),
    #       br(),
    #       code("title=\"Planktivorous yield with baseline demersal fishing\")"),
    #       br(),
    #       h5("Example Plot",style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"),
    #       column(
    #         6,
    #         img(src = "yield_1.png", width = '100%'),
    #         h6(
    #           "Yield data plot for planktivoirous fish in the 1970-1999 North Sea model with baseline demersal harvest ratios. Upper panel, annual average biomass of planktivorous fish (mMN.m-2 in the whole modle domain) as a function of harvest ratio. Lower panel: catch divided into landings and discards of planktivorous fish (mMN.m-2.y-1) as a function of harvest ratio."
    #           ,style = "font-family: 'calibri'; font-si16pt; text-align: justify;padding:10px;"
    #         )
    #       )
    #       # ,
    #       # column(
    #       #   6,
    #       #   h3("Run Yield"),
    #       #   actionButton("runYield", "Run Yield Plot")
    #       # ),
    #       # column(
    #       #   6,
    #       #   h5("Yield plot"),
    #       #   plotOutput("yield_plot")
    #       # ),
    #     )
    # )))
  )
)

notGreatherThan100 <- function(input) {
  if(!(is.numeric(input))){0}
       else if(!(is.null(input) || is.na(input))){
         if(input < 0){
           0 
         }else if(input > 100){
           100
         } else{
           return (isolate(input))
         } 
       } else{0}
}

getCoarseValue <- function(input1 = NA , input2 = NA, input3 = NA, coarseArea = NA) {
  if(!(is.numeric(input1) || is.numeric(input2) || is.numeric(input3))){0}
  else if(!(is.null(input1) || is.null(input2) || is.null(input3) || is.na(input1) || is.na(input2) || is.na(input3))){
  if(as.numeric(coarseArea)==0){
     0
  } else {
    totalForThreeOthers <- as.numeric(isolate(input1)) + as.numeric(isolate(input2)) + as.numeric(isolate(input3))
    coarseValue <- (100 - totalForThreeOthers)
    return(coarseValue)
  }}
  else {
    if(is.null(input1) || is.null(input2) || is.null(input3)){
      if(is.null(input1)) { input1 <- 0}
      if(is.null(input2)) { input2 <- 0}
      if(is.null(input3)) { input3 <- 0}
      totalForThreeOthers <- as.numeric(input1) + as.numeric(input2) + as.numeric(input3)
      coarseValue <- (100 - totalForThreeOthers)
      return(coarseValue)
    } else {
      return(0)
    }
  }
}

getMedValue <- function(input1 = NA , input2 = NA, input3 = NA, coarseArea = NA,medArea  = NA, fieldName  = NA, defaultMedValue = NA) {
  if(!(is.numeric(input1) || is.numeric(input2) || is.numeric(input3))){return(0)}
  else if(!(is.null(input1) || is.null(input2) || is.null(input3) || is.na(input1) || is.na(input2) || is.na(input3))){
    if(as.numeric(medArea)==0){
      return(0)
  } else if(as.numeric(coarseArea)==0 && as.numeric(medArea)>0){
    #disable(fieldName)
    totalForTwoOthers <- as.numeric(input1) + as.numeric(input2)
    medValue <- (100 - totalForTwoOthers)
    return(medValue)
  } else {
    return(isolate(input3))
  }} else {
    if(is.null(input3)){
      return(defaultMedValue)
    } else {
      return(input3)
    }
    }
}

getFineValue <- function(input1 = NA , input2 = NA, input3 = NA, coarseArea = NA,medArea  = NA, fineArea = NA, fieldName  = NA, defaultFineValue= NA) {
  if(!(is.numeric(input1) || is.numeric(input2) || is.numeric(input3))){return(0)}
  else if(!(is.null(input1) || is.null(input2) || is.na(input1) || is.na(input2))){
    if(as.numeric(fineArea)==0){
      return(0)
    } else if(as.numeric(medArea)==0 && as.numeric(coarseArea)==0 && as.numeric(fineArea)>0){
      fineValue <- (100 - as.numeric(input1))
      return(fineValue)
    } else {
      return(isolate(input2))
    }} else {
      if(is.null(input2)){
        return(defaultFineValue)
      } else {
      return(input2)
      }
    }
}

getRockValue <- function(input1 = NA , input2 = NA, input3 = NA, coarseArea = NA, medArea  = NA, fineArea = NA, rockArea = NA, fieldName  = NA, defaultRockValue = NA) {
  if(!(is.numeric(input1) || is.numeric(input2) || is.numeric(input3))){return(0)}
  else if(!(is.null(input1) || is.na(input1))){
    if(as.numeric(rockArea)==0){
      return(0)
    } else if(as.numeric(medArea)==0 && as.numeric(coarseArea)==0 && as.numeric(fineArea)==0 && as.numeric(rockArea)>0){
      return(100)
    } else {
      return(input1)
    }} else {
      if(is.null(input1)){
        return(defaultRockValue)
      }else if(as.numeric(medArea)==0 && as.numeric(coarseArea)==0 && as.numeric(fineArea)==0 && as.numeric(rockArea)>0){ # Remove this
        return(defaultRockValue)  
      } else {
        return(input1)
      }
    }
}

server <- function(input, output, session) {
  
  model_reactive <- reactiveVal()
  results_baseline_reactive <- reactiveVal()
  results_scenario_reactive <- reactiveVal()
  scenario_model_reactive <- reactiveVal()
  #model_reactive2 <- reactiveValues(model =  e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models"))
  
  reactivePlot_e2e_compare_runs_bar_aam_plot <- reactive({
    e2e_compare_runs_bar_gg(
      selection = "AAM",
      model1 = model_reactive(),
      use.saved1 = FALSE,
      results_baseline_reactive(),
      model2 = scenario_model_reactive(),
      use.saved2 = FALSE,
      results_scenario_reactive(),
      log.pc = "PC",
      zone = "W",
      bpmin = input$axisMaxPos[1],
      bpmax = input$axisMaxPos[2],
      maintitle = "",
      outputType = "PLOT")
  })
  
  reactivePlot_e2e_compare_runs_bar_aam_data <- reactive({
     e2e_compare_runs_bar_gg(
      selection = "AAM",
      model1 = model_reactive(),
      use.saved1 = FALSE,
      results_baseline_reactive(),
      model2 = scenario_model_reactive(),
      use.saved2 = FALSE,
      results_scenario_reactive(),
      log.pc = "PC",
      zone = "W",
      bpmin = input$axisMaxPos[1],
      bpmax = input$axisMaxPos[2],
      maintitle = "",
      outputType = "DATA")
  })
  
  reactivePlot_e2e_compare_runs_bar_catch_plot <- reactive({
     e2e_compare_runs_bar_gg(
      selection = "CATCH",
      model1 = model_reactive(),
      use.saved1 = FALSE,
      results_baseline_reactive(),
      model2 = scenario_model_reactive(),
      use.saved2 = FALSE,
      results_scenario_reactive(),
      log.pc = "PC",
      zone = "W",
      bpmin = input$axisMaxPosCatch[1],
      bpmax = input$axisMaxPosCatch[2],
      maintitle = "",
      outputType = "PLOT")
  })
  
  reactivePlot_e2e_compare_runs_bar_catch_data <- reactive({
    e2e_compare_runs_bar_gg(
      selection = "CATCH",
      model1 = model_reactive(),
      use.saved1 = FALSE,
      results_baseline_reactive(),
      model2 = scenario_model_reactive(),
      use.saved2 = FALSE,
      results_scenario_reactive(),
      log.pc = "PC",
      zone = "W",
      bpmin = input$axisMaxPosCatch[1],
      bpmax = input$axisMaxPosCatch[2],
      maintitle = "",
      outputType = "DATA")
  })
  
  observeEvent(input$inshorePercentagePel,{
    updateNumericInput(session, "inshorePercentagePel", value = notGreatherThan100(input$inshorePercentagePel))
    offShorePercent <- 100 - input$inshorePercentagePel
    updateNumericInput(session, "offshorePercentagePel", value = offShorePercent)
    disable("offshorePercentagePel")
  })
  
  observeEvent(input$offshorePercentage,{
    updateNumericInput(session, "offshorePercentagePel", value = input$offshorePercentagePel)
    disable("offshorePercentagePel")
  })
  
  output$percentagePelInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentagePelInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentagePelInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    pelInRock <- model$data$fleet.model$gear_habitat_activity$s0[1]
    pelInFine <- model$data$fleet.model$gear_habitat_activity$s1[1]
    pelInMed <- model$data$fleet.model$gear_habitat_activity$s2[1]
    pelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[1]
    totalInPel <- pelInRock + pelInFine + pelInMed + pelInCoarse
    percentagePelInMedDefault <- pelInMed / totalInPel * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    pelInMed <- getMedValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea,medInArea,"percentagePelInMedInput",percentagePelInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentagePelInMedInput", "Inshore Medium %",pelInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentagePelInMedInput", "Inshore Medium %",pelInMed,width = '50%')
    } else {
        # Don't want field if medInArea is zero
      }
  })
  

  output$percentagePelInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    pelInRock <- model$data$fleet.model$gear_habitat_activity$s0[1]
    pelInFine <- model$data$fleet.model$gear_habitat_activity$s1[1]
    pelInMed <- model$data$fleet.model$gear_habitat_activity$s2[1]
    pelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[1]
    totalInPel <- pelInRock + pelInFine + pelInMed + pelInCoarse
    percentagePelInFineDefault <- pelInFine / totalInPel * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    pelInFine <- getFineValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea,medInArea,fineInArea,"percentagePelInFineInput",percentagePelInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentagePelInFineInput", "Inshore fine %",pelInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentagePelInFineInput", "Inshore fine %",pelInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
# 
#   observeEvent(input$percentagePelInRockInput,{
#     model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
#     coarseInArea <- model$data$physical.parameters$x_area_s3
#     medInArea <- model$data$physical.parameters$x_area_s2
#     fineInArea <- model$data$physical.parameters$x_area_s1
#     rockInArea <- model$data$physical.parameters$x_area_s0
#     pelInRock <- getRockValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentagePelInRockInput",percentagePelInRockDefault)
#     updateNumericInput(session,"percentagePelInRockInput", value = notGreatherThan100(pelInRock))
#     if(medInArea==0 && fineInArea==0 && coarseInArea==0){
#       disable("percentagePelInRockInput")
#     }
#   })
# 
#   observeEvent(input$percentagePelInFineInput,{
#     model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
#     coarseInArea <- model$data$physical.parameters$x_area_s3
#     medInArea <- model$data$physical.parameters$x_area_s2
#     fineInArea <- model$data$physical.parameters$x_area_s1
#     pelInFine <- getFineValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea,medInArea,fineInArea,"percentagePelInFineInput")
#     updateNumericInput(session,"percentagePelInFineInput", value = notGreatherThan100(pelInFine))
#   })
#   
#   observeEvent(input$percentagePelInMedInput,{
#     model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
#     coarseInArea <- model$data$physical.parameters$x_area_s3
#     medInArea <- model$data$physical.parameters$x_area_s2
#     pelInMed <- getMedValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea,medInArea,"percentagePelInMedInput")
#     updateNumericInput(session,"percentagePelInMedInput",value = notGreatherThan100(pelInMed))
#   })
#   
#   
  observeEvent(input$percentagePelInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    pelInCoarse <- getCoarseValue(input$percentagePelInRockInput,input$percentagePelInFineInput,input$percentagePelInMedInput,coarseInArea)
    if ( pelInCoarse < 0 || pelInCoarse > 100)  {
      js$backgroundCol("percentagePelInCoarseInput","red")
    } else {
      js$backgroundCol("percentagePelInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentagePelInCoarseInput", value = notGreatherThan100(pelInCoarse))
    disable("percentagePelInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentagePelOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentagePelOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  # 
  # output$percentagePelOffMed <- renderUI({ 
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   pelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[1]
  #   pelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[1]
  #   pelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[1]
  #   pelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[1]
  #   totalOffPel <- pelOffRock + pelOffFine + pelOffMed + pelOffCoarse
  #   percentagePelOffMedDefault <- pelOffMed / totalOffPel * 100
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   pelOffMed <- getMedValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea,medOffArea,"percentagePelOffMedInput",percentagePelOffMedDefault)
  #   if(medOffArea >0 && coarseOffArea==0 ){
  #     disabled(numericInput("percentagePelOffMedInput", "Offshore Medium %",pelOffMed,width = '50%'))
  #   } else if(medOffArea >0) {
  #     numericInput("percentagePelOffMedInput", "Offshore Medium %",pelOffMed,width = '50%')
  #   } else {
  #     # Don't want field if medOffArea is zero
  #   }
  # })
  # 
  # output$percentagePelOffFine <- renderUI({
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   pelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[1]
  #   pelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[1]
  #   pelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[1]
  #   pelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[1]
  #   totalOffPel <- pelOffRock + pelOffFine + pelOffMed + pelOffCoarse
  #   percentagePelOffFineDefault <- pelOffFine / totalOffPel * 100
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <-  model$data$physical.parameters$x_area_d1
  #   pelOffFine <- getFineValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentagePelOffFineInput",percentagePelOffFineDefault)
  #   if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
  #     disabled(numericOffput("percentagePelOffFineInput", "Offshore fine %",pelOffFine,width = '50%'))
  #   } else if (fineOffArea >0) {
  #     numericInput("percentagePelOffFineInput", "Offshore fine %",pelOffFine,width = '50%')
  #   } else {
  #     # Don't want field if fineOffArea is zero
  #   }
  # })
  
  
  # observeEvent(input$percentagePelOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   pelOffRock <- getRockValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentagePelOffRockInput")
  #   updateNumericInput(session,"percentagePelOffRockInput", value = notGreatherThan100(pelOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentagePelOffRockInput")
  #   }
  # })
  
  # observeEvent(input$percentagePelOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   pelOffFine <- getFineValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentagePelOffFineInput")
  #   updateNumericInput(session,"percentagePelOffFineInput", value = notGreatherThan100(pelOffFine))
  # })
  # 
  # observeEvent(input$percentagePelOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   pelOffMed <- getMedValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea,medOffArea,"percentagePelOffMedInput")
  #   updateNumericInput(session,"percentagePelOffMedInput",value = notGreatherThan100(pelOffMed))
  # })
  # 
  observeEvent(input$percentagePelOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    pelOffCoarse <- getCoarseValue(input$percentagePelOffRockInput,input$percentagePelOffFineInput,input$percentagePelOffMedInput,coarseOffArea)
    if ( pelOffCoarse < 0 || pelOffCoarse > 100)  {
      js$backgroundCol("percentagePelOffCoarseInput","red")
    } else {
      js$backgroundCol("percentagePelOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentagePelOffCoarseInput", value = notGreatherThan100(pelOffCoarse))
    disable("percentagePelOffCoarseInput")
  })
  
  
  observeEvent(input$pelGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    #model <- model_reactive()
    pelInRock <- model$data$fleet.model$gear_habitat_activity$s0[1] 
    pelInFine <- model$data$fleet.model$gear_habitat_activity$s1[1] 
    pelInMed <- model$data$fleet.model$gear_habitat_activity$s2[1] 
    pelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[1]
    pelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[1] 
    pelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[1] 
    pelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[1] 
    pelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[1] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInPel <- pelInRock + pelInFine + pelInMed + pelInCoarse
    totalOffPel <- pelOffRock + pelOffFine + pelOffMed + pelOffCoarse
    totalOverallPel <- totalInPel + totalOffPel
    percentageInPelDefault <- totalInPel/totalOverallPel * 100
    percentageOutPelDefault <- 100 - percentageInPelDefault
    # Now getting percentages Inshore
    percentagePelInRockDefault <- pelInRock/totalInPel * 100
    percentagePelInFineDefault <- pelInFine/totalInPel * 100
    percentagePelInMedDefault <- pelInMed/totalInPel * 100
    percentagePelInCoarseDefault <- pelInCoarse/totalInPel * 100
    # Now getting percentages Offshore
    percentagePelOffRockDefault <- pelOffRock/totalOffPel * 100
    percentagePelOffFineDefault <- pelOffFine/totalOffPel * 100
    percentagePelOffMedDefault <- pelOffMed/totalOffPel * 100
    percentagePelOffCoarseDefault <- pelOffCoarse/totalOffPel * 100
    updateNumericInput(session, "inshorePercentagePel", value = percentageInPelDefault)
    updateNumericInput(session, "offshorePercentagePel", value = percentageOutPelDefault)
    disable("offshorePercentagePel")
    updateNumericInput(session, "percentagePelInRockInput", value = percentagePelInRockDefault)
    updateNumericInput(session, "percentagePelInFineInput", value = percentagePelInFineDefault)
    updateNumericInput(session, "percentagePelInMedInput", value = percentagePelInMedDefault)
    updateNumericInput(session, "percentagePelInCoarseInput", value = percentagePelInCoarseDefault)
    disable("percentagePelInCoarseInput")
    updateNumericInput(session, "percentagePelOffRockInput", value = percentagePelOffRockDefault)
    updateNumericInput(session, "percentagePelOffFineInput", value = percentagePelOffFineDefault)
    updateNumericInput(session, "percentagePelOffMedInput", value = percentagePelOffMedDefault)
    updateNumericInput(session, "percentagePelOffCoarseInput", value = percentagePelOffCoarseDefault)
    disable("percentagePelOffCoarseInput")
  })
    
  observeEvent(input$inshorePercentageSandeel,{
    updateNumericInput(session, "inshorePercentageSandeel", value = notGreatherThan100(input$inshorePercentageSandeel))
    offShorePercent <- 100 - input$inshorePercentageSandeel
    updateNumericInput(session, "offshorePercentageSandeel", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageSandeel,{
    updateNumericInput(session, "offshorePercentageSandeel", value = input$offshorePercentageSandeel)
    disable("offshorePercentageSandeel")
  })
  
  output$percentageSandeelInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageSandeelInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageSandeelInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    SandeelInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
    SandeelInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
    SandeelInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
    SandeelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    totalInSandeel <- SandeelInRock + SandeelInFine + SandeelInMed + SandeelInCoarse
    percentageSandeelInMedDefault <- SandeelInMed / totalInSandeel * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    SandeelInMed <- getMedValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea,medInArea,"percentageSandeelInMedInput",percentageSandeelInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageSandeelInMedInput", "Inshore Medium %",SandeelInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageSandeelInMedInput", "Inshore Medium %",SandeelInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageSandeelInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    SandeelInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
    SandeelInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
    SandeelInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
    SandeelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    totalInSandeel <- SandeelInRock + SandeelInFine + SandeelInMed + SandeelInCoarse
    percentageSandeelInFineDefault <- SandeelInFine / totalInSandeel * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    SandeelInFine <- getFineValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea,medInArea,fineInArea,"percentageSandeelInFineInput",percentageSandeelInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageSandeelInFineInput", "Inshore fine %",SandeelInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageSandeelInFineInput", "Inshore fine %",SandeelInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageSandeelInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   SandeelInRock <- getRockValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageSandeelInRockInput",percentageSandeelInRockDefault)
  #   updateNumericInput(session,"percentageSandeelInRockInput", value = notGreatherThan100(SandeelInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageSandeelInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageSandeelInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   SandeelInFine <- getFineValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea,medInArea,fineInArea,"percentageSandeelInFineInput")
  #   updateNumericInput(session,"percentageSandeelInFineInput", value = notGreatherThan100(SandeelInFine))
  # })
  # 
  # observeEvent(input$percentageSandeelInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   SandeelInMed <- getMedValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea,medInArea,"percentageSandeelInMedInput")
  #   updateNumericInput(session,"percentageSandeelInMedInput",value = notGreatherThan100(SandeelInMed))
  # })
  # 
  
  observeEvent(input$percentageSandeelInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    SandeelInCoarse <- getCoarseValue(input$percentageSandeelInRockInput,input$percentageSandeelInFineInput,input$percentageSandeelInMedInput,coarseInArea)
    if ( SandeelInCoarse < 0 || SandeelInCoarse > 100)  {
      js$backgroundCol("percentageSandeelInCoarseInput","red")
    } else {
      js$backgroundCol("percentageSandeelInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageSandeelInCoarseInput", value = notGreatherThan100(SandeelInCoarse))
    disable("percentageSandeelInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageSandeelOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageSandeelOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageSandeelOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    SandeelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
    SandeelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
    SandeelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2]
    SandeelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
    totalOffSandeel <- SandeelOffRock + SandeelOffFine + SandeelOffMed + SandeelOffCoarse
    percentageSandeelOffMedDefault <- SandeelOffMed / totalOffSandeel * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    SandeelOffMed <- getMedValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea,medOffArea,"percentageSandeelOffMedInput",percentageSandeelOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageSandeelOffMedInput", "Offshore Medium %",SandeelOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageSandeelOffMedInput", "Offshore Medium %",SandeelOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageSandeelOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    SandeelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
    SandeelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
    SandeelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2]
    SandeelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
    totalOffSandeel <- SandeelOffRock + SandeelOffFine + SandeelOffMed + SandeelOffCoarse
    percentageSandeelOffFineDefault <- SandeelOffFine / totalOffSandeel * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    SandeelOffFine <- getFineValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageSandeelOffFineInput",percentageSandeelOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageSandeelOffFineInput", "Offshore fine %",SandeelOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageSandeelOffFineInput", "Offshore fine %",SandeelOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageSandeelOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   SandeelOffRock <- getRockValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageSandeelOffRockInput")
  #   updateNumericInput(session,"percentageSandeelOffRockInput", value = notGreatherThan100(SandeelOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageSandeelOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageSandeelOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   SandeelOffFine <- getFineValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageSandeelOffFineInput")
  #   updateNumericInput(session,"percentageSandeelOffFineInput", value = notGreatherThan100(SandeelOffFine))
  # })
  # 
  # observeEvent(input$percentageSandeelOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   SandeelOffMed <- getMedValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea,medOffArea,"percentageSandeelOffMedInput")
  #   updateNumericInput(session,"percentageSandeelOffMedInput",value = notGreatherThan100(SandeelOffMed))
  # })
  # 
  observeEvent(input$percentageSandeelOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    SandeelOffCoarse <- getCoarseValue(input$percentageSandeelOffRockInput,input$percentageSandeelOffFineInput,input$percentageSandeelOffMedInput,coarseOffArea)
    if ( SandeelOffCoarse < 0 || SandeelOffCoarse > 100)  {
      js$backgroundCol("percentageSandeelOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageSandeelOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageSandeelOffCoarseInput", value = notGreatherThan100(SandeelOffCoarse))
    disable("percentageSandeelOffCoarseInput")
  })
  
  
  observeEvent(input$sandeelGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    #model <- model_reactive()
    SandeelInRock <- model$data$fleet.model$gear_habitat_activity$s0[2] 
    SandeelInFine <- model$data$fleet.model$gear_habitat_activity$s1[2] 
    SandeelInMed <- model$data$fleet.model$gear_habitat_activity$s2[2] 
    SandeelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    SandeelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2] 
    SandeelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2] 
    SandeelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2] 
    SandeelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInSandeel <- SandeelInRock + SandeelInFine + SandeelInMed + SandeelInCoarse
    totalOffSandeel <- SandeelOffRock + SandeelOffFine + SandeelOffMed + SandeelOffCoarse
    totalOverallSandeel <- totalInSandeel + totalOffSandeel
    percentageInSandeelDefault <- totalInSandeel/totalOverallSandeel * 100
    percentageOutSandeelDefault <- 100 - percentageInSandeelDefault
    # Now getting percentages Inshore
    percentageSandeelInRockDefault <- SandeelInRock/totalInSandeel * 100
    percentageSandeelInFineDefault <- SandeelInFine/totalInSandeel * 100
    percentageSandeelInMedDefault <- SandeelInMed/totalInSandeel * 100
    percentageSandeelInCoarseDefault <- SandeelInCoarse/totalInSandeel * 100
    # Now getting percentages Offshore
    percentageSandeelOffRockDefault <- SandeelOffRock/totalOffSandeel * 100
    percentageSandeelOffFineDefault <- SandeelOffFine/totalOffSandeel * 100
    percentageSandeelOffMedDefault <- SandeelOffMed/totalOffSandeel * 100
    percentageSandeelOffCoarseDefault <- SandeelOffCoarse/totalOffSandeel * 100
    updateNumericInput(session, "inshorePercentageSandeel", value = percentageInSandeelDefault)
    updateNumericInput(session, "offshorePercentageSandeel", value = percentageOutSandeelDefault)
    disable("offshorePercentageSandeel")
    updateNumericInput(session, "percentageSandeelInRockInput", value = percentageSandeelInRockDefault)
    updateNumericInput(session, "percentageSandeelInFineInput", value = percentageSandeelInFineDefault)
    updateNumericInput(session, "percentageSandeelInMedInput", value = percentageSandeelInMedDefault)
    updateNumericInput(session, "percentageSandeelInCoarseInput", value = percentageSandeelInCoarseDefault)
    disable("percentageSandeelInCoarseInput")
    updateNumericInput(session, "percentageSandeelOffRockInput", value = percentageSandeelOffRockDefault)
    updateNumericInput(session, "percentageSandeelOffFineInput", value = percentageSandeelOffFineDefault)
    updateNumericInput(session, "percentageSandeelOffMedInput", value = percentageSandeelOffMedDefault)
    updateNumericInput(session, "percentageSandeelOffCoarseInput", value = percentageSandeelOffCoarseDefault)
    disable("percentageSandeelOffCoarseInput")
  })
    
  observeEvent(input$inshorePercentageOtter,{
    updateNumericInput(session, "inshorePercentageOtter", value = notGreatherThan100(input$inshorePercentageOtter))
    offShorePercent <- 100 - input$inshorePercentageOtter
    updateNumericInput(session, "offshorePercentageOtter", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageOtter,{
    updateNumericInput(session, "offshorePercentageOtter", value = input$offshorePercentageOtter)
    disable("offshorePercentageOtter")
  })
  
  output$percentageOtterInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageOtterInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageOtterInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    OtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
    OtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
    OtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
    OtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    totalInOtter <- OtterInRock + OtterInFine + OtterInMed + OtterInCoarse
    percentageOtterInMedDefault <- OtterInMed / totalInOtter * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    OtterInMed <- getMedValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea,medInArea,"percentageOtterInMedInput",percentageOtterInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageOtterInMedInput", "Inshore Medium %",OtterInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageOtterInMedInput", "Inshore Medium %",OtterInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageOtterInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    OtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
    OtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
    OtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
    OtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    totalInOtter <- OtterInRock + OtterInFine + OtterInMed + OtterInCoarse
    percentageOtterInFineDefault <- OtterInFine / totalInOtter * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    OtterInFine <- getFineValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea,medInArea,fineInArea,"percentageOtterInFineInput",percentageOtterInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageOtterInFineInput", "Inshore fine %",OtterInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageOtterInFineInput", "Inshore fine %",OtterInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageOtterInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   OtterInRock <- getRockValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageOtterInRockInput",percentageOtterInRockDefault)
  #   updateNumericInput(session,"percentageOtterInRockInput", value = notGreatherThan100(OtterInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageOtterInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageOtterInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   OtterInFine <- getFineValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea,medInArea,fineInArea,"percentageOtterInFineInput")
  #   updateNumericInput(session,"percentageOtterInFineInput", value = notGreatherThan100(OtterInFine))
  # })
  # 
  # observeEvent(input$percentageOtterInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   OtterInMed <- getMedValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea,medInArea,"percentageOtterInMedInput")
  #   updateNumericInput(session,"percentageOtterInMedInput",value = notGreatherThan100(OtterInMed))
  # })
  
  
  observeEvent(input$percentageOtterInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    OtterInCoarse <- getCoarseValue(input$percentageOtterInRockInput,input$percentageOtterInFineInput,input$percentageOtterInMedInput,coarseInArea)
    if ( OtterInCoarse < 0 || OtterInCoarse > 100)  {
      js$backgroundCol("percentageOtterInCoarseInput","red")
    } else {
      js$backgroundCol("percentageOtterInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageOtterInCoarseInput", value = notGreatherThan100(OtterInCoarse))
    disable("percentageOtterInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageOtterOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageOtterOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageOtterOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    OtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
    OtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
    OtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2]
    OtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
    totalOffOtter <- OtterOffRock + OtterOffFine + OtterOffMed + OtterOffCoarse
    percentageOtterOffMedDefault <- OtterOffMed / totalOffOtter * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    OtterOffMed <- getMedValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea,medOffArea,"percentageOtterOffMedInput",percentageOtterOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageOtterOffMedInput", "Offshore Medium %",OtterOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageOtterOffMedInput", "Offshore Medium %",OtterOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageOtterOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    OtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
    OtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
    OtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2]
    OtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
    totalOffOtter <- OtterOffRock + OtterOffFine + OtterOffMed + OtterOffCoarse
    percentageOtterOffFineDefault <- OtterOffFine / totalOffOtter * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    OtterOffFine <- getFineValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageOtterOffFineInput",percentageOtterOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageOtterOffFineInput", "Offshore fine %",OtterOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageOtterOffFineInput", "Offshore fine %",OtterOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageOtterOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   OtterOffRock <- getRockValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageOtterOffRockInput")
  #   updateNumericInput(session,"percentageOtterOffRockInput", value = notGreatherThan100(OtterOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageOtterOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageOtterOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   OtterOffFine <- getFineValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageOtterOffFineInput")
  #   updateNumericInput(session,"percentageOtterOffFineInput", value = notGreatherThan100(OtterOffFine))
  # })
  # 
  # observeEvent(input$percentageOtterOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   OtterOffMed <- getMedValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea,medOffArea,"percentageOtterOffMedInput")
  #   updateNumericInput(session,"percentageOtterOffMedInput",value = notGreatherThan100(OtterOffMed))
  # })
  
  observeEvent(input$percentageOtterOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    OtterOffCoarse <- getCoarseValue(input$percentageOtterOffRockInput,input$percentageOtterOffFineInput,input$percentageOtterOffMedInput,coarseOffArea)
    if ( OtterOffCoarse < 0 || OtterOffCoarse > 100)  {
      js$backgroundCol("percentageOtterOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageOtterOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageOtterOffCoarseInput", value = notGreatherThan100(OtterOffCoarse))
    disable("percentageOtterOffCoarseInput")
  })
  
  
  observeEvent(input$otterGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    OtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[2] 
    OtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[2] 
    OtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[2] 
    OtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
    OtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2] 
    OtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2] 
    OtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2] 
    OtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInOtter <- OtterInRock + OtterInFine + OtterInMed + OtterInCoarse
    totalOffOtter <- OtterOffRock + OtterOffFine + OtterOffMed + OtterOffCoarse
    totalOverallOtter <- totalInOtter + totalOffOtter
    percentageInOtterDefault <- totalInOtter/totalOverallOtter * 100
    percentageOutOtterDefault <- 100 - percentageInOtterDefault
    # Now getting percentages Inshore
    percentageOtterInRockDefault <- OtterInRock/totalInOtter * 100
    percentageOtterInFineDefault <- OtterInFine/totalInOtter * 100
    percentageOtterInMedDefault <- OtterInMed/totalInOtter * 100
    percentageOtterInCoarseDefault <- OtterInCoarse/totalInOtter * 100
    # Now getting percentages Offshore
    percentageOtterOffRockDefault <- OtterOffRock/totalOffOtter * 100
    percentageOtterOffFineDefault <- OtterOffFine/totalOffOtter * 100
    percentageOtterOffMedDefault <- OtterOffMed/totalOffOtter * 100
    percentageOtterOffCoarseDefault <- OtterOffCoarse/totalOffOtter * 100
    updateNumericInput(session, "inshorePercentageOtter", value = percentageInOtterDefault)
    updateNumericInput(session, "offshorePercentageOtter", value = percentageOutOtterDefault)
    updateNumericInput(session, "percentageOtterInRockInput", value = percentageOtterInRockDefault)
    updateNumericInput(session, "percentageOtterInFineInput", value = percentageOtterInFineDefault)
    updateNumericInput(session, "percentageOtterInMedInput", value = percentageOtterInMedDefault)
    updateNumericInput(session, "percentageOtterInCoarseInput", value = percentageOtterInCoarseDefault)
    updateNumericInput(session, "percentageOtterOffRockInput", value = percentageOtterOffRockDefault)
    updateNumericInput(session, "percentageOtterOffFineInput", value = percentageOtterOffFineDefault)
    updateNumericInput(session, "percentageOtterOffMedInput", value = percentageOtterOffMedDefault)
    updateNumericInput(session, "percentageOtterOffCoarseInput", value = percentageOtterOffCoarseDefault)
  })
  
    
  observeEvent(input$inshorePercentageLonMack,{
    updateNumericInput(session, "inshorePercentageLonMack", value = notGreatherThan100(input$inshorePercentageLonMack))
    offShorePercent <- 100 - input$inshorePercentageLonMack
    updateNumericInput(session, "offshorePercentageLonMack", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageLonMack,{
    updateNumericInput(session, "offshorePercentageLonMack", value = input$offshorePercentageLonMack)
    disable("offshorePercentageLonMack")
  })
  
  output$percentageLonMackInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageLonMackInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageLonMackInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    LonMackInRock <- model$data$fleet.model$gear_habitat_activity$s0[3]
    LonMackInFine <- model$data$fleet.model$gear_habitat_activity$s1[3]
    LonMackInMed <- model$data$fleet.model$gear_habitat_activity$s2[3]
    LonMackInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[3]
    totalInLonMack <- LonMackInRock + LonMackInFine + LonMackInMed + LonMackInCoarse
    percentageLonMackInMedDefault <- LonMackInMed / totalInLonMack * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    LonMackInMed <- getMedValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea,medInArea,"percentageLonMackInMedInput",percentageLonMackInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageLonMackInMedInput", "Inshore Medium %",LonMackInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageLonMackInMedInput", "Inshore Medium %",LonMackInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageLonMackInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    LonMackInRock <- model$data$fleet.model$gear_habitat_activity$s0[3]
    LonMackInFine <- model$data$fleet.model$gear_habitat_activity$s1[3]
    LonMackInMed <- model$data$fleet.model$gear_habitat_activity$s2[3]
    LonMackInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[3]
    totalInLonMack <- LonMackInRock + LonMackInFine + LonMackInMed + LonMackInCoarse
    percentageLonMackInFineDefault <- LonMackInFine / totalInLonMack * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    LonMackInFine <- getFineValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea,medInArea,fineInArea,"percentageLonMackInFineInput",percentageLonMackInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageLonMackInFineInput", "Inshore fine %",LonMackInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageLonMackInFineInput", "Inshore fine %",LonMackInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageLonMackInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   LonMackInRock <- getRockValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageLonMackInRockInput",percentageLonMackInRockDefault)
  #   updateNumericInput(session,"percentageLonMackInRockInput", value = notGreatherThan100(LonMackInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageLonMackInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageLonMackInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   LonMackInFine <- getFineValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea,medInArea,fineInArea,"percentageLonMackInFineInput")
  #   updateNumericInput(session,"percentageLonMackInFineInput", value = notGreatherThan100(LonMackInFine))
  # })
  # 
  # observeEvent(input$percentageLonMackInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   LonMackInMed <- getMedValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea,medInArea,"percentageLonMackInMedInput")
  #   updateNumericInput(session,"percentageLonMackInMedInput",value = notGreatherThan100(LonMackInMed))
  # })
  
  
  observeEvent(input$percentageLonMackInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    LonMackInCoarse <- getCoarseValue(input$percentageLonMackInRockInput,input$percentageLonMackInFineInput,input$percentageLonMackInMedInput,coarseInArea)
    if ( LonMackInCoarse < 0 || LonMackInCoarse > 100)  {
      js$backgroundCol("percentageLonMackInCoarseInput","red")
    } else {
      js$backgroundCol("percentageLonMackInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageLonMackInCoarseInput", value = notGreatherThan100(LonMackInCoarse))
    disable("percentageLonMackInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageLonMackOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageLonMackOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageLonMackOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    LonMackOffRock <- model$data$fleet.model$gear_habitat_activity$d0[3]
    LonMackOffFine <- model$data$fleet.model$gear_habitat_activity$d1[3]
    LonMackOffMed <- model$data$fleet.model$gear_habitat_activity$d2[3]
    LonMackOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[3]
    totalOffLonMack <- LonMackOffRock + LonMackOffFine + LonMackOffMed + LonMackOffCoarse
    percentageLonMackOffMedDefault <- LonMackOffMed / totalOffLonMack * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    LonMackOffMed <- getMedValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea,medOffArea,"percentageLonMackOffMedInput",percentageLonMackOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageLonMackOffMedInput", "Offshore Medium %",LonMackOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageLonMackOffMedInput", "Offshore Medium %",LonMackOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageLonMackOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    LonMackOffRock <- model$data$fleet.model$gear_habitat_activity$d0[3]
    LonMackOffFine <- model$data$fleet.model$gear_habitat_activity$d1[3]
    LonMackOffMed <- model$data$fleet.model$gear_habitat_activity$d2[3]
    LonMackOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[3]
    totalOffLonMack <- LonMackOffRock + LonMackOffFine + LonMackOffMed + LonMackOffCoarse
    percentageLonMackOffFineDefault <- LonMackOffFine / totalOffLonMack * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    LonMackOffFine <- getFineValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageLonMackOffFineInput",percentageLonMackOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageLonMackOffFineInput", "Offshore fine %",LonMackOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageLonMackOffFineInput", "Offshore fine %",LonMackOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageLonMackOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   LonMackOffRock <- getRockValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageLonMackOffRockInput")
  #   updateNumericInput(session,"percentageLonMackOffRockInput", value = notGreatherThan100(LonMackOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageLonMackOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageLonMackOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   LonMackOffFine <- getFineValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageLonMackOffFineInput")
  #   updateNumericInput(session,"percentageLonMackOffFineInput", value = notGreatherThan100(LonMackOffFine))
  # })
  # 
  # observeEvent(input$percentageLonMackOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   LonMackOffMed <- getMedValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea,medOffArea,"percentageLonMackOffMedInput")
  #   updateNumericInput(session,"percentageLonMackOffMedInput",value = notGreatherThan100(LonMackOffMed))
  # })
  
  observeEvent(input$percentageLonMackOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    LonMackOffCoarse <- getCoarseValue(input$percentageLonMackOffRockInput,input$percentageLonMackOffFineInput,input$percentageLonMackOffMedInput,coarseOffArea)
    if ( LonMackOffCoarse < 0 || LonMackOffCoarse > 100)  {
      js$backgroundCol("percentageLonMackOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageLonMackOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageLonMackOffCoarseInput", value = notGreatherThan100(LonMackOffCoarse))
    disable("percentageLonMackOffCoarseInput")
  })
  
  
  observeEvent(input$lonMackGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    LonMackInRock <- model$data$fleet.model$gear_habitat_activity$s0[3] 
    LonMackInFine <- model$data$fleet.model$gear_habitat_activity$s1[3] 
    LonMackInMed <- model$data$fleet.model$gear_habitat_activity$s2[3] 
    LonMackInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[3]
    LonMackOffRock <- model$data$fleet.model$gear_habitat_activity$d0[3] 
    LonMackOffFine <- model$data$fleet.model$gear_habitat_activity$d1[3] 
    LonMackOffMed <- model$data$fleet.model$gear_habitat_activity$d2[3] 
    LonMackOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[3] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInLonMack <- LonMackInRock + LonMackInFine + LonMackInMed + LonMackInCoarse
    totalOffLonMack <- LonMackOffRock + LonMackOffFine + LonMackOffMed + LonMackOffCoarse
    totalOverallLonMack <- totalInLonMack + totalOffLonMack
    percentageInLonMackDefault <- totalInLonMack/totalOverallLonMack * 100
    percentageOutLonMackDefault <- 100 - percentageInLonMackDefault
    # Now getting percentages Inshore
    percentageLonMackInRockDefault <- LonMackInRock/totalInLonMack * 100
    percentageLonMackInFineDefault <- LonMackInFine/totalInLonMack * 100
    percentageLonMackInMedDefault <- LonMackInMed/totalInLonMack * 100
    percentageLonMackInCoarseDefault <- LonMackInCoarse/totalInLonMack * 100
    # Now getting percentages Offshore
    percentageLonMackOffRockDefault <- LonMackOffRock/totalOffLonMack * 100
    percentageLonMackOffFineDefault <- LonMackOffFine/totalOffLonMack * 100
    percentageLonMackOffMedDefault <- LonMackOffMed/totalOffLonMack * 100
    percentageLonMackOffCoarseDefault <- LonMackOffCoarse/totalOffLonMack * 100
    updateNumericInput(session, "inshorePercentageLonMack", value = percentageInLonMackDefault)
    updateNumericInput(session, "offshorePercentageLonMack", value = percentageOutLonMackDefault)
    updateNumericInput(session, "percentageLonMackInRockInput", value = percentageLonMackInRockDefault)
    updateNumericInput(session, "percentageLonMackInFineInput", value = percentageLonMackInFineDefault)
    updateNumericInput(session, "percentageLonMackInMedInput", value = percentageLonMackInMedDefault)
    updateNumericInput(session, "percentageLonMackInCoarseInput", value = percentageLonMackInCoarseDefault)
    updateNumericInput(session, "percentageLonMackOffRockInput", value = percentageLonMackOffRockDefault)
    updateNumericInput(session, "percentageLonMackOffFineInput", value = percentageLonMackOffFineDefault)
    updateNumericInput(session, "percentageLonMackOffMedInput", value = percentageLonMackOffMedDefault)
    updateNumericInput(session, "percentageLonMackOffCoarseInput", value = percentageLonMackOffCoarseDefault)
  })
  
    
  
  observeEvent(input$inshorePercentageBeamTrawl,{
    updateNumericInput(session, "inshorePercentageBeamTrawl", value = notGreatherThan100(input$inshorePercentageBeamTrawl))
    offShorePercent <- 100 - input$inshorePercentageBeamTrawl
    updateNumericInput(session, "offshorePercentageBeamTrawl", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageBeamTrawl,{
    updateNumericInput(session, "offshorePercentageBeamTrawl", value = input$offshorePercentageBeamTrawl)
    disable("offshorePercentageBeamTrawl")
  })
  
  output$percentageBeamTrawlInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageBeamTrawlInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageBeamTrawlInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamTrawlInRock <- model$data$fleet.model$gear_habitat_activity$s0[4]
    BeamTrawlInFine <- model$data$fleet.model$gear_habitat_activity$s1[4]
    BeamTrawlInMed <- model$data$fleet.model$gear_habitat_activity$s2[4]
    BeamTrawlInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[4]
    totalInBeamTrawl <- BeamTrawlInRock + BeamTrawlInFine + BeamTrawlInMed + BeamTrawlInCoarse
    percentageBeamTrawlInMedDefault <- BeamTrawlInMed / totalInBeamTrawl * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    BeamTrawlInMed <- getMedValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea,medInArea,"percentageBeamTrawlInMedInput",percentageBeamTrawlInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageBeamTrawlInMedInput", "Inshore Medium %",BeamTrawlInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageBeamTrawlInMedInput", "Inshore Medium %",BeamTrawlInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageBeamTrawlInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamTrawlInRock <- model$data$fleet.model$gear_habitat_activity$s0[4]
    BeamTrawlInFine <- model$data$fleet.model$gear_habitat_activity$s1[4]
    BeamTrawlInMed <- model$data$fleet.model$gear_habitat_activity$s2[4]
    BeamTrawlInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[4]
    totalInBeamTrawl <- BeamTrawlInRock + BeamTrawlInFine + BeamTrawlInMed + BeamTrawlInCoarse
    percentageBeamTrawlInFineDefault <- BeamTrawlInFine / totalInBeamTrawl * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    BeamTrawlInFine <- getFineValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea,medInArea,fineInArea,"percentageBeamTrawlInFineInput",percentageBeamTrawlInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageBeamTrawlInFineInput", "Inshore fine %",BeamTrawlInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageBeamTrawlInFineInput", "Inshore fine %",BeamTrawlInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageBeamTrawlInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   BeamTrawlInRock <- getRockValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageBeamTrawlInRockInput",percentageBeamTrawlInRockDefault)
  #   updateNumericInput(session,"percentageBeamTrawlInRockInput", value = notGreatherThan100(BeamTrawlInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageBeamTrawlInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageBeamTrawlInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   BeamTrawlInFine <- getFineValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea,medInArea,fineInArea,"percentageBeamTrawlInFineInput")
  #   updateNumericInput(session,"percentageBeamTrawlInFineInput", value = notGreatherThan100(BeamTrawlInFine))
  # })
  # 
  # observeEvent(input$percentageBeamTrawlInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   BeamTrawlInMed <- getMedValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea,medInArea,"percentageBeamTrawlInMedInput")
  #   updateNumericInput(session,"percentageBeamTrawlInMedInput",value = notGreatherThan100(BeamTrawlInMed))
  # })
  
  
  observeEvent(input$percentageBeamTrawlInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    BeamTrawlInCoarse <- getCoarseValue(input$percentageBeamTrawlInRockInput,input$percentageBeamTrawlInFineInput,input$percentageBeamTrawlInMedInput,coarseInArea)
    if ( BeamTrawlInCoarse < 0 || BeamTrawlInCoarse > 100)  {
      js$backgroundCol("percentageBeamTrawlInCoarseInput","red")
    } else {
      js$backgroundCol("percentageBeamTrawlInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageBeamTrawlInCoarseInput", value = notGreatherThan100(BeamTrawlInCoarse))
    disable("percentageBeamTrawlInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageBeamTrawlOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageBeamTrawlOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageBeamTrawlOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamTrawlOffRock <- model$data$fleet.model$gear_habitat_activity$d0[4]
    BeamTrawlOffFine <- model$data$fleet.model$gear_habitat_activity$d1[4]
    BeamTrawlOffMed <- model$data$fleet.model$gear_habitat_activity$d2[4]
    BeamTrawlOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[4]
    totalOffBeamTrawl <- BeamTrawlOffRock + BeamTrawlOffFine + BeamTrawlOffMed + BeamTrawlOffCoarse
    percentageBeamTrawlOffMedDefault <- BeamTrawlOffMed / totalOffBeamTrawl * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    BeamTrawlOffMed <- getMedValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea,medOffArea,"percentageBeamTrawlOffMedInput",percentageBeamTrawlOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageBeamTrawlOffMedInput", "Offshore Medium %",BeamTrawlOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageBeamTrawlOffMedInput", "Offshore Medium %",BeamTrawlOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageBeamTrawlOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamTrawlOffRock <- model$data$fleet.model$gear_habitat_activity$d0[4]
    BeamTrawlOffFine <- model$data$fleet.model$gear_habitat_activity$d1[4]
    BeamTrawlOffMed <- model$data$fleet.model$gear_habitat_activity$d2[4]
    BeamTrawlOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[4]
    totalOffBeamTrawl <- BeamTrawlOffRock + BeamTrawlOffFine + BeamTrawlOffMed + BeamTrawlOffCoarse
    percentageBeamTrawlOffFineDefault <- BeamTrawlOffFine / totalOffBeamTrawl * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    BeamTrawlOffFine <- getFineValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageBeamTrawlOffFineInput",percentageBeamTrawlOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageBeamTrawlOffFineInput", "Offshore fine %",BeamTrawlOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageBeamTrawlOffFineInput", "Offshore fine %",BeamTrawlOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageBeamTrawlOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   BeamTrawlOffRock <- getRockValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageBeamTrawlOffRockInput")
  #   updateNumericInput(session,"percentageBeamTrawlOffRockInput", value = notGreatherThan100(BeamTrawlOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageBeamTrawlOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageBeamTrawlOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   BeamTrawlOffFine <- getFineValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageBeamTrawlOffFineInput")
  #   updateNumericInput(session,"percentageBeamTrawlOffFineInput", value = notGreatherThan100(BeamTrawlOffFine))
  # })
  # 
  # observeEvent(input$percentageBeamTrawlOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   BeamTrawlOffMed <- getMedValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea,medOffArea,"percentageBeamTrawlOffMedInput")
  #   updateNumericInput(session,"percentageBeamTrawlOffMedInput",value = notGreatherThan100(BeamTrawlOffMed))
  # })
  
  observeEvent(input$percentageBeamTrawlOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    BeamTrawlOffCoarse <- getCoarseValue(input$percentageBeamTrawlOffRockInput,input$percentageBeamTrawlOffFineInput,input$percentageBeamTrawlOffMedInput,coarseOffArea)
    if ( BeamTrawlOffCoarse < 0 || BeamTrawlOffCoarse > 100)  {
      js$backgroundCol("percentageBeamTrawlOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageBeamTrawlOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageBeamTrawlOffCoarseInput", value = notGreatherThan100(BeamTrawlOffCoarse))
    disable("percentageBeamTrawlOffCoarseInput")
  })
  
  
  observeEvent(input$beamTrawlGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamTrawlInRock <- model$data$fleet.model$gear_habitat_activity$s0[4] 
    BeamTrawlInFine <- model$data$fleet.model$gear_habitat_activity$s1[4] 
    BeamTrawlInMed <- model$data$fleet.model$gear_habitat_activity$s2[4] 
    BeamTrawlInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[4]
    BeamTrawlOffRock <- model$data$fleet.model$gear_habitat_activity$d0[4] 
    BeamTrawlOffFine <- model$data$fleet.model$gear_habitat_activity$d1[4] 
    BeamTrawlOffMed <- model$data$fleet.model$gear_habitat_activity$d2[4] 
    BeamTrawlOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[4] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInBeamTrawl <- BeamTrawlInRock + BeamTrawlInFine + BeamTrawlInMed + BeamTrawlInCoarse
    totalOffBeamTrawl <- BeamTrawlOffRock + BeamTrawlOffFine + BeamTrawlOffMed + BeamTrawlOffCoarse
    totalOverallBeamTrawl <- totalInBeamTrawl + totalOffBeamTrawl
    percentageInBeamTrawlDefault <- totalInBeamTrawl/totalOverallBeamTrawl * 100
    percentageOutBeamTrawlDefault <- 100 - percentageInBeamTrawlDefault
    # Now getting percentages Inshore
    percentageBeamTrawlInRockDefault <- BeamTrawlInRock/totalInBeamTrawl * 100
    percentageBeamTrawlInFineDefault <- BeamTrawlInFine/totalInBeamTrawl * 100
    percentageBeamTrawlInMedDefault <- BeamTrawlInMed/totalInBeamTrawl * 100
    percentageBeamTrawlInCoarseDefault <- BeamTrawlInCoarse/totalInBeamTrawl * 100
    # Now getting percentages Offshore
    percentageBeamTrawlOffRockDefault <- BeamTrawlOffRock/totalOffBeamTrawl * 100
    percentageBeamTrawlOffFineDefault <- BeamTrawlOffFine/totalOffBeamTrawl * 100
    percentageBeamTrawlOffMedDefault <- BeamTrawlOffMed/totalOffBeamTrawl * 100
    percentageBeamTrawlOffCoarseDefault <- BeamTrawlOffCoarse/totalOffBeamTrawl * 100
    updateNumericInput(session, "inshorePercentageBeamTrawl", value = percentageInBeamTrawlDefault)
    updateNumericInput(session, "offshorePercentageBeamTrawl", value = percentageOutBeamTrawlDefault)
    updateNumericInput(session, "percentageBeamTrawlInRockInput", value = percentageBeamTrawlInRockDefault)
    updateNumericInput(session, "percentageBeamTrawlInFineInput", value = percentageBeamTrawlInFineDefault)
    updateNumericInput(session, "percentageBeamTrawlInMedInput", value = percentageBeamTrawlInMedDefault)
    updateNumericInput(session, "percentageBeamTrawlInCoarseInput", value = percentageBeamTrawlInCoarseDefault)
    updateNumericInput(session, "percentageBeamTrawlOffRockInput", value = percentageBeamTrawlOffRockDefault)
    updateNumericInput(session, "percentageBeamTrawlOffFineInput", value = percentageBeamTrawlOffFineDefault)
    updateNumericInput(session, "percentageBeamTrawlOffMedInput", value = percentageBeamTrawlOffMedDefault)
    updateNumericInput(session, "percentageBeamTrawlOffCoarseInput", value = percentageBeamTrawlOffCoarseDefault)
  })
  

  
  observeEvent(input$inshorePercentageDemSeine,{
    updateNumericInput(session, "inshorePercentageDemSeine", value = notGreatherThan100(input$inshorePercentageDemSeine))
    offShorePercent <- 100 - input$inshorePercentageDemSeine
    updateNumericInput(session, "offshorePercentageDemSeine", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageDemSeine,{
    updateNumericInput(session, "offshorePercentageDemSeine", value = input$offshorePercentageDemSeine)
    disable("offshorePercentageDemSeine")
  })
  
  output$percentageDemSeineInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageDemSeineInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageDemSeineInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemSeineInRock <- model$data$fleet.model$gear_habitat_activity$s0[5]
    DemSeineInFine <- model$data$fleet.model$gear_habitat_activity$s1[5]
    DemSeineInMed <- model$data$fleet.model$gear_habitat_activity$s2[5]
    DemSeineInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[5]
    totalInDemSeine <- DemSeineInRock + DemSeineInFine + DemSeineInMed + DemSeineInCoarse
    percentageDemSeineInMedDefault <- DemSeineInMed / totalInDemSeine * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    DemSeineInMed <- getMedValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea,medInArea,"percentageDemSeineInMedInput",percentageDemSeineInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageDemSeineInMedInput", "Inshore Medium %",DemSeineInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageDemSeineInMedInput", "Inshore Medium %",DemSeineInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageDemSeineInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemSeineInRock <- model$data$fleet.model$gear_habitat_activity$s0[5]
    DemSeineInFine <- model$data$fleet.model$gear_habitat_activity$s1[5]
    DemSeineInMed <- model$data$fleet.model$gear_habitat_activity$s2[5]
    DemSeineInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[5]
    totalInDemSeine <- DemSeineInRock + DemSeineInFine + DemSeineInMed + DemSeineInCoarse
    percentageDemSeineInFineDefault <- DemSeineInFine / totalInDemSeine * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    DemSeineInFine <- getFineValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea,medInArea,fineInArea,"percentageDemSeineInFineInput",percentageDemSeineInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageDemSeineInFineInput", "Inshore fine %",DemSeineInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageDemSeineInFineInput", "Inshore fine %",DemSeineInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageDemSeineInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   DemSeineInRock <- getRockValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageDemSeineInRockInput",percentageDemSeineInRockDefault)
  #   updateNumericInput(session,"percentageDemSeineInRockInput", value = notGreatherThan100(DemSeineInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageDemSeineInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageDemSeineInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   DemSeineInFine <- getFineValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea,medInArea,fineInArea,"percentageDemSeineInFineInput")
  #   updateNumericInput(session,"percentageDemSeineInFineInput", value = notGreatherThan100(DemSeineInFine))
  # })
  # 
  # observeEvent(input$percentageDemSeineInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   DemSeineInMed <- getMedValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea,medInArea,"percentageDemSeineInMedInput")
  #   updateNumericInput(session,"percentageDemSeineInMedInput",value = notGreatherThan100(DemSeineInMed))
  # })
  
  
  observeEvent(input$percentageDemSeineInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    DemSeineInCoarse <- getCoarseValue(input$percentageDemSeineInRockInput,input$percentageDemSeineInFineInput,input$percentageDemSeineInMedInput,coarseInArea)
    if ( DemSeineInCoarse < 0 || DemSeineInCoarse > 100)  {
      js$backgroundCol("percentageDemSeineInCoarseInput","red")
    } else {
      js$backgroundCol("percentageDemSeineInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageDemSeineInCoarseInput", value = notGreatherThan100(DemSeineInCoarse))
    disable("percentageDemSeineInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageDemSeineOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageDemSeineOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageDemSeineOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemSeineOffRock <- model$data$fleet.model$gear_habitat_activity$d0[5]
    DemSeineOffFine <- model$data$fleet.model$gear_habitat_activity$d1[5]
    DemSeineOffMed <- model$data$fleet.model$gear_habitat_activity$d2[5]
    DemSeineOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[5]
    totalOffDemSeine <- DemSeineOffRock + DemSeineOffFine + DemSeineOffMed + DemSeineOffCoarse
    percentageDemSeineOffMedDefault <- DemSeineOffMed / totalOffDemSeine * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    DemSeineOffMed <- getMedValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea,medOffArea,"percentageDemSeineOffMedInput",percentageDemSeineOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageDemSeineOffMedInput", "Offshore Medium %",DemSeineOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageDemSeineOffMedInput", "Offshore Medium %",DemSeineOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageDemSeineOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemSeineOffRock <- model$data$fleet.model$gear_habitat_activity$d0[5]
    DemSeineOffFine <- model$data$fleet.model$gear_habitat_activity$d1[5]
    DemSeineOffMed <- model$data$fleet.model$gear_habitat_activity$d2[5]
    DemSeineOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[5]
    totalOffDemSeine <- DemSeineOffRock + DemSeineOffFine + DemSeineOffMed + DemSeineOffCoarse
    percentageDemSeineOffFineDefault <- DemSeineOffFine / totalOffDemSeine * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    DemSeineOffFine <- getFineValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageDemSeineOffFineInput",percentageDemSeineOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageDemSeineOffFineInput", "Offshore fine %",DemSeineOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageDemSeineOffFineInput", "Offshore fine %",DemSeineOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageDemSeineOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   DemSeineOffRock <- getRockValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageDemSeineOffRockInput")
  #   updateNumericInput(session,"percentageDemSeineOffRockInput", value = notGreatherThan100(DemSeineOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageDemSeineOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageDemSeineOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   DemSeineOffFine <- getFineValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageDemSeineOffFineInput")
  #   updateNumericInput(session,"percentageDemSeineOffFineInput", value = notGreatherThan100(DemSeineOffFine))
  # })
  # 
  # observeEvent(input$percentageDemSeineOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   DemSeineOffMed <- getMedValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea,medOffArea,"percentageDemSeineOffMedInput")
  #   updateNumericInput(session,"percentageDemSeineOffMedInput",value = notGreatherThan100(DemSeineOffMed))
  # })
  
  observeEvent(input$percentageDemSeineOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    DemSeineOffCoarse <- getCoarseValue(input$percentageDemSeineOffRockInput,input$percentageDemSeineOffFineInput,input$percentageDemSeineOffMedInput,coarseOffArea)
    if ( DemSeineOffCoarse < 0 || DemSeineOffCoarse > 100)  {
      js$backgroundCol("percentageDemSeineOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageDemSeineOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageDemSeineOffCoarseInput", value = notGreatherThan100(DemSeineOffCoarse))
    disable("percentageDemSeineOffCoarseInput")
  })
  
  
  observeEvent(input$demSeineGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemSeineInRock <- model$data$fleet.model$gear_habitat_activity$s0[5] 
    DemSeineInFine <- model$data$fleet.model$gear_habitat_activity$s1[5] 
    DemSeineInMed <- model$data$fleet.model$gear_habitat_activity$s2[5] 
    DemSeineInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[5]
    DemSeineOffRock <- model$data$fleet.model$gear_habitat_activity$d0[5] 
    DemSeineOffFine <- model$data$fleet.model$gear_habitat_activity$d1[5] 
    DemSeineOffMed <- model$data$fleet.model$gear_habitat_activity$d2[5] 
    DemSeineOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[5] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInDemSeine <- DemSeineInRock + DemSeineInFine + DemSeineInMed + DemSeineInCoarse
    totalOffDemSeine <- DemSeineOffRock + DemSeineOffFine + DemSeineOffMed + DemSeineOffCoarse
    totalOverallDemSeine <- totalInDemSeine + totalOffDemSeine
    percentageInDemSeineDefault <- totalInDemSeine/totalOverallDemSeine * 100
    percentageOutDemSeineDefault <- 100 - percentageInDemSeineDefault
    # Now getting percentages Inshore
    percentageDemSeineInRockDefault <- DemSeineInRock/totalInDemSeine * 100
    percentageDemSeineInFineDefault <- DemSeineInFine/totalInDemSeine * 100
    percentageDemSeineInMedDefault <- DemSeineInMed/totalInDemSeine * 100
    percentageDemSeineInCoarseDefault <- DemSeineInCoarse/totalInDemSeine * 100
    # Now getting percentages Offshore
    percentageDemSeineOffRockDefault <- DemSeineOffRock/totalOffDemSeine * 100
    percentageDemSeineOffFineDefault <- DemSeineOffFine/totalOffDemSeine * 100
    percentageDemSeineOffMedDefault <- DemSeineOffMed/totalOffDemSeine * 100
    percentageDemSeineOffCoarseDefault <- DemSeineOffCoarse/totalOffDemSeine * 100
    updateNumericInput(session, "inshorePercentageDemSeine", value = percentageInDemSeineDefault)
    updateNumericInput(session, "offshorePercentageDemSeine", value = percentageOutDemSeineDefault)
    updateNumericInput(session, "percentageDemSeineInRockInput", value = percentageDemSeineInRockDefault)
    updateNumericInput(session, "percentageDemSeineInFineInput", value = percentageDemSeineInFineDefault)
    updateNumericInput(session, "percentageDemSeineInMedInput", value = percentageDemSeineInMedDefault)
    updateNumericInput(session, "percentageDemSeineInCoarseInput", value = percentageDemSeineInCoarseDefault)
    updateNumericInput(session, "percentageDemSeineOffRockInput", value = percentageDemSeineOffRockDefault)
    updateNumericInput(session, "percentageDemSeineOffFineInput", value = percentageDemSeineOffFineDefault)
    updateNumericInput(session, "percentageDemSeineOffMedInput", value = percentageDemSeineOffMedDefault)
    updateNumericInput(session, "percentageDemSeineOffCoarseInput", value = percentageDemSeineOffCoarseDefault)
  }) 
  
    
  
  observeEvent(input$inshorePercentageDemOtter,{
    updateNumericInput(session, "inshorePercentageDemOtter", value = notGreatherThan100(input$inshorePercentageDemOtter))
    offShorePercent <- 100 - input$inshorePercentageDemOtter
    updateNumericInput(session, "offshorePercentageDemOtter", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageDemOtter,{
    updateNumericInput(session, "offshorePercentageDemOtter", value = input$offshorePercentageDemOtter)
    disable("offshorePercentageDemOtter")
  })
  
  output$percentageDemOtterInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageDemOtterInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageDemOtterInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemOtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[6]
    DemOtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[6]
    DemOtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[6]
    DemOtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[6]
    totalInDemOtter <- DemOtterInRock + DemOtterInFine + DemOtterInMed + DemOtterInCoarse
    percentageDemOtterInMedDefault <- DemOtterInMed / totalInDemOtter * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    DemOtterInMed <- getMedValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea,medInArea,"percentageDemOtterInMedInput",percentageDemOtterInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageDemOtterInMedInput", "Inshore Medium %",DemOtterInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageDemOtterInMedInput", "Inshore Medium %",DemOtterInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageDemOtterInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemOtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[6]
    DemOtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[6]
    DemOtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[6]
    DemOtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[6]
    totalInDemOtter <- DemOtterInRock + DemOtterInFine + DemOtterInMed + DemOtterInCoarse
    percentageDemOtterInFineDefault <- DemOtterInFine / totalInDemOtter * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    DemOtterInFine <- getFineValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea,medInArea,fineInArea,"percentageDemOtterInFineInput",percentageDemOtterInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageDemOtterInFineInput", "Inshore fine %",DemOtterInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageDemOtterInFineInput", "Inshore fine %",DemOtterInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  # 
  # observeEvent(input$percentageDemOtterInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   DemOtterInRock <- getRockValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageDemOtterInRockInput",percentageDemOtterInRockDefault)
  #   updateNumericInput(session,"percentageDemOtterInRockInput", value = notGreatherThan100(DemOtterInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageDemOtterInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageDemOtterInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   DemOtterInFine <- getFineValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea,medInArea,fineInArea,"percentageDemOtterInFineInput")
  #   updateNumericInput(session,"percentageDemOtterInFineInput", value = notGreatherThan100(DemOtterInFine))
  # })
  # 
  # observeEvent(input$percentageDemOtterInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   DemOtterInMed <- getMedValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea,medInArea,"percentageDemOtterInMedInput")
  #   updateNumericInput(session,"percentageDemOtterInMedInput",value = notGreatherThan100(DemOtterInMed))
  # })
  # 
  
  observeEvent(input$percentageDemOtterInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    DemOtterInCoarse <- getCoarseValue(input$percentageDemOtterInRockInput,input$percentageDemOtterInFineInput,input$percentageDemOtterInMedInput,coarseInArea)
    if ( DemOtterInCoarse < 0 || DemOtterInCoarse > 100)  {
      js$backgroundCol("percentageDemOtterInCoarseInput","red")
    } else {
      js$backgroundCol("percentageDemOtterInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageDemOtterInCoarseInput", value = notGreatherThan100(DemOtterInCoarse))
    disable("percentageDemOtterInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageDemOtterOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageDemOtterOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageDemOtterOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemOtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[6]
    DemOtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[6]
    DemOtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[6]
    DemOtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[6]
    totalOffDemOtter <- DemOtterOffRock + DemOtterOffFine + DemOtterOffMed + DemOtterOffCoarse
    percentageDemOtterOffMedDefault <- DemOtterOffMed / totalOffDemOtter * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    DemOtterOffMed <- getMedValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea,medOffArea,"percentageDemOtterOffMedInput",percentageDemOtterOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageDemOtterOffMedInput", "Offshore Medium %",DemOtterOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageDemOtterOffMedInput", "Offshore Medium %",DemOtterOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageDemOtterOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemOtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[6]
    DemOtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[6]
    DemOtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[6]
    DemOtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[6]
    totalOffDemOtter <- DemOtterOffRock + DemOtterOffFine + DemOtterOffMed + DemOtterOffCoarse
    percentageDemOtterOffFineDefault <- DemOtterOffFine / totalOffDemOtter * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    DemOtterOffFine <- getFineValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageDemOtterOffFineInput",percentageDemOtterOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageDemOtterOffFineInput", "Offshore fine %",DemOtterOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageDemOtterOffFineInput", "Offshore fine %",DemOtterOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageDemOtterOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   DemOtterOffRock <- getRockValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageDemOtterOffRockInput")
  #   updateNumericInput(session,"percentageDemOtterOffRockInput", value = notGreatherThan100(DemOtterOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageDemOtterOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageDemOtterOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   DemOtterOffFine <- getFineValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageDemOtterOffFineInput")
  #   updateNumericInput(session,"percentageDemOtterOffFineInput", value = notGreatherThan100(DemOtterOffFine))
  # })
  # 
  # observeEvent(input$percentageDemOtterOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   DemOtterOffMed <- getMedValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea,medOffArea,"percentageDemOtterOffMedInput")
  #   updateNumericInput(session,"percentageDemOtterOffMedInput",value = notGreatherThan100(DemOtterOffMed))
  # })
  
  observeEvent(input$percentageDemOtterOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    DemOtterOffCoarse <- getCoarseValue(input$percentageDemOtterOffRockInput,input$percentageDemOtterOffFineInput,input$percentageDemOtterOffMedInput,coarseOffArea)
    if ( DemOtterOffCoarse < 0 || DemOtterOffCoarse > 100)  {
      js$backgroundCol("percentageDemOtterOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageDemOtterOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageDemOtterOffCoarseInput", value = notGreatherThan100(DemOtterOffCoarse))
    disable("percentageDemOtterOffCoarseInput")
  })
  
  
  observeEvent(input$demOtterGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    DemOtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[6] 
    DemOtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[6] 
    DemOtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[6] 
    DemOtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[6]
    DemOtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[6] 
    DemOtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[6] 
    DemOtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[6] 
    DemOtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[6] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInDemOtter <- DemOtterInRock + DemOtterInFine + DemOtterInMed + DemOtterInCoarse
    totalOffDemOtter <- DemOtterOffRock + DemOtterOffFine + DemOtterOffMed + DemOtterOffCoarse
    totalOverallDemOtter <- totalInDemOtter + totalOffDemOtter
    percentageInDemOtterDefault <- totalInDemOtter/totalOverallDemOtter * 100
    percentageOutDemOtterDefault <- 100 - percentageInDemOtterDefault
    # Now getting percentages Inshore
    percentageDemOtterInRockDefault <- DemOtterInRock/totalInDemOtter * 100
    percentageDemOtterInFineDefault <- DemOtterInFine/totalInDemOtter * 100
    percentageDemOtterInMedDefault <- DemOtterInMed/totalInDemOtter * 100
    percentageDemOtterInCoarseDefault <- DemOtterInCoarse/totalInDemOtter * 100
    # Now getting percentages Offshore
    percentageDemOtterOffRockDefault <- DemOtterOffRock/totalOffDemOtter * 100
    percentageDemOtterOffFineDefault <- DemOtterOffFine/totalOffDemOtter * 100
    percentageDemOtterOffMedDefault <- DemOtterOffMed/totalOffDemOtter * 100
    percentageDemOtterOffCoarseDefault <- DemOtterOffCoarse/totalOffDemOtter * 100
    updateNumericInput(session, "inshorePercentageDemOtter", value = percentageInDemOtterDefault)
    updateNumericInput(session, "offshorePercentageDemOtter", value = percentageOutDemOtterDefault)
    updateNumericInput(session, "percentageDemOtterInRockInput", value = percentageDemOtterInRockDefault)
    updateNumericInput(session, "percentageDemOtterInFineInput", value = percentageDemOtterInFineDefault)
    updateNumericInput(session, "percentageDemOtterInMedInput", value = percentageDemOtterInMedDefault)
    updateNumericInput(session, "percentageDemOtterInCoarseInput", value = percentageDemOtterInCoarseDefault)
    updateNumericInput(session, "percentageDemOtterOffRockInput", value = percentageDemOtterOffRockDefault)
    updateNumericInput(session, "percentageDemOtterOffFineInput", value = percentageDemOtterOffFineDefault)
    updateNumericInput(session, "percentageDemOtterOffMedInput", value = percentageDemOtterOffMedDefault)
    updateNumericInput(session, "percentageDemOtterOffCoarseInput", value = percentageDemOtterOffCoarseDefault)
  })
  
  observeEvent(input$inshorePercentageGillNet,{
    updateNumericInput(session, "inshorePercentageGillNet", value = notGreatherThan100(input$inshorePercentageGillNet))
    offShorePercent <- 100 - input$inshorePercentageGillNet
    updateNumericInput(session, "offshorePercentageGillNet", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageGillNet,{
    updateNumericInput(session, "offshorePercentageGillNet", value = input$offshorePercentageGillNet)
    disable("offshorePercentageGillNet")
  })
  
  output$percentageGillNetInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageGillNetInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageGillNetInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    GillNetInRock <- model$data$fleet.model$gear_habitat_activity$s0[7]
    GillNetInFine <- model$data$fleet.model$gear_habitat_activity$s1[7]
    GillNetInMed <- model$data$fleet.model$gear_habitat_activity$s2[7]
    GillNetInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[7]
    totalInGillNet <- GillNetInRock + GillNetInFine + GillNetInMed + GillNetInCoarse
    percentageGillNetInMedDefault <- GillNetInMed / totalInGillNet * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    GillNetInMed <- getMedValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea,medInArea,"percentageGillNetInMedInput",percentageGillNetInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageGillNetInMedInput", "Inshore Medium %",GillNetInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageGillNetInMedInput", "Inshore Medium %",GillNetInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageGillNetInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    GillNetInRock <- model$data$fleet.model$gear_habitat_activity$s0[7]
    GillNetInFine <- model$data$fleet.model$gear_habitat_activity$s1[7]
    GillNetInMed <- model$data$fleet.model$gear_habitat_activity$s2[7]
    GillNetInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[7]
    totalInGillNet <- GillNetInRock + GillNetInFine + GillNetInMed + GillNetInCoarse
    percentageGillNetInFineDefault <- GillNetInFine / totalInGillNet * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    GillNetInFine <- getFineValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea,medInArea,fineInArea,"percentageGillNetInFineInput",percentageGillNetInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageGillNetInFineInput", "Inshore fine %",GillNetInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageGillNetInFineInput", "Inshore fine %",GillNetInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageGillNetInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   GillNetInRock <- getRockValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageGillNetInRockInput",percentageGillNetInRockDefault)
  #   updateNumericInput(session,"percentageGillNetInRockInput", value = notGreatherThan100(GillNetInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageGillNetInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageGillNetInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   GillNetInFine <- getFineValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea,medInArea,fineInArea,"percentageGillNetInFineInput")
  #   updateNumericInput(session,"percentageGillNetInFineInput", value = notGreatherThan100(GillNetInFine))
  # })
  # 
  # observeEvent(input$percentageGillNetInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   GillNetInMed <- getMedValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea,medInArea,"percentageGillNetInMedInput")
  #   updateNumericInput(session,"percentageGillNetInMedInput",value = notGreatherThan100(GillNetInMed))
  # })
  # 
  
  observeEvent(input$percentageGillNetInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    GillNetInCoarse <- getCoarseValue(input$percentageGillNetInRockInput,input$percentageGillNetInFineInput,input$percentageGillNetInMedInput,coarseInArea)
    if ( GillNetInCoarse < 0 || GillNetInCoarse > 100)  {
      js$backgroundCol("percentageGillNetInCoarseInput","red")
    } else {
      js$backgroundCol("percentageGillNetInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageGillNetInCoarseInput", value = notGreatherThan100(GillNetInCoarse))
    disable("percentageGillNetInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageGillNetOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageGillNetOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageGillNetOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    GillNetOffRock <- model$data$fleet.model$gear_habitat_activity$d0[7]
    GillNetOffFine <- model$data$fleet.model$gear_habitat_activity$d1[7]
    GillNetOffMed <- model$data$fleet.model$gear_habitat_activity$d2[7]
    GillNetOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[7]
    totalOffGillNet <- GillNetOffRock + GillNetOffFine + GillNetOffMed + GillNetOffCoarse
    percentageGillNetOffMedDefault <- GillNetOffMed / totalOffGillNet * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    GillNetOffMed <- getMedValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea,medOffArea,"percentageGillNetOffMedInput",percentageGillNetOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageGillNetOffMedInput", "Offshore Medium %",GillNetOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageGillNetOffMedInput", "Offshore Medium %",GillNetOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageGillNetOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    GillNetOffRock <- model$data$fleet.model$gear_habitat_activity$d0[7]
    GillNetOffFine <- model$data$fleet.model$gear_habitat_activity$d1[7]
    GillNetOffMed <- model$data$fleet.model$gear_habitat_activity$d2[7]
    GillNetOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[7]
    totalOffGillNet <- GillNetOffRock + GillNetOffFine + GillNetOffMed + GillNetOffCoarse
    percentageGillNetOffFineDefault <- GillNetOffFine / totalOffGillNet * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    GillNetOffFine <- getFineValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageGillNetOffFineInput",percentageGillNetOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageGillNetOffFineInput", "Offshore fine %",GillNetOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageGillNetOffFineInput", "Offshore fine %",GillNetOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageGillNetOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   GillNetOffRock <- getRockValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageGillNetOffRockInput")
  #   updateNumericInput(session,"percentageGillNetOffRockInput", value = notGreatherThan100(GillNetOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageGillNetOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageGillNetOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   GillNetOffFine <- getFineValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageGillNetOffFineInput")
  #   updateNumericInput(session,"percentageGillNetOffFineInput", value = notGreatherThan100(GillNetOffFine))
  # })
  # 
  # observeEvent(input$percentageGillNetOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   GillNetOffMed <- getMedValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea,medOffArea,"percentageGillNetOffMedInput")
  #   updateNumericInput(session,"percentageGillNetOffMedInput",value = notGreatherThan100(GillNetOffMed))
  # })
  
  observeEvent(input$percentageGillNetOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    GillNetOffCoarse <- getCoarseValue(input$percentageGillNetOffRockInput,input$percentageGillNetOffFineInput,input$percentageGillNetOffMedInput,coarseOffArea)
    if ( GillNetOffCoarse < 0 || GillNetOffCoarse > 100)  {
      js$backgroundCol("percentageGillNetOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageGillNetOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageGillNetOffCoarseInput", value = notGreatherThan100(GillNetOffCoarse))
    disable("percentageGillNetOffCoarseInput")
  })
  
  observeEvent(input$gillNetGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    GillNetInRock <- model$data$fleet.model$gear_habitat_activity$s0[7] 
    GillNetInFine <- model$data$fleet.model$gear_habitat_activity$s1[7] 
    GillNetInMed <- model$data$fleet.model$gear_habitat_activity$s2[7] 
    GillNetInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[7]
    GillNetOffRock <- model$data$fleet.model$gear_habitat_activity$d0[7] 
    GillNetOffFine <- model$data$fleet.model$gear_habitat_activity$d1[7] 
    GillNetOffMed <- model$data$fleet.model$gear_habitat_activity$d2[7] 
    GillNetOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[7] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInGillNet <- GillNetInRock + GillNetInFine + GillNetInMed + GillNetInCoarse
    totalOffGillNet <- GillNetOffRock + GillNetOffFine + GillNetOffMed + GillNetOffCoarse
    totalOverallGillNet <- totalInGillNet + totalOffGillNet
    percentageInGillNetDefault <- totalInGillNet/totalOverallGillNet * 100
    percentageOutGillNetDefault <- 100 - percentageInGillNetDefault
    # Now getting percentages Inshore
    percentageGillNetInRockDefault <- GillNetInRock/totalInGillNet * 100
    percentageGillNetInFineDefault <- GillNetInFine/totalInGillNet * 100
    percentageGillNetInMedDefault <- GillNetInMed/totalInGillNet * 100
    percentageGillNetInCoarseDefault <- GillNetInCoarse/totalInGillNet * 100
    # Now getting percentages Offshore
    percentageGillNetOffRockDefault <- GillNetOffRock/totalOffGillNet * 100
    percentageGillNetOffFineDefault <- GillNetOffFine/totalOffGillNet * 100
    percentageGillNetOffMedDefault <- GillNetOffMed/totalOffGillNet * 100
    percentageGillNetOffCoarseDefault <- GillNetOffCoarse/totalOffGillNet * 100
    updateNumericInput(session, "inshorePercentageGillNet", value = percentageInGillNetDefault)
    updateNumericInput(session, "offshorePercentageGillNet", value = percentageOutGillNetDefault)
    updateNumericInput(session, "percentageGillNetInRockInput", value = percentageGillNetInRockDefault)
    updateNumericInput(session, "percentageGillNetInFineInput", value = percentageGillNetInFineDefault)
    updateNumericInput(session, "percentageGillNetInMedInput", value = percentageGillNetInMedDefault)
    updateNumericInput(session, "percentageGillNetInCoarseInput", value = percentageGillNetInCoarseDefault)
    updateNumericInput(session, "percentageGillNetOffRockInput", value = percentageGillNetOffRockDefault)
    updateNumericInput(session, "percentageGillNetOffFineInput", value = percentageGillNetOffFineDefault)
    updateNumericInput(session, "percentageGillNetOffMedInput", value = percentageGillNetOffMedDefault)
    updateNumericInput(session, "percentageGillNetOffCoarseInput", value = percentageGillNetOffCoarseDefault)
  })
  
  observeEvent(input$inshorePercentageBeamShrimp,{
    updateNumericInput(session, "inshorePercentageBeamShrimp", value = notGreatherThan100(input$inshorePercentageBeamShrimp))
    offShorePercent <- 100 - input$inshorePercentageBeamShrimp
    updateNumericInput(session, "offshorePercentageBeamShrimp", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageBeamShrimp,{
    updateNumericInput(session, "offshorePercentageBeamShrimp", value = input$offshorePercentageBeamShrimp)
    disable("offshorePercentageBeamShrimp")
  })
  
  output$percentageBeamShrimpInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageBeamShrimpInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageBeamShrimpInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamShrimpInRock <- model$data$fleet.model$gear_habitat_activity$s0[8]
    BeamShrimpInFine <- model$data$fleet.model$gear_habitat_activity$s1[8]
    BeamShrimpInMed <- model$data$fleet.model$gear_habitat_activity$s2[8]
    BeamShrimpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[8]
    totalInBeamShrimp <- BeamShrimpInRock + BeamShrimpInFine + BeamShrimpInMed + BeamShrimpInCoarse
    percentageBeamShrimpInMedDefault <- BeamShrimpInMed / totalInBeamShrimp * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    BeamShrimpInMed <- getMedValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea,medInArea,"percentageBeamShrimpInMedInput",percentageBeamShrimpInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageBeamShrimpInMedInput", "Inshore Medium %",BeamShrimpInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageBeamShrimpInMedInput", "Inshore Medium %",BeamShrimpInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageBeamShrimpInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamShrimpInRock <- model$data$fleet.model$gear_habitat_activity$s0[8]
    BeamShrimpInFine <- model$data$fleet.model$gear_habitat_activity$s1[8]
    BeamShrimpInMed <- model$data$fleet.model$gear_habitat_activity$s2[8]
    BeamShrimpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[8]
    totalInBeamShrimp <- BeamShrimpInRock + BeamShrimpInFine + BeamShrimpInMed + BeamShrimpInCoarse
    percentageBeamShrimpInFineDefault <- BeamShrimpInFine / totalInBeamShrimp * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    BeamShrimpInFine <- getFineValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea,medInArea,fineInArea,"percentageBeamShrimpInFineInput",percentageBeamShrimpInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageBeamShrimpInFineInput", "Inshore fine %",BeamShrimpInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageBeamShrimpInFineInput", "Inshore fine %",BeamShrimpInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageBeamShrimpInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   BeamShrimpInRock <- getRockValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageBeamShrimpInRockInput",percentageBeamShrimpInRockDefault)
  #   updateNumericInput(session,"percentageBeamShrimpInRockInput", value = notGreatherThan100(BeamShrimpInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageBeamShrimpInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageBeamShrimpInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   BeamShrimpInFine <- getFineValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea,medInArea,fineInArea,"percentageBeamShrimpInFineInput")
  #   updateNumericInput(session,"percentageBeamShrimpInFineInput", value = notGreatherThan100(BeamShrimpInFine))
  # })
  # 
  # observeEvent(input$percentageBeamShrimpInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   BeamShrimpInMed <- getMedValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea,medInArea,"percentageBeamShrimpInMedInput")
  #   updateNumericInput(session,"percentageBeamShrimpInMedInput",value = notGreatherThan100(BeamShrimpInMed))
  # })
  # 
  
  observeEvent(input$percentageBeamShrimpInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    BeamShrimpInCoarse <- getCoarseValue(input$percentageBeamShrimpInRockInput,input$percentageBeamShrimpInFineInput,input$percentageBeamShrimpInMedInput,coarseInArea)
    if ( BeamShrimpInCoarse < 0 || BeamShrimpInCoarse > 100)  {
      js$backgroundCol("percentageBeamShrimpInCoarseInput","red")
    } else {
      js$backgroundCol("percentageBeamShrimpInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageBeamShrimpInCoarseInput", value = notGreatherThan100(BeamShrimpInCoarse))
    disable("percentageBeamShrimpInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageBeamShrimpOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageBeamShrimpOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageBeamShrimpOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamShrimpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[8]
    BeamShrimpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[8]
    BeamShrimpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[8]
    BeamShrimpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[8]
    totalOffBeamShrimp <- BeamShrimpOffRock + BeamShrimpOffFine + BeamShrimpOffMed + BeamShrimpOffCoarse
    percentageBeamShrimpOffMedDefault <- BeamShrimpOffMed / totalOffBeamShrimp * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    BeamShrimpOffMed <- getMedValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea,medOffArea,"percentageBeamShrimpOffMedInput",percentageBeamShrimpOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageBeamShrimpOffMedInput", "Offshore Medium %",BeamShrimpOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageBeamShrimpOffMedInput", "Offshore Medium %",BeamShrimpOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageBeamShrimpOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamShrimpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[8]
    BeamShrimpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[8]
    BeamShrimpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[8]
    BeamShrimpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[8]
    totalOffBeamShrimp <- BeamShrimpOffRock + BeamShrimpOffFine + BeamShrimpOffMed + BeamShrimpOffCoarse
    percentageBeamShrimpOffFineDefault <- BeamShrimpOffFine / totalOffBeamShrimp * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    BeamShrimpOffFine <- getFineValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageBeamShrimpOffFineInput",percentageBeamShrimpOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageBeamShrimpOffFineInput", "Offshore fine %",BeamShrimpOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageBeamShrimpOffFineInput", "Offshore fine %",BeamShrimpOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageBeamShrimpOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   BeamShrimpOffRock <- getRockValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageBeamShrimpOffRockInput")
  #   updateNumericInput(session,"percentageBeamShrimpOffRockInput", value = notGreatherThan100(BeamShrimpOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageBeamShrimpOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageBeamShrimpOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   BeamShrimpOffFine <- getFineValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageBeamShrimpOffFineInput")
  #   updateNumericInput(session,"percentageBeamShrimpOffFineInput", value = notGreatherThan100(BeamShrimpOffFine))
  # })
  # 
  # observeEvent(input$percentageBeamShrimpOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   BeamShrimpOffMed <- getMedValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea,medOffArea,"percentageBeamShrimpOffMedInput")
  #   updateNumericInput(session,"percentageBeamShrimpOffMedInput",value = notGreatherThan100(BeamShrimpOffMed))
  # })
  
  observeEvent(input$percentageBeamShrimpOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    BeamShrimpOffCoarse <- getCoarseValue(input$percentageBeamShrimpOffRockInput,input$percentageBeamShrimpOffFineInput,input$percentageBeamShrimpOffMedInput,coarseOffArea)
    if ( BeamShrimpOffCoarse < 0 || BeamShrimpOffCoarse > 100)  {
      js$backgroundCol("percentageBeamShrimpOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageBeamShrimpOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageBeamShrimpOffCoarseInput", value = notGreatherThan100(BeamShrimpOffCoarse))
    disable("percentageBeamShrimpOffCoarseInput")
  })
  
  
  observeEvent(input$beamShrimpGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    BeamShrimpInRock <- model$data$fleet.model$gear_habitat_activity$s0[8] 
    BeamShrimpInFine <- model$data$fleet.model$gear_habitat_activity$s1[8] 
    BeamShrimpInMed <- model$data$fleet.model$gear_habitat_activity$s2[8] 
    BeamShrimpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[8]
    BeamShrimpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[8] 
    BeamShrimpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[8] 
    BeamShrimpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[8] 
    BeamShrimpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[8] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInBeamShrimp <- BeamShrimpInRock + BeamShrimpInFine + BeamShrimpInMed + BeamShrimpInCoarse
    totalOffBeamShrimp <- BeamShrimpOffRock + BeamShrimpOffFine + BeamShrimpOffMed + BeamShrimpOffCoarse
    totalOverallBeamShrimp <- totalInBeamShrimp + totalOffBeamShrimp
    percentageInBeamShrimpDefault <- totalInBeamShrimp/totalOverallBeamShrimp * 100
    percentageOutBeamShrimpDefault <- 100 - percentageInBeamShrimpDefault
    # Now getting percentages Inshore
    percentageBeamShrimpInRockDefault <- BeamShrimpInRock/totalInBeamShrimp * 100
    percentageBeamShrimpInFineDefault <- BeamShrimpInFine/totalInBeamShrimp * 100
    percentageBeamShrimpInMedDefault <- BeamShrimpInMed/totalInBeamShrimp * 100
    percentageBeamShrimpInCoarseDefault <- BeamShrimpInCoarse/totalInBeamShrimp * 100
    # Now getting percentages Offshore
    percentageBeamShrimpOffRockDefault <- BeamShrimpOffRock/totalOffBeamShrimp * 100
    percentageBeamShrimpOffFineDefault <- BeamShrimpOffFine/totalOffBeamShrimp * 100
    percentageBeamShrimpOffMedDefault <- BeamShrimpOffMed/totalOffBeamShrimp * 100
    percentageBeamShrimpOffCoarseDefault <- BeamShrimpOffCoarse/totalOffBeamShrimp * 100
    updateNumericInput(session, "inshorePercentageBeamShrimp", value = percentageInBeamShrimpDefault)
    updateNumericInput(session, "offshorePercentageBeamShrimp", value = percentageOutBeamShrimpDefault)
    updateNumericInput(session, "percentageBeamShrimpInRockInput", value = percentageBeamShrimpInRockDefault)
    updateNumericInput(session, "percentageBeamShrimpInFineInput", value = percentageBeamShrimpInFineDefault)
    updateNumericInput(session, "percentageBeamShrimpInMedInput", value = percentageBeamShrimpInMedDefault)
    updateNumericInput(session, "percentageBeamShrimpInCoarseInput", value = percentageBeamShrimpInCoarseDefault)
    updateNumericInput(session, "percentageBeamShrimpOffRockInput", value = percentageBeamShrimpOffRockDefault)
    updateNumericInput(session, "percentageBeamShrimpOffFineInput", value = percentageBeamShrimpOffFineDefault)
    updateNumericInput(session, "percentageBeamShrimpOffMedInput", value = percentageBeamShrimpOffMedDefault)
    updateNumericInput(session, "percentageBeamShrimpOffCoarseInput", value = percentageBeamShrimpOffCoarseDefault)
  })
  
  
  observeEvent(input$inshorePercentageNephropsTR2,{
    updateNumericInput(session, "inshorePercentageNephropsTR2", value = notGreatherThan100(input$inshorePercentageNephropsTR2))
    offShorePercent <- 100 - input$inshorePercentageNephropsTR2
    updateNumericInput(session, "offshorePercentageNephropsTR2", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageNephropsTR2,{
    updateNumericInput(session, "offshorePercentageNephropsTR2", value = input$offshorePercentageNephropsTR2)
    disable("offshorePercentageNephropsTR2")
  })
  
  output$percentageNephropsTR2InCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageNephropsTR2InCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageNephropsTR2InMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR2InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
    NephropsTR2InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
    NephropsTR2InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
    NephropsTR2InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    totalInNephropsTR2 <- NephropsTR2InRock + NephropsTR2InFine + NephropsTR2InMed + NephropsTR2InCoarse
    percentageNephropsTR2InMedDefault <- NephropsTR2InMed / totalInNephropsTR2 * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    NephropsTR2InMed <- getMedValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea,medInArea,"percentageNephropsTR2InMedInput",percentageNephropsTR2InMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageNephropsTR2InMedInput", "Inshore Medium %",NephropsTR2InMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageNephropsTR2InMedInput", "Inshore Medium %",NephropsTR2InMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageNephropsTR2InFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR2InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
    NephropsTR2InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
    NephropsTR2InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
    NephropsTR2InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    totalInNephropsTR2 <- NephropsTR2InRock + NephropsTR2InFine + NephropsTR2InMed + NephropsTR2InCoarse
    percentageNephropsTR2InFineDefault <- NephropsTR2InFine / totalInNephropsTR2 * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    NephropsTR2InFine <- getFineValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea,medInArea,fineInArea,"percentageNephropsTR2InFineInput",percentageNephropsTR2InFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageNephropsTR2InFineInput", "Inshore fine %",NephropsTR2InFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageNephropsTR2InFineInput", "Inshore fine %",NephropsTR2InFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageNephropsTR2InRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   NephropsTR2InRock <- getRockValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageNephropsTR2InRockInput",percentageNephropsTR2InRockDefault)
  #   updateNumericInput(session,"percentageNephropsTR2InRockInput", value = notGreatherThan100(NephropsTR2InRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageNephropsTR2InRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageNephropsTR2InFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   NephropsTR2InFine <- getFineValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea,medInArea,fineInArea,"percentageNephropsTR2InFineInput")
  #   updateNumericInput(session,"percentageNephropsTR2InFineInput", value = notGreatherThan100(NephropsTR2InFine))
  # })
  # 
  # observeEvent(input$percentageNephropsTR2InMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   NephropsTR2InMed <- getMedValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea,medInArea,"percentageNephropsTR2InMedInput")
  #   updateNumericInput(session,"percentageNephropsTR2InMedInput",value = notGreatherThan100(NephropsTR2InMed))
  # })
  
  
  observeEvent(input$percentageNephropsTR2InCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    NephropsTR2InCoarse <- getCoarseValue(input$percentageNephropsTR2InRockInput,input$percentageNephropsTR2InFineInput,input$percentageNephropsTR2InMedInput,coarseInArea)
    if ( NephropsTR2InCoarse < 0 || NephropsTR2InCoarse > 100)  {
      js$backgroundCol("percentageNephropsTR2InCoarseInput","red")
    } else {
      js$backgroundCol("percentageNephropsTR2InCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageNephropsTR2InCoarseInput", value = notGreatherThan100(NephropsTR2InCoarse))
    disable("percentageNephropsTR2InCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageNephropsTR2OffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageNephropsTR2OffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageNephropsTR2OffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR2OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
    NephropsTR2OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
    NephropsTR2OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9]
    NephropsTR2OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
    totalOffNephropsTR2 <- NephropsTR2OffRock + NephropsTR2OffFine + NephropsTR2OffMed + NephropsTR2OffCoarse
    percentageNephropsTR2OffMedDefault <- NephropsTR2OffMed / totalOffNephropsTR2 * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    NephropsTR2OffMed <- getMedValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea,medOffArea,"percentageNephropsTR2OffMedInput",percentageNephropsTR2OffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageNephropsTR2OffMedInput", "Offshore Medium %",NephropsTR2OffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageNephropsTR2OffMedInput", "Offshore Medium %",NephropsTR2OffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageNephropsTR2OffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR2OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
    NephropsTR2OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
    NephropsTR2OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9]
    NephropsTR2OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
    totalOffNephropsTR2 <- NephropsTR2OffRock + NephropsTR2OffFine + NephropsTR2OffMed + NephropsTR2OffCoarse
    percentageNephropsTR2OffFineDefault <- NephropsTR2OffFine / totalOffNephropsTR2 * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    NephropsTR2OffFine <- getFineValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageNephropsTR2OffFineInput",percentageNephropsTR2OffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageNephropsTR2OffFineInput", "Offshore fine %",NephropsTR2OffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageNephropsTR2OffFineInput", "Offshore fine %",NephropsTR2OffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageNephropsTR2OffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   NephropsTR2OffRock <- getRockValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageNephropsTR2OffRockInput")
  #   updateNumericInput(session,"percentageNephropsTR2OffRockInput", value = notGreatherThan100(NephropsTR2OffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageNephropsTR2OffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageNephropsTR2OffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   NephropsTR2OffFine <- getFineValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageNephropsTR2OffFineInput")
  #   updateNumericInput(session,"percentageNephropsTR2OffFineInput", value = notGreatherThan100(NephropsTR2OffFine))
  # })
  # 
  # observeEvent(input$percentageNephropsTR2OffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   NephropsTR2OffMed <- getMedValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea,medOffArea,"percentageNephropsTR2OffMedInput")
  #   updateNumericInput(session,"percentageNephropsTR2OffMedInput",value = notGreatherThan100(NephropsTR2OffMed))
  # })
  # 
  observeEvent(input$percentageNephropsTR2OffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    NephropsTR2OffCoarse <- getCoarseValue(input$percentageNephropsTR2OffRockInput,input$percentageNephropsTR2OffFineInput,input$percentageNephropsTR2OffMedInput,coarseOffArea)
    if ( NephropsTR2OffCoarse < 0 || NephropsTR2OffCoarse > 100)  {
      js$backgroundCol("percentageNephropsTR2OffCoarseInput","red")
    } else {
      js$backgroundCol("percentageNephropsTR2OffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageNephropsTR2OffCoarseInput", value = notGreatherThan100(NephropsTR2OffCoarse))
    disable("percentageNephropsTR2OffCoarseInput")
  })
  
  
  observeEvent(input$nephropsTR2GearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR2InRock <- model$data$fleet.model$gear_habitat_activity$s0[9] 
    NephropsTR2InFine <- model$data$fleet.model$gear_habitat_activity$s1[9] 
    NephropsTR2InMed <- model$data$fleet.model$gear_habitat_activity$s2[9] 
    NephropsTR2InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    NephropsTR2OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9] 
    NephropsTR2OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9] 
    NephropsTR2OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9] 
    NephropsTR2OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInNephropsTR2 <- NephropsTR2InRock + NephropsTR2InFine + NephropsTR2InMed + NephropsTR2InCoarse
    totalOffNephropsTR2 <- NephropsTR2OffRock + NephropsTR2OffFine + NephropsTR2OffMed + NephropsTR2OffCoarse
    totalOverallNephropsTR2 <- totalInNephropsTR2 + totalOffNephropsTR2
    percentageInNephropsTR2Default <- totalInNephropsTR2/totalOverallNephropsTR2 * 100
    percentageOutNephropsTR2Default <- 100 - percentageInNephropsTR2Default
    # Now getting percentages Inshore
    percentageNephropsTR2InRockDefault <- NephropsTR2InRock/totalInNephropsTR2 * 100
    percentageNephropsTR2InFineDefault <- NephropsTR2InFine/totalInNephropsTR2 * 100
    percentageNephropsTR2InMedDefault <- NephropsTR2InMed/totalInNephropsTR2 * 100
    percentageNephropsTR2InCoarseDefault <- NephropsTR2InCoarse/totalInNephropsTR2 * 100
    # Now getting percentages Offshore
    percentageNephropsTR2OffRockDefault <- NephropsTR2OffRock/totalOffNephropsTR2 * 100
    percentageNephropsTR2OffFineDefault <- NephropsTR2OffFine/totalOffNephropsTR2 * 100
    percentageNephropsTR2OffMedDefault <- NephropsTR2OffMed/totalOffNephropsTR2 * 100
    percentageNephropsTR2OffCoarseDefault <- NephropsTR2OffCoarse/totalOffNephropsTR2 * 100
    updateNumericInput(session, "inshorePercentageNephropsTR2", value = percentageInNephropsTR2Default)
    updateNumericInput(session, "offshorePercentageNephropsTR2", value = percentageOutNephropsTR2Default)
    updateNumericInput(session, "percentageNephropsTR2InRockInput", value = percentageNephropsTR2InRockDefault)
    updateNumericInput(session, "percentageNephropsTR2InFineInput", value = percentageNephropsTR2InFineDefault)
    updateNumericInput(session, "percentageNephropsTR2InMedInput", value = percentageNephropsTR2InMedDefault)
    updateNumericInput(session, "percentageNephropsTR2InCoarseInput", value = percentageNephropsTR2InCoarseDefault)
    updateNumericInput(session, "percentageNephropsTR2OffRockInput", value = percentageNephropsTR2OffRockDefault)
    updateNumericInput(session, "percentageNephropsTR2OffFineInput", value = percentageNephropsTR2OffFineDefault)
    updateNumericInput(session, "percentageNephropsTR2OffMedInput", value = percentageNephropsTR2OffMedDefault)
    updateNumericInput(session, "percentageNephropsTR2OffCoarseInput", value = percentageNephropsTR2OffCoarseDefault)
  })
  

  
  observeEvent(input$inshorePercentageNephropsTR3,{
    updateNumericInput(session, "inshorePercentageNephropsTR3", value = notGreatherThan100(input$inshorePercentageNephropsTR3))
    offShorePercent <- 100 - input$inshorePercentageNephropsTR3
    updateNumericInput(session, "offshorePercentageNephropsTR3", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageNephropsTR3,{
    updateNumericInput(session, "offshorePercentageNephropsTR3", value = input$offshorePercentageNephropsTR3)
    disable("offshorePercentageNephropsTR3")
  })
  
  output$percentageNephropsTR3InCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageNephropsTR3InCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageNephropsTR3InMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR3InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
    NephropsTR3InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
    NephropsTR3InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
    NephropsTR3InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    totalInNephropsTR3 <- NephropsTR3InRock + NephropsTR3InFine + NephropsTR3InMed + NephropsTR3InCoarse
    percentageNephropsTR3InMedDefault <- NephropsTR3InMed / totalInNephropsTR3 * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    NephropsTR3InMed <- getMedValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea,medInArea,"percentageNephropsTR3InMedInput",percentageNephropsTR3InMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageNephropsTR3InMedInput", "Inshore Medium %",NephropsTR3InMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageNephropsTR3InMedInput", "Inshore Medium %",NephropsTR3InMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageNephropsTR3InFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR3InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
    NephropsTR3InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
    NephropsTR3InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
    NephropsTR3InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    totalInNephropsTR3 <- NephropsTR3InRock + NephropsTR3InFine + NephropsTR3InMed + NephropsTR3InCoarse
    percentageNephropsTR3InFineDefault <- NephropsTR3InFine / totalInNephropsTR3 * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    NephropsTR3InFine <- getFineValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea,medInArea,fineInArea,"percentageNephropsTR3InFineInput",percentageNephropsTR3InFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageNephropsTR3InFineInput", "Inshore fine %",NephropsTR3InFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageNephropsTR3InFineInput", "Inshore fine %",NephropsTR3InFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageNephropsTR3InRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   NephropsTR3InRock <- getRockValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageNephropsTR3InRockInput",percentageNephropsTR3InRockDefault)
  #   updateNumericInput(session,"percentageNephropsTR3InRockInput", value = notGreatherThan100(NephropsTR3InRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageNephropsTR3InRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageNephropsTR3InFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   NephropsTR3InFine <- getFineValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea,medInArea,fineInArea,"percentageNephropsTR3InFineInput")
  #   updateNumericInput(session,"percentageNephropsTR3InFineInput", value = notGreatherThan100(NephropsTR3InFine))
  # })
  # 
  # observeEvent(input$percentageNephropsTR3InMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   NephropsTR3InMed <- getMedValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea,medInArea,"percentageNephropsTR3InMedInput")
  #   updateNumericInput(session,"percentageNephropsTR3InMedInput",value = notGreatherThan100(NephropsTR3InMed))
  # })
  
  
  observeEvent(input$percentageNephropsTR3InCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    NephropsTR3InCoarse <- getCoarseValue(input$percentageNephropsTR3InRockInput,input$percentageNephropsTR3InFineInput,input$percentageNephropsTR3InMedInput,coarseInArea)
    if ( NephropsTR3InCoarse < 0 || NephropsTR3InCoarse > 100)  {
      js$backgroundCol("percentageNephropsTR3InCoarseInput","red")
    } else {
      js$backgroundCol("percentageNephropsTR3InCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageNephropsTR3InCoarseInput", value = notGreatherThan100(NephropsTR3InCoarse))
    disable("percentageNephropsTR3InCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageNephropsTR3OffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageNephropsTR3OffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageNephropsTR3OffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR3OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
    NephropsTR3OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
    NephropsTR3OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9]
    NephropsTR3OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
    totalOffNephropsTR3 <- NephropsTR3OffRock + NephropsTR3OffFine + NephropsTR3OffMed + NephropsTR3OffCoarse
    percentageNephropsTR3OffMedDefault <- NephropsTR3OffMed / totalOffNephropsTR3 * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    NephropsTR3OffMed <- getMedValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea,medOffArea,"percentageNephropsTR3OffMedInput",percentageNephropsTR3OffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageNephropsTR3OffMedInput", "Offshore Medium %",NephropsTR3OffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageNephropsTR3OffMedInput", "Offshore Medium %",NephropsTR3OffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageNephropsTR3OffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR3OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
    NephropsTR3OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
    NephropsTR3OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9]
    NephropsTR3OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
    totalOffNephropsTR3 <- NephropsTR3OffRock + NephropsTR3OffFine + NephropsTR3OffMed + NephropsTR3OffCoarse
    percentageNephropsTR3OffFineDefault <- NephropsTR3OffFine / totalOffNephropsTR3 * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    NephropsTR3OffFine <- getFineValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageNephropsTR3OffFineInput",percentageNephropsTR3OffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageNephropsTR3OffFineInput", "Offshore fine %",NephropsTR3OffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageNephropsTR3OffFineInput", "Offshore fine %",NephropsTR3OffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageNephropsTR3OffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   NephropsTR3OffRock <- getRockValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageNephropsTR3OffRockInput")
  #   updateNumericInput(session,"percentageNephropsTR3OffRockInput", value = notGreatherThan100(NephropsTR3OffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageNephropsTR3OffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageNephropsTR3OffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   NephropsTR3OffFine <- getFineValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageNephropsTR3OffFineInput")
  #   updateNumericInput(session,"percentageNephropsTR3OffFineInput", value = notGreatherThan100(NephropsTR3OffFine))
  # })
  # 
  # observeEvent(input$percentageNephropsTR3OffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   NephropsTR3OffMed <- getMedValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea,medOffArea,"percentageNephropsTR3OffMedInput")
  #   updateNumericInput(session,"percentageNephropsTR3OffMedInput",value = notGreatherThan100(NephropsTR3OffMed))
  # })
  
  observeEvent(input$percentageNephropsTR3OffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    NephropsTR3OffCoarse <- getCoarseValue(input$percentageNephropsTR3OffRockInput,input$percentageNephropsTR3OffFineInput,input$percentageNephropsTR3OffMedInput,coarseOffArea)
    if ( NephropsTR3OffCoarse < 0 || NephropsTR3OffCoarse > 100)  {
      js$backgroundCol("percentageNephropsTR3OffCoarseInput","red")
    } else {
      js$backgroundCol("percentageNephropsTR3OffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageNephropsTR3OffCoarseInput", value = notGreatherThan100(NephropsTR3OffCoarse))
    disable("percentageNephropsTR3OffCoarseInput")
  })
  
  
  observeEvent(input$nephropsTR3GearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    NephropsTR3InRock <- model$data$fleet.model$gear_habitat_activity$s0[9] 
    NephropsTR3InFine <- model$data$fleet.model$gear_habitat_activity$s1[9] 
    NephropsTR3InMed <- model$data$fleet.model$gear_habitat_activity$s2[9] 
    NephropsTR3InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
    NephropsTR3OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9] 
    NephropsTR3OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9] 
    NephropsTR3OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9] 
    NephropsTR3OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInNephropsTR3 <- NephropsTR3InRock + NephropsTR3InFine + NephropsTR3InMed + NephropsTR3InCoarse
    totalOffNephropsTR3 <- NephropsTR3OffRock + NephropsTR3OffFine + NephropsTR3OffMed + NephropsTR3OffCoarse
    totalOverallNephropsTR3 <- totalInNephropsTR3 + totalOffNephropsTR3
    percentageInNephropsTR3Default <- totalInNephropsTR3/totalOverallNephropsTR3 * 100
    percentageOutNephropsTR3Default <- 100 - percentageInNephropsTR3Default
    # Now getting percentages Inshore
    percentageNephropsTR3InRockDefault <- NephropsTR3InRock/totalInNephropsTR3 * 100
    percentageNephropsTR3InFineDefault <- NephropsTR3InFine/totalInNephropsTR3 * 100
    percentageNephropsTR3InMedDefault <- NephropsTR3InMed/totalInNephropsTR3 * 100
    percentageNephropsTR3InCoarseDefault <- NephropsTR3InCoarse/totalInNephropsTR3 * 100
    # Now getting percentages Offshore
    percentageNephropsTR3OffRockDefault <- NephropsTR3OffRock/totalOffNephropsTR3 * 100
    percentageNephropsTR3OffFineDefault <- NephropsTR3OffFine/totalOffNephropsTR3 * 100
    percentageNephropsTR3OffMedDefault <- NephropsTR3OffMed/totalOffNephropsTR3 * 100
    percentageNephropsTR3OffCoarseDefault <- NephropsTR3OffCoarse/totalOffNephropsTR3 * 100
    updateNumericInput(session, "inshorePercentageNephropsTR3", value = percentageInNephropsTR3Default)
    updateNumericInput(session, "offshorePercentageNephropsTR3", value = percentageOutNephropsTR3Default)
    updateNumericInput(session, "percentageNephropsTR3InRockInput", value = percentageNephropsTR3InRockDefault)
    updateNumericInput(session, "percentageNephropsTR3InFineInput", value = percentageNephropsTR3InFineDefault)
    updateNumericInput(session, "percentageNephropsTR3InMedInput", value = percentageNephropsTR3InMedDefault)
    updateNumericInput(session, "percentageNephropsTR3InCoarseInput", value = percentageNephropsTR3InCoarseDefault)
    updateNumericInput(session, "percentageNephropsTR3OffRockInput", value = percentageNephropsTR3OffRockDefault)
    updateNumericInput(session, "percentageNephropsTR3OffFineInput", value = percentageNephropsTR3OffFineDefault)
    updateNumericInput(session, "percentageNephropsTR3OffMedInput", value = percentageNephropsTR3OffMedDefault)
    updateNumericInput(session, "percentageNephropsTR3OffCoarseInput", value = percentageNephropsTR3OffCoarseDefault)
  })
    
  
  observeEvent(input$inshorePercentageCreels,{
    updateNumericInput(session, "inshorePercentageCreels", value = notGreatherThan100(input$inshorePercentageCreels))
    offShorePercent <- 100 - input$inshorePercentageCreels
    updateNumericInput(session, "offshorePercentageCreels", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageCreels,{
    updateNumericInput(session, "offshorePercentageCreels", value = input$offshorePercentageCreels)
    disable("offshorePercentageCreels")
  })
  
  output$percentageCreelsInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageCreelsInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageCreelsInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    CreelsInRock <- model$data$fleet.model$gear_habitat_activity$s0[10]
    CreelsInFine <- model$data$fleet.model$gear_habitat_activity$s1[10]
    CreelsInMed <- model$data$fleet.model$gear_habitat_activity$s2[10]
    CreelsInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[10]
    totalInCreels <- CreelsInRock + CreelsInFine + CreelsInMed + CreelsInCoarse
    percentageCreelsInMedDefault <- CreelsInMed / totalInCreels * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    CreelsInMed <- getMedValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea,medInArea,"percentageCreelsInMedInput",percentageCreelsInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageCreelsInMedInput", "Inshore Medium %",CreelsInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageCreelsInMedInput", "Inshore Medium %",CreelsInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageCreelsInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    CreelsInRock <- model$data$fleet.model$gear_habitat_activity$s0[10]
    CreelsInFine <- model$data$fleet.model$gear_habitat_activity$s1[10]
    CreelsInMed <- model$data$fleet.model$gear_habitat_activity$s2[10]
    CreelsInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[10]
    totalInCreels <- CreelsInRock + CreelsInFine + CreelsInMed + CreelsInCoarse
    percentageCreelsInFineDefault <- CreelsInFine / totalInCreels * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    CreelsInFine <- getFineValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea,medInArea,fineInArea,"percentageCreelsInFineInput",percentageCreelsInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageCreelsInFineInput", "Inshore fine %",CreelsInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageCreelsInFineInput", "Inshore fine %",CreelsInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageCreelsInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   CreelsInRock <- getRockValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageCreelsInRockInput",percentageCreelsInRockDefault)
  #   updateNumericInput(session,"percentageCreelsInRockInput", value = notGreatherThan100(CreelsInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageCreelsInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageCreelsInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   CreelsInFine <- getFineValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea,medInArea,fineInArea,"percentageCreelsInFineInput")
  #   updateNumericInput(session,"percentageCreelsInFineInput", value = notGreatherThan100(CreelsInFine))
  # })
  # 
  # observeEvent(input$percentageCreelsInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   CreelsInMed <- getMedValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea,medInArea,"percentageCreelsInMedInput")
  #   updateNumericInput(session,"percentageCreelsInMedInput",value = notGreatherThan100(CreelsInMed))
  # })
  
  
  observeEvent(input$percentageCreelsInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    CreelsInCoarse <- getCoarseValue(input$percentageCreelsInRockInput,input$percentageCreelsInFineInput,input$percentageCreelsInMedInput,coarseInArea)
    if ( CreelsInCoarse < 0 || CreelsInCoarse > 100)  {
      js$backgroundCol("percentageCreelsInCoarseInput","red")
    } else {
      js$backgroundCol("percentageCreelsInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageCreelsInCoarseInput", value = notGreatherThan100(CreelsInCoarse))
    disable("percentageCreelsInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageCreelsOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageCreelsOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageCreelsOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    CreelsOffRock <- model$data$fleet.model$gear_habitat_activity$d0[10]
    CreelsOffFine <- model$data$fleet.model$gear_habitat_activity$d1[10]
    CreelsOffMed <- model$data$fleet.model$gear_habitat_activity$d2[10]
    CreelsOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[10]
    totalOffCreels <- CreelsOffRock + CreelsOffFine + CreelsOffMed + CreelsOffCoarse
    percentageCreelsOffMedDefault <- CreelsOffMed / totalOffCreels * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    CreelsOffMed <- getMedValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea,medOffArea,"percentageCreelsOffMedInput",percentageCreelsOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageCreelsOffMedInput", "Offshore Medium %",CreelsOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageCreelsOffMedInput", "Offshore Medium %",CreelsOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageCreelsOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    CreelsOffRock <- model$data$fleet.model$gear_habitat_activity$d0[10]
    CreelsOffFine <- model$data$fleet.model$gear_habitat_activity$d1[10]
    CreelsOffMed <- model$data$fleet.model$gear_habitat_activity$d2[10]
    CreelsOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[10]
    totalOffCreels <- CreelsOffRock + CreelsOffFine + CreelsOffMed + CreelsOffCoarse
    percentageCreelsOffFineDefault <- CreelsOffFine / totalOffCreels * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    CreelsOffFine <- getFineValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageCreelsOffFineInput",percentageCreelsOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageCreelsOffFineInput", "Offshore fine %",CreelsOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageCreelsOffFineInput", "Offshore fine %",CreelsOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageCreelsOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   CreelsOffRock <- getRockValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageCreelsOffRockInput")
  #   updateNumericInput(session,"percentageCreelsOffRockInput", value = notGreatherThan100(CreelsOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageCreelsOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageCreelsOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   CreelsOffFine <- getFineValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageCreelsOffFineInput")
  #   updateNumericInput(session,"percentageCreelsOffFineInput", value = notGreatherThan100(CreelsOffFine))
  # })
  # 
  # observeEvent(input$percentageCreelsOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   CreelsOffMed <- getMedValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea,medOffArea,"percentageCreelsOffMedInput")
  #   updateNumericInput(session,"percentageCreelsOffMedInput",value = notGreatherThan100(CreelsOffMed))
  # })
  
  observeEvent(input$percentageCreelsOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    CreelsOffCoarse <- getCoarseValue(input$percentageCreelsOffRockInput,input$percentageCreelsOffFineInput,input$percentageCreelsOffMedInput,coarseOffArea)
    if ( CreelsOffCoarse < 0 || CreelsOffCoarse > 100)  {
      js$backgroundCol("percentageCreelsOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageCreelsOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageCreelsOffCoarseInput", value = notGreatherThan100(CreelsOffCoarse))
    disable("percentageCreelsOffCoarseInput")
  })
  
  
  observeEvent(input$creelsGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    CreelsInRock <- model$data$fleet.model$gear_habitat_activity$s0[10] 
    CreelsInFine <- model$data$fleet.model$gear_habitat_activity$s1[10] 
    CreelsInMed <- model$data$fleet.model$gear_habitat_activity$s2[10] 
    CreelsInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[10]
    CreelsOffRock <- model$data$fleet.model$gear_habitat_activity$d0[10] 
    CreelsOffFine <- model$data$fleet.model$gear_habitat_activity$d1[10] 
    CreelsOffMed <- model$data$fleet.model$gear_habitat_activity$d2[10] 
    CreelsOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[10] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInCreels <- CreelsInRock + CreelsInFine + CreelsInMed + CreelsInCoarse
    totalOffCreels <- CreelsOffRock + CreelsOffFine + CreelsOffMed + CreelsOffCoarse
    totalOverallCreels <- totalInCreels + totalOffCreels
    percentageInCreelsDefault <- totalInCreels/totalOverallCreels * 100
    percentageOutCreelsDefault <- 100 - percentageInCreelsDefault
    # Now getting percentages Inshore
    percentageCreelsInRockDefault <- CreelsInRock/totalInCreels * 100
    percentageCreelsInFineDefault <- CreelsInFine/totalInCreels * 100
    percentageCreelsInMedDefault <- CreelsInMed/totalInCreels * 100
    percentageCreelsInCoarseDefault <- CreelsInCoarse/totalInCreels * 100
    # Now getting percentages Offshore
    percentageCreelsOffRockDefault <- CreelsOffRock/totalOffCreels * 100
    percentageCreelsOffFineDefault <- CreelsOffFine/totalOffCreels * 100
    percentageCreelsOffMedDefault <- CreelsOffMed/totalOffCreels * 100
    percentageCreelsOffCoarseDefault <- CreelsOffCoarse/totalOffCreels * 100
    updateNumericInput(session, "inshorePercentageCreels", value = percentageInCreelsDefault)
    updateNumericInput(session, "offshorePercentageCreels", value = percentageOutCreelsDefault)
    updateNumericInput(session, "percentageCreelsInRockInput", value = percentageCreelsInRockDefault)
    updateNumericInput(session, "percentageCreelsInFineInput", value = percentageCreelsInFineDefault)
    updateNumericInput(session, "percentageCreelsInMedInput", value = percentageCreelsInMedDefault)
    updateNumericInput(session, "percentageCreelsInCoarseInput", value = percentageCreelsInCoarseDefault)
    updateNumericInput(session, "percentageCreelsOffRockInput", value = percentageCreelsOffRockDefault)
    updateNumericInput(session, "percentageCreelsOffFineInput", value = percentageCreelsOffFineDefault)
    updateNumericInput(session, "percentageCreelsOffMedInput", value = percentageCreelsOffMedDefault)
    updateNumericInput(session, "percentageCreelsOffCoarseInput", value = percentageCreelsOffCoarseDefault)
  })
  
  
  observeEvent(input$inshorePercentageMollusc,{
    updateNumericInput(session, "inshorePercentageMollusc", value = notGreatherThan100(input$inshorePercentageMollusc))
    offShorePercent <- 100 - input$inshorePercentageMollusc
    updateNumericInput(session, "offshorePercentageMollusc", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageMollusc,{
    updateNumericInput(session, "offshorePercentageMollusc", value = input$offshorePercentageMollusc)
    disable("offshorePercentageMollusc")
  })
  
  output$percentageMolluscInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageMolluscInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageMolluscInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    MolluscInRock <- model$data$fleet.model$gear_habitat_activity$s0[11]
    MolluscInFine <- model$data$fleet.model$gear_habitat_activity$s1[11]
    MolluscInMed <- model$data$fleet.model$gear_habitat_activity$s2[11]
    MolluscInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[11]
    totalInMollusc <- MolluscInRock + MolluscInFine + MolluscInMed + MolluscInCoarse
    percentageMolluscInMedDefault <- MolluscInMed / totalInMollusc * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    MolluscInMed <- getMedValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea,medInArea,"percentageMolluscInMedInput",percentageMolluscInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageMolluscInMedInput", "Inshore Medium %",MolluscInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageMolluscInMedInput", "Inshore Medium %",MolluscInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageMolluscInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    MolluscInRock <- model$data$fleet.model$gear_habitat_activity$s0[11]
    MolluscInFine <- model$data$fleet.model$gear_habitat_activity$s1[11]
    MolluscInMed <- model$data$fleet.model$gear_habitat_activity$s2[11]
    MolluscInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[11]
    totalInMollusc <- MolluscInRock + MolluscInFine + MolluscInMed + MolluscInCoarse
    percentageMolluscInFineDefault <- MolluscInFine / totalInMollusc * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    MolluscInFine <- getFineValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea,medInArea,fineInArea,"percentageMolluscInFineInput",percentageMolluscInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageMolluscInFineInput", "Inshore fine %",MolluscInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageMolluscInFineInput", "Inshore fine %",MolluscInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageMolluscInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   MolluscInRock <- getRockValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageMolluscInRockInput",percentageMolluscInRockDefault)
  #   updateNumericInput(session,"percentageMolluscInRockInput", value = notGreatherThan100(MolluscInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageMolluscInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageMolluscInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   MolluscInFine <- getFineValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea,medInArea,fineInArea,"percentageMolluscInFineInput")
  #   updateNumericInput(session,"percentageMolluscInFineInput", value = notGreatherThan100(MolluscInFine))
  # })
  # 
  # observeEvent(input$percentageMolluscInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   MolluscInMed <- getMedValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea,medInArea,"percentageMolluscInMedInput")
  #   updateNumericInput(session,"percentageMolluscInMedInput",value = notGreatherThan100(MolluscInMed))
  # })
  
  
  observeEvent(input$percentageMolluscInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    MolluscInCoarse <- getCoarseValue(input$percentageMolluscInRockInput,input$percentageMolluscInFineInput,input$percentageMolluscInMedInput,coarseInArea)
    if ( MolluscInCoarse < 0 || MolluscInCoarse > 100)  {
      js$backgroundCol("percentageMolluscInCoarseInput","red")
    } else {
      js$backgroundCol("percentageMolluscInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageMolluscInCoarseInput", value = notGreatherThan100(MolluscInCoarse))
    disable("percentageMolluscInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageMolluscOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageMolluscOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageMolluscOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    MolluscOffRock <- model$data$fleet.model$gear_habitat_activity$d0[11]
    MolluscOffFine <- model$data$fleet.model$gear_habitat_activity$d1[11]
    MolluscOffMed <- model$data$fleet.model$gear_habitat_activity$d2[11]
    MolluscOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[11]
    totalOffMollusc <- MolluscOffRock + MolluscOffFine + MolluscOffMed + MolluscOffCoarse
    percentageMolluscOffMedDefault <- MolluscOffMed / totalOffMollusc * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    MolluscOffMed <- getMedValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea,medOffArea,"percentageMolluscOffMedInput",percentageMolluscOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageMolluscOffMedInput", "Offshore Medium %",MolluscOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageMolluscOffMedInput", "Offshore Medium %",MolluscOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageMolluscOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    MolluscOffRock <- model$data$fleet.model$gear_habitat_activity$d0[11]
    MolluscOffFine <- model$data$fleet.model$gear_habitat_activity$d1[11]
    MolluscOffMed <- model$data$fleet.model$gear_habitat_activity$d2[11]
    MolluscOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[11]
    totalOffMollusc <- MolluscOffRock + MolluscOffFine + MolluscOffMed + MolluscOffCoarse
    percentageMolluscOffFineDefault <- MolluscOffFine / totalOffMollusc * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    MolluscOffFine <- getFineValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageMolluscOffFineInput",percentageMolluscOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageMolluscOffFineInput", "Offshore fine %",MolluscOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageMolluscOffFineInput", "Offshore fine %",MolluscOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageMolluscOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   MolluscOffRock <- getRockValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageMolluscOffRockInput")
  #   updateNumericInput(session,"percentageMolluscOffRockInput", value = notGreatherThan100(MolluscOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageMolluscOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageMolluscOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   MolluscOffFine <- getFineValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageMolluscOffFineInput")
  #   updateNumericInput(session,"percentageMolluscOffFineInput", value = notGreatherThan100(MolluscOffFine))
  # })
  # 
  # observeEvent(input$percentageMolluscOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   MolluscOffMed <- getMedValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea,medOffArea,"percentageMolluscOffMedInput")
  #   updateNumericInput(session,"percentageMolluscOffMedInput",value = notGreatherThan100(MolluscOffMed))
  # })
  
  observeEvent(input$percentageMolluscOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    MolluscOffCoarse <- getCoarseValue(input$percentageMolluscOffRockInput,input$percentageMolluscOffFineInput,input$percentageMolluscOffMedInput,coarseOffArea)
    if ( MolluscOffCoarse < 0 || MolluscOffCoarse > 100)  {
      js$backgroundCol("percentageMolluscOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageMolluscOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageMolluscOffCoarseInput", value = notGreatherThan100(MolluscOffCoarse))
    disable("percentageMolluscOffCoarseInput")
  })
  
  
  observeEvent(input$molluscGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    MolluscInRock <- model$data$fleet.model$gear_habitat_activity$s0[11] 
    MolluscInFine <- model$data$fleet.model$gear_habitat_activity$s1[11] 
    MolluscInMed <- model$data$fleet.model$gear_habitat_activity$s2[11] 
    MolluscInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[11]
    MolluscOffRock <- model$data$fleet.model$gear_habitat_activity$d0[11] 
    MolluscOffFine <- model$data$fleet.model$gear_habitat_activity$d1[11] 
    MolluscOffMed <- model$data$fleet.model$gear_habitat_activity$d2[11] 
    MolluscOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[11] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInMollusc <- MolluscInRock + MolluscInFine + MolluscInMed + MolluscInCoarse
    totalOffMollusc <- MolluscOffRock + MolluscOffFine + MolluscOffMed + MolluscOffCoarse
    totalOverallMollusc <- totalInMollusc + totalOffMollusc
    percentageInMolluscDefault <- totalInMollusc/totalOverallMollusc * 100
    percentageOutMolluscDefault <- 100 - percentageInMolluscDefault
    # Now getting percentages Inshore
    percentageMolluscInRockDefault <- MolluscInRock/totalInMollusc * 100
    percentageMolluscInFineDefault <- MolluscInFine/totalInMollusc * 100
    percentageMolluscInMedDefault <- MolluscInMed/totalInMollusc * 100
    percentageMolluscInCoarseDefault <- MolluscInCoarse/totalInMollusc * 100
    # Now getting percentages Offshore
    percentageMolluscOffRockDefault <- MolluscOffRock/totalOffMollusc * 100
    percentageMolluscOffFineDefault <- MolluscOffFine/totalOffMollusc * 100
    percentageMolluscOffMedDefault <- MolluscOffMed/totalOffMollusc * 100
    percentageMolluscOffCoarseDefault <- MolluscOffCoarse/totalOffMollusc * 100
    updateNumericInput(session, "inshorePercentageMollusc", value = percentageInMolluscDefault)
    updateNumericInput(session, "offshorePercentageMollusc", value = percentageOutMolluscDefault)
    updateNumericInput(session, "percentageMolluscInRockInput", value = percentageMolluscInRockDefault)
    updateNumericInput(session, "percentageMolluscInFineInput", value = percentageMolluscInFineDefault)
    updateNumericInput(session, "percentageMolluscInMedInput", value = percentageMolluscInMedDefault)
    updateNumericInput(session, "percentageMolluscInCoarseInput", value = percentageMolluscInCoarseDefault)
    updateNumericInput(session, "percentageMolluscOffRockInput", value = percentageMolluscOffRockDefault)
    updateNumericInput(session, "percentageMolluscOffFineInput", value = percentageMolluscOffFineDefault)
    updateNumericInput(session, "percentageMolluscOffMedInput", value = percentageMolluscOffMedDefault)
    updateNumericInput(session, "percentageMolluscOffCoarseInput", value = percentageMolluscOffCoarseDefault)
  })
  

  observeEvent(input$inshorePercentageWhaler,{
    updateNumericInput(session, "inshorePercentageWhaler", value = notGreatherThan100(input$inshorePercentageWhaler))
    offShorePercent <- 100 - input$inshorePercentageWhaler
    updateNumericInput(session, "offshorePercentageWhaler", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageWhaler,{
    updateNumericInput(session, "offshorePercentageWhaler", value = input$offshorePercentageWhaler)
    disable("offshorePercentageWhaler")
  })
  
  output$percentageWhalerInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageWhalerInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageWhalerInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    WhalerInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
    WhalerInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
    WhalerInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
    WhalerInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    totalInWhaler <- WhalerInRock + WhalerInFine + WhalerInMed + WhalerInCoarse
    percentageWhalerInMedDefault <- WhalerInMed / totalInWhaler * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    WhalerInMed <- getMedValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea,medInArea,"percentageWhalerInMedInput",percentageWhalerInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageWhalerInMedInput", "Inshore Medium %",WhalerInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageWhalerInMedInput", "Inshore Medium %",WhalerInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageWhalerInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    WhalerInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
    WhalerInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
    WhalerInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
    WhalerInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    totalInWhaler <- WhalerInRock + WhalerInFine + WhalerInMed + WhalerInCoarse
    percentageWhalerInFineDefault <- WhalerInFine / totalInWhaler * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    WhalerInFine <- getFineValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea,medInArea,fineInArea,"percentageWhalerInFineInput",percentageWhalerInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageWhalerInFineInput", "Inshore fine %",WhalerInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageWhalerInFineInput", "Inshore fine %",WhalerInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  
  # observeEvent(input$percentageWhalerInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   WhalerInRock <- getRockValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageWhalerInRockInput",percentageWhalerInRockDefault)
  #   updateNumericInput(session,"percentageWhalerInRockInput", value = notGreatherThan100(WhalerInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageWhalerInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageWhalerInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   WhalerInFine <- getFineValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea,medInArea,fineInArea,"percentageWhalerInFineInput")
  #   updateNumericInput(session,"percentageWhalerInFineInput", value = notGreatherThan100(WhalerInFine))
  # })
  # 
  # observeEvent(input$percentageWhalerInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   WhalerInMed <- getMedValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea,medInArea,"percentageWhalerInMedInput")
  #   updateNumericInput(session,"percentageWhalerInMedInput",value = notGreatherThan100(WhalerInMed))
  # })
  
  
  observeEvent(input$percentageWhalerInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    WhalerInCoarse <- getCoarseValue(input$percentageWhalerInRockInput,input$percentageWhalerInFineInput,input$percentageWhalerInMedInput,coarseInArea)
    if ( WhalerInCoarse < 0 || WhalerInCoarse > 100)  {
      js$backgroundCol("percentageWhalerInCoarseInput","red")
    } else {
      js$backgroundCol("percentageWhalerInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageWhalerInCoarseInput", value = notGreatherThan100(WhalerInCoarse))
    disable("percentageWhalerInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageWhalerOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageWhalerOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageWhalerOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    WhalerOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
    WhalerOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
    WhalerOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12]
    WhalerOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
    totalOffWhaler <- WhalerOffRock + WhalerOffFine + WhalerOffMed + WhalerOffCoarse
    percentageWhalerOffMedDefault <- WhalerOffMed / totalOffWhaler * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    WhalerOffMed <- getMedValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea,medOffArea,"percentageWhalerOffMedInput",percentageWhalerOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageWhalerOffMedInput", "Offshore Medium %",WhalerOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageWhalerOffMedInput", "Offshore Medium %",WhalerOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageWhalerOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    WhalerOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
    WhalerOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
    WhalerOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12]
    WhalerOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
    totalOffWhaler <- WhalerOffRock + WhalerOffFine + WhalerOffMed + WhalerOffCoarse
    percentageWhalerOffFineDefault <- WhalerOffFine / totalOffWhaler * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    WhalerOffFine <- getFineValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageWhalerOffFineInput",percentageWhalerOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageWhalerOffFineInput", "Offshore fine %",WhalerOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageWhalerOffFineInput", "Offshore fine %",WhalerOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageWhalerOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   WhalerOffRock <- getRockValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageWhalerOffRockInput")
  #   updateNumericInput(session,"percentageWhalerOffRockInput", value = notGreatherThan100(WhalerOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageWhalerOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageWhalerOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   WhalerOffFine <- getFineValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageWhalerOffFineInput")
  #   updateNumericInput(session,"percentageWhalerOffFineInput", value = notGreatherThan100(WhalerOffFine))
  # })
  # 
  # observeEvent(input$percentageWhalerOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   WhalerOffMed <- getMedValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea,medOffArea,"percentageWhalerOffMedInput")
  #   updateNumericInput(session,"percentageWhalerOffMedInput",value = notGreatherThan100(WhalerOffMed))
  # })
  
  observeEvent(input$percentageWhalerOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    WhalerOffCoarse <- getCoarseValue(input$percentageWhalerOffRockInput,input$percentageWhalerOffFineInput,input$percentageWhalerOffMedInput,coarseOffArea)
    if ( WhalerOffCoarse < 0 || WhalerOffCoarse > 100)  {
      js$backgroundCol("percentageWhalerOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageWhalerOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageWhalerOffCoarseInput", value = notGreatherThan100(WhalerOffCoarse))
    disable("percentageWhalerOffCoarseInput")
  })
  
  
  observeEvent(input$whalerGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    WhalerInRock <- model$data$fleet.model$gear_habitat_activity$s0[12] 
    WhalerInFine <- model$data$fleet.model$gear_habitat_activity$s1[12] 
    WhalerInMed <- model$data$fleet.model$gear_habitat_activity$s2[12] 
    WhalerInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    WhalerOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12] 
    WhalerOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12] 
    WhalerOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12] 
    WhalerOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInWhaler <- WhalerInRock + WhalerInFine + WhalerInMed + WhalerInCoarse
    totalOffWhaler <- WhalerOffRock + WhalerOffFine + WhalerOffMed + WhalerOffCoarse
    totalOverallWhaler <- totalInWhaler + totalOffWhaler
    percentageInWhalerDefault <- totalInWhaler/totalOverallWhaler * 100
    percentageOutWhalerDefault <- 100 - percentageInWhalerDefault
    # Now getting percentages Inshore
    if(totalInWhaler>0){
      percentageWhalerInRockDefault <- WhalerInRock/totalInWhaler * 100
    } else {
      percentageWhalerInRockDefault <- 0
    }
    if(totalInWhaler>0){
      percentageWhalerInFineDefault <- WhalerInFine/totalInWhaler * 100
    } else {
      percentageWhalerInFineDefault <- 0
    }
    if(totalInWhaler>0){
      percentageWhalerInMedDefault <- WhalerInMed/totalInWhaler * 100
    } else {
      percentageWhalerInMedDefault <- 0
    }
    if(totalInWhaler>0){
      percentageWhalerInCoarseDefault <- WhalerInCoarse/totalInWhaler * 100
    } else {
      percentageWhalerInCoarseDefault <- 0
    }
    
    # Now getting percentages Offshore
    if(totalOffWhaler>0){
      percentageWhalerOffRockDefault <- WhalerOffRock/totalOffWhaler * 100
    } else {
      percentageWhalerOffRockDefault <- 0
    }
    if(totalOffWhaler>0){
      percentageWhalerOffFineDefault <- WhalerOffFine/totalOffWhaler * 100
    } else {
      percentageWhalerOffFineDefault <- 0
    }
    if(totalOffWhaler>0){
      percentageWhalerOffMedDefault <- WhalerOffMed/totalOffWhaler * 100
    } else {
      percentageWhalerOffMedDefault <- 0
    }
    if(totalOffWhaler>0){
      percentageWhalerOffCoarseDefault <- WhalerOffCoarse/totalOffWhaler * 100
    } else {
      percentageWhalerOffCoarseDefault <- 0
    }
    updateNumericInput(session, "inshorePercentageWhaler", value = percentageInWhalerDefault)
    updateNumericInput(session, "offshorePercentageWhaler", value = percentageOutWhalerDefault)
    updateNumericInput(session, "percentageWhalerInRockInput", value = percentageWhalerInRockDefault)
    updateNumericInput(session, "percentageWhalerInFineInput", value = percentageWhalerInFineDefault)
    updateNumericInput(session, "percentageWhalerInMedInput", value = percentageWhalerInMedDefault)
    updateNumericInput(session, "percentageWhalerInCoarseInput", value = percentageWhalerInCoarseDefault)
    updateNumericInput(session, "percentageWhalerOffRockInput", value = percentageWhalerOffRockDefault)
    updateNumericInput(session, "percentageWhalerOffFineInput", value = percentageWhalerOffFineDefault)
    updateNumericInput(session, "percentageWhalerOffMedInput", value = percentageWhalerOffMedDefault)
    updateNumericInput(session, "percentageWhalerOffCoarseInput", value = percentageWhalerOffCoarseDefault)
  })
  
  
  observeEvent(input$inshorePercentageKelp,{
    updateNumericInput(session, "inshorePercentageKelp", value = notGreatherThan100(input$inshorePercentageKelp))
    offShorePercent <- 100 - input$inshorePercentageKelp
    updateNumericInput(session, "offshorePercentageKelp", value = offShorePercent)
  })
  
  observeEvent(input$offshorePercentageKelp,{
    updateNumericInput(session, "offshorePercentageKelp", value = input$offshorePercentageKelp)
    disable("offshorePercentageKelp")
  })
  
  output$percentageKelpInCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    coarseValueIn <- getCoarseValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea)
    if(coarseInArea >0 ){disabled(numericInput("percentageKelpInCoarseInput", "Inshore coarse %",coarseValueIn,width = '50%'))}
  })
  
  output$percentageKelpInMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    KelpInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
    KelpInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
    KelpInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
    KelpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    totalInKelp <- KelpInRock + KelpInFine + KelpInMed + KelpInCoarse
    percentageKelpInMedDefault <- KelpInMed / totalInKelp * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    KelpInMed <- getMedValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea,medInArea,"percentageKelpInMedInput",percentageKelpInMedDefault)
    if(medInArea >0 && coarseInArea==0 ){
      disabled(numericInput("percentageKelpInMedInput", "Inshore Medium %",KelpInMed,width = '50%'))
    } else if(medInArea >0) {
      numericInput("percentageKelpInMedInput", "Inshore Medium %",KelpInMed,width = '50%')
    } else {
      # Don't want field if medInArea is zero
    }
  })
  
  
  output$percentageKelpInFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    KelpInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
    KelpInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
    KelpInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
    KelpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    totalInKelp <- KelpInRock + KelpInFine + KelpInMed + KelpInCoarse
    percentageKelpInFineDefault <- KelpInFine / totalInKelp * 100
    coarseInArea <- model$data$physical.parameters$x_area_s3
    medInArea <- model$data$physical.parameters$x_area_s2
    fineInArea <-  model$data$physical.parameters$x_area_s1
    KelpInFine <- getFineValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea,medInArea,fineInArea,"percentageKelpInFineInput",percentageKelpInFineDefault)
    if(fineInArea >0 && medInArea==0 && coarseInArea==0 ){
      disabled(numericInput("percentageKelpInFineInput", "Inshore fine %",KelpInFine,width = '50%'))
    } else if (fineInArea >0) {
      numericInput("percentageKelpInFineInput", "Inshore fine %",KelpInFine,width = '50%')
    } else {
      # Don't want field if fineInArea is zero
    }
  })
  
  # 
  # observeEvent(input$percentageKelpInRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   rockInArea <- model$data$physical.parameters$x_area_s0
  #   KelpInRock <- getRockValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea,medInArea,fineInArea,rockInArea,"percentageKelpInRockInput",percentageKelpInRockDefault)
  #   updateNumericInput(session,"percentageKelpInRockInput", value = notGreatherThan100(KelpInRock))
  #   if(medInArea==0 && fineInArea==0 && coarseInArea==0){
  #     disable("percentageKelpInRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageKelpInFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   fineInArea <- model$data$physical.parameters$x_area_s1
  #   KelpInFine <- getFineValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea,medInArea,fineInArea,"percentageKelpInFineInput")
  #   updateNumericInput(session,"percentageKelpInFineInput", value = notGreatherThan100(KelpInFine))
  # })
  # 
  # observeEvent(input$percentageKelpInMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseInArea <- model$data$physical.parameters$x_area_s3
  #   medInArea <- model$data$physical.parameters$x_area_s2
  #   KelpInMed <- getMedValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea,medInArea,"percentageKelpInMedInput")
  #   updateNumericInput(session,"percentageKelpInMedInput",value = notGreatherThan100(KelpInMed))
  # })
  
  
  observeEvent(input$percentageKelpInCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseInArea <- model$data$physical.parameters$x_area_s3
    KelpInCoarse <- getCoarseValue(input$percentageKelpInRockInput,input$percentageKelpInFineInput,input$percentageKelpInMedInput,coarseInArea)
    if ( KelpInCoarse < 0 || KelpInCoarse > 100)  {
      js$backgroundCol("percentageKelpInCoarseInput","red")
    } else {
      js$backgroundCol("percentageKelpInCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageKelpInCoarseInput", value = notGreatherThan100(KelpInCoarse))
    disable("percentageKelpInCoarseInput")
  })
  
  ##### Off Starts here
  
  output$percentageKelpOffCoarse <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    coarseValueOff <- getCoarseValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea)
    if(coarseOffArea>0) {disabled(numericInput("percentageKelpOffCoarseInput", "Offshore coarse %",coarseValueOff,width = '50%'))}
  })
  
  output$percentageKelpOffMed <- renderUI({ 
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    KelpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
    KelpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
    KelpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12]
    KelpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
    totalOffKelp <- KelpOffRock + KelpOffFine + KelpOffMed + KelpOffCoarse
    percentageKelpOffMedDefault <- KelpOffMed / totalOffKelp * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    KelpOffMed <- getMedValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea,medOffArea,"percentageKelpOffMedInput",percentageKelpOffMedDefault)
    if(medOffArea >0 && coarseOffArea==0 ){
      disabled(numericInput("percentageKelpOffMedInput", "Offshore Medium %",KelpOffMed,width = '50%'))
    } else if(medOffArea >0) {
      numericInput("percentageKelpOffMedInput", "Offshore Medium %",KelpOffMed,width = '50%')
    } else {
      # Don't want field if medOffArea is zero
    }
  })
  
  output$percentageKelpOffFine <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    KelpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
    KelpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
    KelpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12]
    KelpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
    totalOffKelp <- KelpOffRock + KelpOffFine + KelpOffMed + KelpOffCoarse
    percentageKelpOffFineDefault <- KelpOffFine / totalOffKelp * 100
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    medOffArea <- model$data$physical.parameters$x_area_d2
    fineOffArea <-  model$data$physical.parameters$x_area_d1
    KelpOffFine <- getFineValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageKelpOffFineInput",percentageKelpOffFineDefault)
    if(fineOffArea >0 && medOffArea==0 && coarseOffArea==0 ){
      disabled(numericOffput("percentageKelpOffFineInput", "Offshore fine %",KelpOffFine,width = '50%'))
    } else if (fineOffArea >0) {
      numericInput("percentageKelpOffFineInput", "Offshore fine %",KelpOffFine,width = '50%')
    } else {
      # Don't want field if fineOffArea is zero
    }
  })
  
  
  # observeEvent(input$percentageKelpOffRockInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   #model <- model_reactive()
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   rockOffArea <- model$data$physical.parameters$x_area_d0
  #   KelpOffRock <- getRockValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea,medOffArea,fineOffArea,rockOffArea,"percentageKelpOffRockInput")
  #   updateNumericInput(session,"percentageKelpOffRockInput", value = notGreatherThan100(KelpOffRock))
  #   if(medOffArea==0 && fineOffArea==0 && coarseOffArea==0){
  #     disable("percentageKelpOffRockInput")
  #   }
  # })
  # 
  # observeEvent(input$percentageKelpOffFineInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   fineOffArea <- model$data$physical.parameters$x_area_d1
  #   KelpOffFine <- getFineValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea,medOffArea,fineOffArea,"percentageKelpOffFineInput")
  #   updateNumericInput(session,"percentageKelpOffFineInput", value = notGreatherThan100(KelpOffFine))
  # })
  # 
  # observeEvent(input$percentageKelpOffMedInput,{
  #   model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
  #   coarseOffArea <- model$data$physical.parameters$x_area_d3
  #   medOffArea <- model$data$physical.parameters$x_area_d2
  #   KelpOffMed <- getMedValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea,medOffArea,"percentageKelpOffMedInput")
  #   updateNumericInput(session,"percentageKelpOffMedInput",value = notGreatherThan100(KelpOffMed))
  # })
  # 
  observeEvent(input$percentageKelpOffCoarseInput,{
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    coarseOffArea <- model$data$physical.parameters$x_area_d3
    KelpOffCoarse <- getCoarseValue(input$percentageKelpOffRockInput,input$percentageKelpOffFineInput,input$percentageKelpOffMedInput,coarseOffArea)
    if ( KelpOffCoarse < 0 || KelpOffCoarse > 100)  {
      js$backgroundCol("percentageKelpOffCoarseInput","red")
    } else {
      js$backgroundCol("percentageKelpOffCoarseInput","light grey")
    }
    updateNumericInput(session,"percentageKelpOffCoarseInput", value = notGreatherThan100(KelpOffCoarse))
    disable("percentageKelpOffCoarseInput")
  })
  
  
  observeEvent(input$kelpGearPerHab_reset, {
    #Note need check here to make sure selectedlocation and  selectedVariant are set
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    KelpInRock <- model$data$fleet.model$gear_habitat_activity$s0[12] 
    KelpInFine <- model$data$fleet.model$gear_habitat_activity$s1[12] 
    KelpInMed <- model$data$fleet.model$gear_habitat_activity$s2[12] 
    KelpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
    KelpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12] 
    KelpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12] 
    KelpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12] 
    KelpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12] 
    
    rockInArea <- model$data$physical.parameters$x_area_s0
    fineInArea <- model$data$physical.parameters$x_area_s1 
    medInArea <-  model$data$physical.parameters$x_area_s2 
    coarseInArea <- model$data$physical.parameters$x_area_s3
    rockOffArea <- model$data$physical.parameters$x_area_d0 
    fineOffArea <- model$data$physical.parameters$x_area_d1 
    medOffArea <- model$data$physical.parameters$x_area_d2
    coarseOffArea <- model$data$physical.parameters$x_area_d3 
    # Getting Total Inshore/Offshore here
    totalInKelp <- KelpInRock + KelpInFine + KelpInMed + KelpInCoarse
    totalOffKelp <- KelpOffRock + KelpOffFine + KelpOffMed + KelpOffCoarse
    totalOverallKelp <- totalInKelp + totalOffKelp
    percentageInKelpDefault <- totalInKelp/totalOverallKelp * 100
    percentageOutKelpDefault <- 100 - percentageInKelpDefault
    # Now getting percentages Inshore
    if(totalInKelp>0){
      percentageKelpInRockDefault <- KelpInRock/totalInKelp * 100
    } else {
      percentageKelpInRockDefault <- 0
    }
    if(totalInKelp>0){
      percentageKelpInFineDefault <- KelpInFine/totalInKelp * 100
    } else {
      percentageKelpInFineDefault <- 0
    }
    if(totalInKelp>0){
      percentageKelpInMedDefault <- KelpInMed/totalInKelp * 100
    } else {
      percentageKelpInMedDefault <- 0
    }
    if(totalInKelp>0){
      percentageKelpInCoarseDefault <- KelpInCoarse/totalInKelp * 100
    } else {
      percentageKelpInCoarseDefault <- 0
    }
    # Now getting percentages Offshore
    if(totalOffKelp>0){
      percentageKelpOffRockDefault <- KelpOffRock/totalOffKelp * 100
    } else {
      percentageKelpOffRockDefault <- 0
    }
    if(totalOffKelp>0){
      percentageKelpOffFineDefault <- KelpOffFine/totalOffKelp * 100
    } else {
      percentageKelpOffFineDefault <- 0
    }
    if(totalOffKelp>0){
      percentageKelpOffMedDefault <- KelpOffMed/totalOffKelp * 100
    } else {
      percentageKelpOffMedDefault <- 0
    }
    if(totalOffKelp>0){
      percentageKelpOffCoarseDefault <- KelpOffCoarse/totalOffKelp * 100
    } else {
      percentageKelpOffCoarseDefault <- 0
    }
    updateNumericInput(session, "inshorePercentageKelp", value = percentageInKelpDefault)
    updateNumericInput(session, "offshorePercentageKelp", value = percentageOutKelpDefault)
    updateNumericInput(session, "percentageKelpInRockInput", value = percentageKelpInRockDefault)
    updateNumericInput(session, "percentageKelpInFineInput", value = percentageKelpInFineDefault)
    updateNumericInput(session, "percentageKelpInMedInput", value = percentageKelpInMedDefault)
    updateNumericInput(session, "percentageKelpInCoarseInput", value = percentageKelpInCoarseDefault)
    updateNumericInput(session, "percentageKelpOffRockInput", value = percentageKelpOffRockDefault)
    updateNumericInput(session, "percentageKelpOffFineInput", value = percentageKelpOffFineDefault)
    updateNumericInput(session, "percentageKelpOffMedInput", value = percentageKelpOffMedDefault)
    updateNumericInput(session, "percentageKelpOffCoarseInput", value = percentageKelpOffCoarseDefault)
  })
    
  
  output$uiFishingActivity <- renderUI({
    # current chosen model on dropdown lists
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    createFishingActivityUI(model)
  })
    
  
  output$uiSeabedAbrasian <- renderUI({
    # current chosen model on dropdown lists
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    createSeabedAbrasianUI(model)
  })
  
  chooseGear <- reactive({ ## This all works locally - but not on server (populates dropdown with number without this)- so adding hacky belts and braces 
    selectedModel <- input$selectedlocation
    selectedYears <- input$selectedVariant
    # current chosen model on dropdown lists
    model <- e2e_read(selectedModel, selectedYears, models.path="Models")
    if(selectedModel == "North_Sea"){
      gears = list(
        "Pelagic_Trawl+Seine",
        "Sandeel+sprat_trawl(Otter30-70mm+TR3)",
        "Longline_mackerel",
        "Beam_Trawl_BT1+BT2",
        "Demersal_Seine",
        "Demersal_Otter_Trawl_TR1",
        "Gill_Nets+Longline_demersal",
        "Beam_Trawl_shrimp",
        "Nephrops_Trawl_TR2",
        "Creels",
        "Mollusc_Dredge",
        "Whaler"
      )
    } else if (selectedModel == "Celtic_Sea"){
      gears = list(
        "Pelagic_Trawl+Seine",
        "Otter30-70mm+TR3(sandeel+sprat)",
        "Longline_mackerel",
        "Beam_Trawl_BT1+BT2",
        "Demersal_Seine",
        "Demersal_Otter_Trawl_TR1",
        "Gill_Nets+Longline_demersal",
        "Beam_Trawl_shrimp",
        "Nephrops_Trawl_TR3",
        "Creels",
        "Mollusc_Dredge",
        "KelpHarvester"
      )
    } else { # Not sure if this will still populate dropdown list with numbers for new models
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
    gears <- list(gear1,gear2,gear3,gear4,gear5,gear6,gear7,gear8,gear9,gear10,gear11,gear12)
    }
    })
  
  output$gear_dropdown <- renderUI({
    selectedModel <- input$selectedlocation
    selectedYears <- input$selectedVariant
    # current chosen model on dropdown lists
    model <- e2e_read(selectedModel, selectedYears, models.path="Models")
    
    if(is.null(model)){
    gears = list(
      "Pelagic_Trawl+Seine",
      "Sandeel+sprat_trawl(Otter30-70mm+TR3)",
      "Longline_mackerel",
      "Beam_Trawl_BT1+BT2",
      "Demersal_Seine",
      "Demersal_Otter_Trawl_TR1",
      "Gill_Nets+Longline_demersal",
      "Beam_Trawl_shrimp",
      "Nephrops_Trawl_TR2",
      "Creels",
      "Mollusc_Dredge",
      "Whaler"
    )}
    else {
      gears = chooseGear()
    }
    selectInput("outputGearType",
                "Select Gear",
                choices = gears,
       selected = "Pelagic_Trawl+Seine"
    )
  })
  
  output$gear_forhabitat_dropdown <- renderUI({
    selectedModel <- input$selectedlocation
    selectedYears <- input$selectedVariant
    # current chosen model on dropdown lists
    model <- e2e_read(selectedModel, selectedYears, models.path="Models")
    
    if(is.null(model)){
      gears = list(
        "Pelagic_Trawl+Seine",
        "Sandeel+sprat_trawl(Otter30-70mm+TR3)",
        "Longline_mackerel",
        "Beam_Trawl_BT1+BT2",
        "Demersal_Seine",
        "Demersal_Otter_Trawl_TR1",
        "Gill_Nets+Longline_demersal",
        "Beam_Trawl_shrimp",
        "Nephrops_Trawl_TR2",
        "Creels",
        "Mollusc_Dredge",
        "Whaler"
      )}
    else {
      gears = chooseGear()
    }
    selectInput("selectedGearForHabitatDist",
                "Select Gear",
                choices = gears,
                selected = "Pelagic_Trawl+Seine"
    )
  })
  
  output$variant_dropdown <- renderUI({
    # current chosen model on dropdown lost
    modelChosen <- input$selectedlocation
    modelChosenPath <- file.path(getwd(), "Models", modelChosen)
    variants <- list.dirs(path = modelChosenPath, full.names = FALSE, recursive=FALSE)
    variants <- as.list(variants)
    selectInput("selectedVariant", 
                "Select Time Period",
                choices = variants
    )
  })
  
  output$textSelectRegion <- renderText(quoted = FALSE,"Blue areas in the map above represent the inshore zone of the model, orange offshore. D1, D2, D3, S1, S2, S3 refers to seabed habitats - D/S1 = mud, D/S2= sand, D/S3 = gravel. Rock is denoted by D/S0 or shown as a separate map of percentage of rock cover.")
 
  output$textSpecificRegion <- renderUI(
                                        if(is.null(input$selectedlocation)){
                                          fluidRow(HTML("<br><p style = \"font-family: 'calibri'; font-si16pt \">Full documentation of the configuration and inputs for the North Sea model is provided <a href='https://marineresourcemodelling.gitlab.io/resources/StrathE2E2/documents/3.3.0/StrathE2E2_North_Sea_model.pdf'>here</a>. The model has been extensively validated against independent observational data"))}
                                          else if(input$selectedlocation == "North_Sea"){
                                            fluidRow(HTML("<br><p style = \"font-family: 'calibri'; font-si16pt \">Full documentation of the configuration and inputs for the North Sea model is provided <a href='https://marineresourcemodelling.gitlab.io/resources/StrathE2E2/documents/3.3.0/StrathE2E2_North_Sea_model.pdf'>here</a> The model has been extensively validated against independent observational data"))}
                                          else if(input$selectedlocation == "Celtic_Sea"){
                                            fluidRow(HTML("<br><p style = \"font-family: 'calibri'; font-si16pt \">Sources of data for inputs to the Celtic Sea model were the same as for the North Sea, which is documented <a href='https://marineresourcemodelling.gitlab.io/resources/StrathE2E2/documents/3.3.0/StrathE2E2_North_Sea_model.pdf'>here</a>"))}
                                          else {
                                            fluidRow(HTML("<br><p style = \"font-family: 'calibri'; font-si16pt \">Full documentation of the configuration and inputs for the North Sea model is provided <a href='https://marineresourcemodelling.gitlab.io/resources/StrathE2E2/documents/3.3.0/StrathE2E2_North_Sea_model.pdf'>here</a> The model has been extensively validated against independent observational data"))}
                                        )
    
  output$textEco <- renderUI(
                               if(is.null(input$outputEcoType) || is.null(input$Nutrient_Phytoplankton_Tab) ){# && is.null(input$Zooplankton_Tab) && is.null(input$Fish_Tab) && is.null(input$Benthos_Tab) && is.null(input$Predators_Tab) && is.null(input$Corpse_Discard_Tab) && is.null(input$Macrophyte_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of organic detritus and associated bacteria suspended in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is approximately equivalent to 0.16 grams of material per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Nutrient_Phytoplankton" && input$Nutrient_Phytoplankton_Tab == "nut_phyt_Detritus") {
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of organic detritus and associated bacteria suspended in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is approximately equivalent to 0.16 grams of material per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Nutrient_Phytoplankton" && input$Nutrient_Phytoplankton_Tab == "nut_phyt_Ammonia"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of ammonia dissolved in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Nutrient_Phytoplankton" && input$Nutrient_Phytoplankton_Tab == "nut_phyt_Nitrate"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of nitrate dissolved in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup> is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Nutrient_Phytoplankton" && input$Nutrient_Phytoplankton_Tab == "nut_phyt_Phytoplankton"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of phytoplankton in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is approximately equivalent to 0.5 grams of live weight per m<sup>3</sup>"))}
                               else if(is.null(input$outputEcoType) || is.null(input$Sediment_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of ammonia dissolved in the water within  seabed sediments (\"porewaters\") of the inshore zone. s1 = muddy sediment, s2 = sandy, s3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedInAmmonia"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of ammonia dissolved in the water within  seabed sediments (\"porewaters\") of the inshore zone. s1 = muddy sediment, s2 = sandy, s3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedOffAmmonia"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of ammonia dissolved in the water within  seabed sediments (\"porewaters\") of the offshore zone. d1 = muddy sediment, d2 = sandy, d3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedInNitrate"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of nitrate dissolved in the water within  seabed sediments (\"porewaters\") of the inshore zone. s1 = muddy sediment, s2 = sandy, s3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedOffNitrate"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of nitrate dissolved in the water within  seabed sediments (\"porewaters\") of the offshore zone. d1 = muddy sediment, d2 = sandy, d3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is equivalent to 0.014 grams of nitrogen per m<sup>3</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedInDetrius"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the organic detritus and bacteria content of  seabed sediments of the inshore zone. s1 = muddy sediment, s2 = sandy, s3 = gravelly. Units: Grams of organic nitrogen per gram of dry sediment, expressed as a percentage"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedOffDetrius"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the organic detritus and bacteria content of  seabed sediments of the offshore zone. d1 = muddy sediment, d2 = sandy, d3 = gravelly. Units: Grams of organic nitrogen per gram of dry sediment, expressed as a percentage"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedInCorpses"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of dead animals  (from plankton to whales) in the seabed habitats of the inshore zone. s1 = muddy sediment, s2 = sandy, s3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 0.5 grams of material per m<sup>2</sup>"))}
                               else if(input$outputEcoType == "Sediment" && input$Sediment_Tab == "sedOffCorpses"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of dead animals  (from plankton to whales) in the seabed habitats of the offshore zone. d1 = muddy sediment, d2 = sandy, d3 = gravelly. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 0.5 grams of material per m<sup>2</sup> "))}
                               else if(is.null(input$outputEcoType) || is.null(input$Zooplankton_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of omnivorous zooplankton in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))} 
                               else if(input$outputEcoType == "Zooplankton" && input$Zooplankton_Tab == "zooOmni"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of omnivorous zooplankton in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))} 
                               else if(input$outputEcoType == "Zooplankton" && input$Zooplankton_Tab == "zooCarn"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of carnivorous zooplankton in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))} 
                               else if(is.null(input$outputEcoType) || is.null(input$Fish_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of planktivorous (plankton-eating) fish in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Fish" && input$Fish_Tab == "plankFish"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of planktivorous (plankton-eating) fish in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Fish" && input$Fish_Tab == "plankFishLarv"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of larvae of planktivorous (plankton-eating) fish in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Fish" && input$Fish_Tab == "demFish"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of demersal (benthos and fish-eating) fish in the inshore and offshore zones.Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Fish" && input$Fish_Tab == "demFishLarv"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of larvae of demersal (benthos and fish-eating) fish in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(is.null(input$outputEcoType) || is.null(input$Benthos_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of suspension and deposit feeding benthos (seabed -living invertebrate animals) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of seabed"))}
                               else if(input$outputEcoType == "Benthos" && input$Benthos_Tab == "benSusFeed"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of suspension and deposit feeding benthos (seabed -living invertebrate animals) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of seabed"))}
                               else if(input$outputEcoType == "Benthos" && input$Benthos_Tab == "benSusLarvFeed"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of planktonic larvae of suspension and deposit feeding benthos (seabed -living invertebrate animals) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1  km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Benthos" && input$Benthos_Tab == "benCarnFeed"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of carnivorous and scavenge feeding benthos (seabed -living invertebrate animals) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of seabed"))}
                               else if(input$outputEcoType == "Benthos" && input$Benthos_Tab == "benCarnLarvFeed"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of planktonic larvae of carnivorous and scavenge feeding benthos (seabed -living invertebrate animals) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1  km<sup>2</sup> of sea surface"))}
                               else if(is.null(input$outputEcoType) || is.null(input$Predators_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of seabirds in the inshore and offshore zones. Units: 0.01 milli-Mole of nitrogen per m<sup>2</sup> (0.01 mMN.m<sup>-2</sup>) is approximately equivalent to 5 kg of live weight per km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Predators" && input$Predators_Tab == "predBirds"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of seabirds in the inshore and offshore zones. Units: 0.01 milli-Mole of nitrogen per m<sup>2</sup> (0.01 mMN.m<sup>-2</sup>) is approximately equivalent to 5 kg of live weight per km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Predators" && input$Predators_Tab == "predPinnipeds"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of pinnipeds (seals) in the inshore and offshore zones. Units: 0.01 milli-Mole of nitrogen per m<sup>2</sup> (0.01 mMN.m<sup>-2</sup>) is approximately equivalent to 5 kg of live weight per km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Predators" && input$Predators_Tab == "predCetaceans"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of cetaceans (whales) in the inshore and offshore zones. Units: 0.01 milli-Mole of nitrogen per m<sup>2</sup> (0.01 mMN.m<sup>-2</sup>) is approximately equivalent to 5 kg of live weight per km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Predators" && input$Predators_Tab == "predMigFish"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of migratory fish in the inshore and offshore zones. Migratory fish spend only part of the year in the model region. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(is.null(input$outputEcoType) || is.null(input$Corpse_Discard_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of dead animals  (from plankton to whales) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of material below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Corpse_Discard" && input$Corpse_Discard_Tab == "corpses"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of dead animals  (from plankton to whales) in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of material below 1 km<sup>2</sup> of sea surface"))}
                               else if(input$outputEcoType == "Corpse_Discard" && input$Corpse_Discard_Tab == "discard"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of dead fish discarded by fishing vessels in the inshore and offshore zones. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of material below 1 km<sup>2</sup> of sea surface"))}
                               else if(is.null(input$outputEcoType) || is.null(input$Macrophyte_Tab)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of macrophytes (large seaweeds) in the inshore zone. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup>  of sea surface"))}
                               else if(input$outputEcoType == "Macrophyte" && input$Macrophyte_Tab == "inshoreMac"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of macrophytes (large seaweeds) in the inshore zone. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup>  of sea surface"))}
                               else if(input$outputEcoType == "Macrophyte" && input$Macrophyte_Tab == "inshoreMacDebris"){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in the quantity of macrophyte debris (dead, broken-off pieces off seaweed) in the inshore zone. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> (1 mMN.m<sup>-2</sup>) is approximately equivalent to 500 kg of live weight below 1 km<sup>2</sup> of sea surface"))}
                               else if(is.null(input$outputEcoType)){
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of organic detritus and associated bacteria suspended in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is approximately equivalent to 0.16 grams of material per m<sup>3</sup>"))}
                               else {
                                 fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Changes over the year in concentrations of organic detritus and associated bacteria suspended in the surface-offshore, surface-inshore and the deep-offshore layers/zones. Units: 1 milli-Mole of nitrogen per m<sup>3</sup> (1 mMN.m<sup>-3</sup>) is approximately equivalent to 0.16 grams of material per m<sup>3</sup>"))}
                               ) 

  output$textCatchGuild <- renderUI({
                                    if(is.null(input$outputCatchType)){
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of planktivorous (plankton-eating) fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. Examples of these species would be herring, sardines. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Planktivorous fish") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of planktivorous (plankton-eating) fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. Examples of these species would be herring, sardines. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Quota limited Demersal fish") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of 'quota limited' demersal (benthos and fish-eating) fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. 'Quota-limited' means that these are species for which landings are governed by quota restrictions, e.g. cod, haddock. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Non quota demersal fish") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of 'non-quota ' demersal (benthos and fish-eating) fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. 'Non-quota' means that these are by-catch species for which there are no landing quota restrictions, e.g. wrasse or long-rough dab. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Migratory fish") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of migratory fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. An example of these species would be mackerel. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Susp/deposit feeding benthos") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of suspension and deposit feeding benthos (seabed-living invertebrate animals) in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. An example of these species would be scallops. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region."))}
                                    else if(input$outputCatchType == "Carn/scavebge feeding benthos") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of carnivorous and scavenge feeding benthos (seabed-living invertebrate animals) in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. Examples of these species would be prawns, crabs and lobsters. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Pelagic invertebrates") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of pelagic invertebrates (carnivorous zooplankton in the model) in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. Examples of these species would be squids. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Birds") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of seabirds in the inshore and offshore zones by each fishing gear type. Other then in specific cases seabirds are taken unintentionally as a by-catch and discarded. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Pinnipeds") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of pinnipeds (seals)  in the inshore and offshore zones by each fishing gear type. Other then in specific hunting cases pinnipeds are taken unintentionally as a by-catch and discarded. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Cetaceans") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of cetaceans (whales)  in the inshore and offshore zones by each fishing gear type. Other then in specific hunting cases cetaceans are taken unintentionally as a by-catch and discarded. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "Macrophytes") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of macrophytes (seaweeds) in the inshore zone by each fishing gear type, broken down by landings and discards. Examples of these species would be kelps. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
                                    else if(input$outputCatchType == "All guilds combined") {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Combined annual catches of all guilds in the inshore  and offshore zones by each fishing gear type, broken down by landings and discards. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region."))}
                                    else {
                                      fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of planktivorous (plankton-eating) fish in the inshore and offshore zones by each fishing gear type, broken down by landings and discards. Examples of these species would be herring, sardines. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-3</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}
  }) 
  
  output$textEnvironmental <- renderUI({
                                       # if(is.null(input$edriverType)){
                                       if(is.null(input$edriverType)){
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged intensity of sunlight at the sea surface. Units: Einsteins per m<sup>2</sup> per day. An Einstein is a mole of photons (6.022×10<sup>23</sup> photons)"))}
                                       else if(input$edriverType == "Surface irradiance") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged intensity of sunlight at the sea surface. Units: Einsteins per m<sup>2</sup> per day. An Einstein is a mole of photons (6.022×10<sup>23</sup> photons)"))}
                                       else if(input$edriverType == "Susp.partic. matter") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of suspended particulate matter (silt) in the water of the offshore and inshore zones This determines how much sunlight penetrates below the sea surface. Units: grams per m<sup>3</sup>"))}
                                       else if(input$edriverType == "Temperature") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged temperature in each zone and layer of the model  - upper (surface) layer offshore; lower (deep ) layer offshore; inshore"))}
                                       else if(input$edriverType == "Diffusivity gradient") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged vertical diffusivity gradient in the offshore zone (m per day). Diffusivity is a measure of the intensity of mixing between the surface and deep water layers"))}
                                       else if(input$edriverType == "External Inflow") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged current inflow rate of water to each layer and zone from the ocean outside the model. Units: m<sup>3</sup> of inflow per m<sup>2</sup> of sea surface area."))}
                                       else if(input$edriverType == "River discharge") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged volume of water flowing into the model region from rivers. Units: m<sup>3</sup> of inflow per m<sup>2</sup> of sea surface area."))}
                                       else if(input$edriverType == "Wave height") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged wave height in the inshore zone of the model. Waveheight determines the fragmentation rate of macrophytes in the model. Units: metres"))}
                                       else if(input$edriverType == "Sediment disturbance") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged proportion of the seabed sediment area disturbed naturally by waves and currents in the inshore and offshore zones. Units: proportion of area per day."))}
                                       else if(input$edriverType == "Boundary nitrate") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of nitrate in the water flowing into each layer and zone of the model from the ocean outside. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "Boundary ammonia") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of ammonia in the water flowing into each layer and zone of the model from the ocean outside. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "Boundary phytoplankton") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of phytoplankton in the water flowing into each layer and zone of the model from the ocean outside. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "Boundary detritus") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of dead organic matter (detritus) in the water flowing into each layer and zone of the model from the ocean outside. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "River nitrate") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of nitrate in the river waters flowing into the model region. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "River ammonia") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged concentration of ammonia in the river waters flowing into the model region. Units: milli-Moles of nitrogen per m<sup>3</sup>"))}
                                       else if(input$edriverType == "Atmospheric nitrate") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged deposition rate of nitrate onto the sea surface from the atmosphere in rain and dust. Units: milli-Moles of nitrogen per m<sup>2</sup> per day."))}
                                       else if(input$edriverType == "Atmospheric ammonia") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged deposition rate of ammonia onto the sea surface from the atmosphere in rain and dust. Units: milli-Moles of nitrogen per m<sup>2</sup> per day."))}
                                       else {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Monthly averaged intensity of sunlight at the sea surface. Units: Einsteins per m<sup>2</sup> per day. An Einstein is a mole of photons (6.022×10<sup>23</sup> photons)"))}
  })
  

  output$textFishery <- renderUI({
                                       if(is.null(input$fdriverType)){
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \"Annual activity rate of each fishing gear type over each seabed sediment habitat. Darker purple indicates more activity. Units: log seconds of activity per m<sup>2</sup> per day."))}
                                       else if(input$fdriverType == "Activity") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual activity rate of each fishing gear type over each seabed sediment habitat. Darker purple indicates more activity. Units: log seconds of activity per m<sup>2</sup> per day."))}
                                       else if(input$fdriverType == "Abrasion") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Distribution of seabed abrasion rates by each fishing gear. Darker purple indicates more intense abrasion. Units: log proportion sediment area abraded per day."))}
                                       else if(input$fdriverType == "HarvestR") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Selectivity pattern of each fishing gear, mapped in terms of the annual averaged harvest ratio on each guild of animals by each gear. Darker purple indicates higher harvest  ratio. Harvest ratio is the proportion of biomass captured per day, and is equivalent to the fishing mortality rate. Units: log proportion caught per day"))}
                                       else if(input$fdriverType == "Discards") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Discard rate of each guild of organisms caught by each fishing gear. Discard rate of the proportion of catch which is returned to the sea. In the model these animals are presumed to be dead. Darker purple indicates higher discard rate. Units: log proportion of catch discarded."))}
                                       else if(input$fdriverType == "Offal") {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Offal production rate of each guild of organisms caught by each fishing gear. Offal are the discarded viscera of fish which are processed at sea. Darker purple indicates higher offal production rate. Units: log proportion of catch discarded viscera due to processing."))}
                                        else {
                                         fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual activity rate of each fishing gear type over each seabed sediment habitat. Darker purple indicates more activity. Units: log seconds of activity per m<sup>2</sup> per day."))}
  }) 
  
  
  output$textCatchGear <- renderUI({fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Annual catches of each guild by the selected gear type, broken down by landings and discards. Units: 1 milli-Mole of nitrogen per m<sup>2</sup> per year (1 mMN.m<sup>-2</sup>.y<sup>-1</sup>) is approximately equivalent to 500 kg of live weight per km<sup>2</sup> of whole model region"))}) 
  
  output$textCatchPerHabitatGear <- renderUI({fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt \">On this page you can change the spatial distribution of activity by each fishing gear."),
                                                       HTML("<p style = \"font-family: 'calibri'; font-si16pt \">First select a gear from the drop-down list above."),
                                                       HTML("<p style = \"font-family: 'calibri'; font-si16pt \">The upper boxes show the distribution (%) of activity between inshore and offshore. Type a new value into the Inshore box – the offshore box will automatically adjust to ensure 100% overall."),
                                                       HTML("<p style = \"font-family: 'calibri'; font-si16pt \">The lower boxes show the distribution of the inshore (left) and offshore (right) activity across seabed habitats. When a new value is entered, the bottom box will adjust automatically to ensure 100% overall. A seabed habitat will be absent from the interface if it doesn’t exist in a particular region."),
                                                       HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Press ‘Reset’ to restore the original values."),
                                                       HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Note that this page does not alter the overall activity level of any gear, only its spatial distribution. To change the overall activity use the ‘Fishing Activity’ page from the ‘Setup Scenario’ menu."))}) 
  
  
  output$textRunBaselineModel <- renderUI({fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Each model is configured so that it outputs annual cycles of daily values of everything in the system,  averaged over the selected period of years. We do this by running the model over and over with repeating annual cycles of input data until the outputs are stable from one year to the next. We call this a \"steady state\" which represents the \"climatology\" of the system, and should match the average conditions in the sea for any given day of the year.")))})
                                                    #HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Running the baseline will take a few seconds on our computer server. The results are saved in memory for you to explore using our graph drawing tools, or you can download the output as .csv files to analyse yourself if you wish (e.g. read them into Excel)."),
                                                    #HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Progress to the “Setup Scenario” tab to configure a new set of model inputs and then rerun the model and compare the results with your baseline run")))}) 
  
  
  
  output$textCompAAM <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runScenario == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the scenario before viewing comparison plots",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">‘Tornado’ bar-plot of the differences in annual averaged quantities of model components in the whole model region between your scenario model run and the baseline."),
                                         HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">The upper plot shows ecology guilds inhabiting the water column, the lower plot in and on the seabed"),
                                         HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Green bars to the right indicate that the (bio)mass was higher in the scenario run than the baseline, and vice-versa for red bars to the left."),
                                         HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">You can adjust the axis limits for the tornado plot using the slider bar below. The plot will default to ±40% but you may want to alter these to accommodate the output from your scenario or to standardize the graphs for different scenarios, To access the numeric values for each bar in the graphs, use the “Download data” button."))})
  
  
  output$textCompFishCatch <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runScenario == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the scenario before viewing comparison plots",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">‘Tornado’ bar-plot of the differences in annual fishery landings and discards in the whole model region between your scenario model run and the baseline."),
                                          HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">The upper plot shows fishery landings (the quantity of catch which is brought ashore), the lower plot shows the discards (unwanted catch which is returned to the sea and assumed to be dead in the model)."),
                                          HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Green and black bars to the right indicate that the landings or discards were higher in the scenario run than the baseline, and vice-versa for red and grey bars to the left."),
                                          HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">You can adjust the axis limits for the tornado plot using the slider bar below. The plot will default to ±40% but you may want to alter these to accommodate the output from your scenario or to standardize the graphs for different scenarios, To access the numeric values for each bar in the graphs, use the “Download data” button."))})
  
  output$textRunBaselineFiles <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runBaseline == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    
    fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">The following .csv files are available to download : WHOLEDOMAIN_model_anav_biomass-baseline, OFFSHORE_model_anav_biomass-baseline, INSHORE_model_anav_biomass-baseline, OFFSHORE_landingcomposition_by_gear-baseline, OFFSHORE_discardcomposition_by_gear-baseline, INSHORE_landingcomposition_by_gear-baseline, INSHORE_discardcomposition_by_gear-baseline where baseline is an identifier for your baseline data."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Data are scaled to represent the quantity per m<sup>2</sup> of the whole region for reach model."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"anav_biomass files\" contain annual average mass of each component of the model in the named zone. The files includes data on layer and zone thickness and area so that the mass data can be converted to concentrations."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"landingcomposition\" and \"discardcomposition\" files contain a matrix of landed and discarded quantity by living guild and gear type. Units: (milli-Moles of nitrogen per m<sup>2</sup> of whole model region per year).")))}) 

  output$textRunBaselinePlots <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runBaseline == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring plots",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">All of the available plots of input data and outputs from the baseline model run have been saved as .png files and gathered together here in a zip-file for you to download. The download contains a README file to help you locate the plot you need.")))}) 
  
  output$textRunScenarioFiles <- renderUI({
    
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runScenario == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the scenario before exploring scenario output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    

    fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">The following .csv files are available to download : WHOLEDOMAIN_model_anav_biomass-scenario, OFFSHORE_model_anav_biomass-scenario, INSHORE_model_anav_biomass-scenario, OFFSHORE_landingcomposition_by_gear-scenario, OFFSHORE_discardcomposition_by_gear-scenario, INSHORE_landingcomposition_by_gear-scenario, INSHORE_discardcomposition_by_gear-scenario where scenario is an identifier for your scenario data."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Data are scaled to represent the quantity per m<sup>2</sup> of the whole region for reach model."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"anav_biomass files\" contain annual average mass of each component of the model in the named zone. The files includes data on layer and zone thickness and area so that the mass data can be converted to concentrations."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"landingcomposition\" and \"discardcomposition\" files contain a matrix of landed and discarded quantity by living guild and gear type. Units: (milli-Moles of nitrogen per m<sup>2</sup> of whole model region per year).")))}) 
  
  output$textRunScenarioModel <- renderUI({
    
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please also choose a region and time period in 'Setup Model' before running a scenario",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    
    
    fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">As with the baseline model, your scenario model will be run over and over with repeating annual cycles of your new input data. The number of years required to reach a new “steady state”, where each successive year of output is the same as the previous, will depend on the extent of changes you made to the inputs. Typically, 40 years will be sufficient to reach a new steady state for an extensive set of changes to the inputs."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Use the slider to select different numbers of years and see how your changes to the inputs impact the ecosystem over time. For a “quick look” run for the default setting of 5 years. Otherwise use the slider to select 40 years to be certain of reach the new steady state."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Expect the model run-time to be about 4 seconds per year."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">The results are saved in memory for you to compare with your baseline model run in the “Scenario Results” tab, or you can download the output as .csv files to analyse yourself if you wish (e.g. read them into Excel). ")))}) 
  
  output$textDiscardGuild <- renderUI({fluidRow(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to change the discard rate of each guild by each fishing gear."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Baseline discard rates are shown in blue boxes. Discard rate can be varied between 0 and 1."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Discard rate is the proportion of catch which is returned to the sea. The model, discards are assumed to be dead."))}) 
  
  output$model_map <- renderUI({
    # current chosen model on dropdown lost
    switch(
      input$selectedlocation,
    "North_Sea" = div(img(src = "North.png", width = '80%') , style="text-align: center;"),
    "Celtic_Sea" = div(img(src = "Celtic.png", width = '80%') , style="text-align: center;")
    )
  })
  
  output$uiCatchGuild <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runBaseline == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    switch(
      input$outputCatchType,
      "Planktivorous fish" = fluidRow(plotOutput("ecoPlot_catch_guild_1")),
      "Quota limited Demersal fish" = fluidRow(plotOutput("ecoPlot_catch_guild_2")),
      "Non quota demersal fish" = fluidRow(plotOutput("ecoPlot_catch_guild_3")),
      "Migratory fish" = fluidRow(plotOutput("ecoPlot_catch_guild_4")),
      "Susp/deposit feeding benthos" = fluidRow(plotOutput("ecoPlot_catch_guild_5")),
      "Carn/scavebge feeding benthos" = fluidRow(plotOutput("ecoPlot_catch_guild_6")),
      "Pelagic invertebrates" = fluidRow(plotOutput("ecoPlot_catch_guild_7")),
      "Birds" = fluidRow(plotOutput("ecoPlot_catch_guild_8")),
      "Pinnipeds" = fluidRow(plotOutput("ecoPlot_catch_guild_9")),
      "Cetaceans" = fluidRow(plotOutput("ecoPlot_catch_guild_10")),
      "Macrophytes" = fluidRow(plotOutput("ecoPlot_catch_guild_11")),
      "All guilds combined" = fluidRow(plotOutput("ecoPlot_catch_guild_12"))
    )
  })
  
 # output$uiCatchGear <- renderUI({
  
  catchGear <- reactive({
    gearType <- input$outputGearType
    if (is.null(input$outputGearType)) gearType <- c("Pelagic_Trawl+Seine")
    switch(
      gearType,
      "Pelagic_Trawl+Seine" = fluidRow(plotOutput("ecoPlot_catch_gear_1")),
      "Sandeel+sprat_trawl(Otter30-70mm+TR3)" = fluidRow(plotOutput("ecoPlot_catch_gear_2")),
      "Otter30-70mm+TR3(sandeel+sprat)" = fluidRow(plotOutput("ecoPlot_catch_gear_2")),
      "Longline_mackerel" = fluidRow(plotOutput("ecoPlot_catch_gear_3")),
      "Beam_Trawl_BT1+BT2" = fluidRow(plotOutput("ecoPlot_catch_gear_4")),
      "Demersal_Seine" = fluidRow(plotOutput("ecoPlot_catch_gear_5")),
      "Demersal_Otter_Trawl_TR1" = fluidRow(plotOutput("ecoPlot_catch_gear_6")),
      "Gill_Nets+Longline_demersal" = fluidRow(plotOutput("ecoPlot_catch_gear_7")),
      "Beam_Trawl_shrimp" = fluidRow(plotOutput("ecoPlot_catch_gear_8")),
      "Nephrops_Trawl_TR2" = fluidRow(plotOutput("ecoPlot_catch_gear_9")),
      "Nephrops_Trawl_TR3" = fluidRow(plotOutput("ecoPlot_catch_gear_9")),
      "Creels" = fluidRow(plotOutput("ecoPlot_catch_gear_10")),
      "Mollusc_Dredge" = fluidRow(plotOutput("ecoPlot_catch_gear_11")),
      "Whaler" = fluidRow(plotOutput("ecoPlot_catch_gear_12_Whaler")),
      "KelpHarvester" = fluidRow(plotOutput("ecoPlot_catch_gear_12_KelpHarvester")),
      fluidRow(plotOutput("ecoPlot_catch_gear_1")) #Default
    )
  })
  
  output$uiCatchGear <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runBaseline == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    catchGear()
    }) 

  output$UiEdriver <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring input data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    output$edriver_plot_1 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Surface irradiance"
        )
      })
    
    output$edriver_plot_2 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Susp.partic. matter"
        )
      })
    
    output$edriver_plot_3 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Temperature"
        )
      })
    
    output$edriver_plot_4 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Diffusivity gradient"
        )
      })
    
    output$edriver_plot_5 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "External Inflow"
        )
      })
    
    output$edriver_plot_6 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "River discharge"
        )
      })
    
    output$edriver_plot_7 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Wave height"
        )
      })
    
    output$edriver_plot_8 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Sediment disturbance"
        )
      })
    
    output$edriver_plot_9 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Boundary nitrate"
        )
      })
    
    output$edriver_plot_10 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Boundary ammonia"
        )
      })
    
    output$edriver_plot_11 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Boundary phytoplankton"
        )
      })
    
    output$edriver_plot_12 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Boundary detritus"
        )
      })
    
    output$edriver_plot_13 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "River nitrate"
        )
      })
    
    output$edriver_plot_14 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "River ammonia"
        )
      })
    
    output$edriver_plot_15 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Atmospheric nitrate"
        )
      })
    
    output$edriver_plot_16 <- 
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        createEDriversPlots(
          model,
          selection = "Atmospheric ammonia"
        )
      })
    switch(
      input$edriverType,
      "Surface irradiance" = fluidRow(plotOutput("edriver_plot_1")),
      "Susp.partic. matter" = fluidRow(plotOutput("edriver_plot_2")),
      "Temperature" = fluidRow(plotOutput("edriver_plot_3")),
      "Diffusivity gradient" = fluidRow(plotOutput("edriver_plot_4")),
      "External Inflow" = fluidRow(plotOutput("edriver_plot_5")),
      "River discharge" = fluidRow(plotOutput("edriver_plot_6")),
      "Wave height" = fluidRow(plotOutput("edriver_plot_7")),
      "Sediment disturbance" = fluidRow(plotOutput("edriver_plot_8")),
      "Boundary nitrate" = fluidRow(plotOutput("edriver_plot_9")),
      "Boundary ammonia" = fluidRow(plotOutput("edriver_plot_10")),
      "Boundary phytoplankton" = fluidRow(plotOutput("edriver_plot_11")),
      "Boundary detritus" = fluidRow(plotOutput("edriver_plot_12")),
      "River nitrate" = fluidRow(plotOutput("edriver_plot_13")),
      "River ammonia" = fluidRow(plotOutput("edriver_plot_14")),
      "Atmospheric nitrate" = fluidRow(plotOutput("edriver_plot_15")),
      "Atmospheric ammonia" = fluidRow(plotOutput("edriver_plot_16"))
    )
  })
  output$UiFdriver <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring input data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    output$basePlot_fdriver_activity <-
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        e2e_plot_fdrivers(model, selection = "ACTIVITY")
      })

    output$basePlot_fdriver_abrasion <-
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        e2e_plot_fdrivers(model, selection = "ABRASION")
      })

    output$basePlot_fdriver_harvestr <-
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        e2e_plot_fdrivers(model, selection = "HARVESTR")
      })

    output$basePlot_fdriver_discards <-
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        e2e_plot_fdrivers(model, selection = "DISCARDS")
      })

    output$basePlot_fdriver_offal <-
      renderPlot({
        model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
        e2e_plot_fdrivers(model, selection = "OFFAL")
      })
    switch(
      input$fdriverType,
      "Activity" = fluidRow(plotOutput("basePlot_fdriver_activity")),
      "Abrasion" = fluidRow(plotOutput("basePlot_fdriver_abrasion")),
      "HarvestR" = fluidRow(plotOutput("basePlot_fdriver_harvestr")),
      "Discards" = fluidRow(plotOutput("basePlot_fdriver_discards")),
      "Offal" = fluidRow(plotOutput("basePlot_fdriver_offal"))
    )
  })

  output$uiEco <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runBaseline == 0){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the model before exploring output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    switch(
      input$outputEcoType,
      "Nutrient_Phytoplankton" =
        fluidRow(tabsetPanel(type = "pills",
                    tabPanel("Detritus", value = "nut_phyt_Detritus", plotOutput("ecoPlot_nut_phyt_Detritus")),
                    tabPanel("Ammonia", value = "nut_phyt_Ammonia", plotOutput("ecoPlot_nut_phyt_Ammonia")),
                    tabPanel("Nitrate", value = "nut_phyt_Nitrate", plotOutput("ecoPlot_nut_phyt_Nitrate")),
                    tabPanel("Phytoplankton", value = "nut_phyt_Phytoplankton", plotOutput("ecoPlot_nut_phyt_Phytoplankton")),
                    id = "Nutrient_Phytoplankton_Tab"
        )),
      "Sediment" =
        fluidRow(tabsetPanel(type = "pills",
                    tabPanel("Inshore ammonia", value = "sedInAmmonia", plotOutput("ecoPlot_sediment_InAmm")),
                    tabPanel("Offshore ammonia", value = "sedOffAmmonia", plotOutput("ecoPlot_sediment_OffAmm")),
                    tabPanel("Inshore nitrate", value = "sedInNitrate", plotOutput("ecoPlot_sediment_InNit")),
                    tabPanel("Offshore nitrate", value = "sedOffNitrate", plotOutput("ecoPlot_sediment_OffNit")),
                    tabPanel("Inshore detritus", value = "sedInDetrius", plotOutput("ecoPlot_sediment_InDet")),
                    tabPanel("Offshore detritus", value = "sedOffDetrius", plotOutput("ecoPlot_sediment_OffDet")),
                    tabPanel("Inshore corpses", value = "sedInCorpses", plotOutput("ecoPlot_sediment_InCorp")),
                    tabPanel("Offshore corpses", value = "sedOffCorpses", plotOutput("ecoPlot_sediment_OffCorp")),
                    id = "Sediment_Tab"
        )),
      "Zooplankton" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Omnivorous zooplankton", value = "zooOmni", plotOutput("ecoPlot_zooplankton_OmnZoo")),
                             tabPanel("Carnivorous zooplankton", value = "zooCarn", plotOutput("ecoPlot_zooplankton_CarnZoo")),
                             id = "Zooplankton_Tab"
        )),
      "Fish" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Planktivorous fish", value = "plankFish", plotOutput("ecoPlot_fish_PlankFish")),
                             tabPanel("Planktivorous fish larvae", value = "plankFishLarv", plotOutput("ecoPlot_fish_PlankFishLarv")),
                             tabPanel("Demersal fish", value = "demFish", plotOutput("ecoPlot_fish_DemFish")),
                             tabPanel("Demersal fish larvae", value = "demFishLarv", plotOutput("ecoPlot_fish_DemFishLarv")),
                             id = "Fish_Tab"
        )),
      "Benthos" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Benthos susp/dep feeders", value = "benSusFeed", plotOutput("ecoPlot_benthos_BenSusFeed")),
                             tabPanel("Benthos susp/dep feeders larvae", value = "benSusLarvFeed", plotOutput("ecoPlot_benthos_BenSusFeedLarv")),
                             tabPanel("Benthos carn/scav feeders", value = "benCarnFeed", plotOutput("ecoPlot_benthos_BenCarnFeed")),
                             tabPanel("Benthos carn/scav feeders larvae", value = "benCarnLarvFeed", plotOutput("ecoPlot_benthos_BenCarnFeedLarv")),
                             id = "Benthos_Tab"
        )),
      "Predators" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Birds", value = "predBirds", plotOutput("ecoPlot_predator_birds")),
                             tabPanel("Pinnipeds", value = "predPinnipeds", plotOutput("ecoPlot_predator_pinnipeds")),
                             tabPanel("Cetaceans", value = "predCetaceans", plotOutput("ecoPlot_predator_cetaceans")),
                             tabPanel("Migratory fish", value = "predMigFish", plotOutput("ecoPlot_predator_migFish")),
                             id = "Predators_Tab"
        )),
      "Corpse_Discard" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Corpses", value = "corpses", plotOutput("ecoPlot_corpdisc_corpses")),
                             tabPanel("Discards", value = "discards", plotOutput("ecoPlot_corpdisc_discard")),
                             id = "Corpse_Discard_Tab"
        )),
      "Macrophyte" =
        fluidRow(tabsetPanel(type = "pills",
                             tabPanel("Inshore macrophytes", value = "inshoreMac", plotOutput("ecoPlot_macrophyte_inshore")),
                             tabPanel("Inshore macrophyte debris", value = "inshoreMacDebris", plotOutput("ecoPlot_macrophyte_inshoreDeb")),
                             id = "Macrophyte_Tab"
        ))
    )
  })
  
  output$uiGearHabDist <- renderUI({
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please choose a region and time period before changing gear activity distribution per habitat",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    createGearHabDistUI(model,input,jsCode)
  })
  
  output$ui <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    createDiscardPerGearUI(model,input)
  })
  
  #Pelagic discard reset
  
  observeEvent(input$pelagicTrawlDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[9]
    )
  })
  
  observeEvent(input$creelsDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[11]
    )
  })
  
  observeEvent(input$whalerDiscard_pel_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_pel",
      value = model$data$fleet.model$gear_group_discard$pelagic[12]
    )
  })
  
  #Demersal discard reset
  
  observeEvent(input$pelagicTrawlDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[9]
    )
  })
  
  observeEvent(input$creelsDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[11]
    )
  })
  
  observeEvent(input$whalerDiscard_dem_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_dem",
      value = model$data$fleet.model$gear_group_discard$demersal[12]
    )
  })
  
  #Migratory discard reset
  
  observeEvent(input$pelagicTrawlDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[9]
    )
  })
  
  observeEvent(input$creelsDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[11]
    )
  })
  
  observeEvent(input$whalerDiscard_mig_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_mig",
      value = model$data$fleet.model$gear_group_discard$migratory[12]
    )
  })
  
  #Filtben discard reset
  
  observeEvent(input$pelagicTrawlDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[9]
    )
  })
  
  observeEvent(input$creelsDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_fb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_fb",
      value = model$data$fleet.model$gear_group_discard$filtben[11]
    )
  })
  
  #Carnben discard reset
  
  observeEvent(input$pelagicTrawlDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[9]
    )
  })
  
  observeEvent(input$creelsDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[11]
    )
  })
  
  observeEvent(input$whalerDiscard_cb_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_cb",
      value = model$data$fleet.model$gear_group_discard$carnben[12]
    )
  })
  
  #Carnzoo discard reset
  
  observeEvent(input$pelagicTrawlDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[9]
    )
  })
  
  observeEvent(input$creelsDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[11]
    )
  })
  
  observeEvent(input$whalerDiscard_cz_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_cz",
      value = model$data$fleet.model$gear_group_discard$carnzoo[12]
    )
  })
  
  #Bird discard reset
  
  observeEvent(input$pelagicTrawlDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[9]
    )
  })
  
  observeEvent(input$creelsDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(session,
                      "creelsDiscard_b",
                      value = model$data$fleet.model$gear_group_discard$bird[10])
  })
  
  observeEvent(input$molluscDredgeDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_b",
      value = model$data$fleet.model$gear_group_discard$bird[11]
    )
  })
  
  observeEvent(input$whalerDiscard_b_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(session,
                      "whalerDiscard_b",
                      value = model$data$fleet.model$gear_group_discard$bird[12])
  })
  
  #Seal discard reset
  
  observeEvent(input$pelagicTrawlDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[9]
    )
  })
  
  observeEvent(input$creelsDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(session,
                      "creelsDiscard_s",
                      value = model$data$fleet.model$gear_group_discard$seal[10])
  })
  
  observeEvent(input$molluscDredgeDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_s",
      value = model$data$fleet.model$gear_group_discard$seal[11]
    )
  })
  
  observeEvent(input$whalerDiscard_s_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(session,
                      "whalerDiscard_s",
                      value = model$data$fleet.model$gear_group_discard$seal[12])
  })
  
  #Ceta discard reset
  
  observeEvent(input$pelagicTrawlDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[9]
    )
  })
  
  observeEvent(input$creelsDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[11]
    )
  })
  
  observeEvent(input$whalerDiscard_ceta_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_ceta",
      value = model$data$fleet.model$gear_group_discard$ceta[12]
    )
  })
  
  #Kelp discard reset
  
  observeEvent(input$pelagicTrawlDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "pelagicTrawlDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[1]
    )
  })
  
  observeEvent(input$sanSpratTrawlDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "sanSpratTrawlDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[2]
    )
  })
  
  observeEvent(input$llMackerelDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "llMackerelDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[3]
    )
  })
  
  observeEvent(input$beamTrawlDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[4]
    )
  })
  
  observeEvent(input$demersalSeineDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalSeineDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[5]
    )
  })
  
  observeEvent(input$demersalOtterTrawlDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "demersalOtterTrawlDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[6]
    )
  })
  
  observeEvent(input$gillLongDemersalDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "gillLongDemersalDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[7]
    )
  })
  
  observeEvent(input$beamTrawlShrimpDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "beamTrawlShrimpDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[8]
    )
  })
  
  observeEvent(input$nephropsTrawlDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "nephropsTrawlDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[9]
    )
  })
  
  observeEvent(input$creelsDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "creelsDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[10]
    )
  })
  
  observeEvent(input$molluscDredgeDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "molluscDredgeDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[11]
    )
  })
  
  observeEvent(input$whalerDiscard_kelp_reset, {
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
    updateSliderInput(
      session,
      "whalerDiscard_kelp",
      value = model$data$fleet.model$gear_group_discard$kelp[12]
    )
  })
  
  observeEvent(input$runYield, {
    showModal(
      modalDialog(
        "Please wait whilst yield run finishes- this can take from 8 to 10 minutes",
        footer = NULL
      )
    )
    # Run yield
    location <- input$selectedlocation
    variant <- input$selectedVariant
    
    #a <- future({
    model <<- e2e_read(location,variant)
    pf_yield_data <- e2e_run_ycurve(
      model,
      selection="PLANKTIV",
      nyears = 50,
      HRvector = c(0, 0.5, 1, 1.5, 2, 2.5, 3),
      HRfixed = 1,
      csv.output = FALSE
    )

    removeModal()
    
    output$yield_plot <-
      renderPlot({
        e2e_plot_ycurve(model, selection="PLANKTIV", results=pf_yield_data,
                        title="Planktivorous yield with baseline demersal fishing")
      })
    
    #sendmail("thomas.doherty@strath.ac.uk", "thomas.doherty@strath.ac.uk", "Test", "Test")
   # })
  })
  
  observeEvent(input$runBaseline, {
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please choose a region and time period before running model",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    showModal(
      modalDialog(
        "Please wait whilst model runs baseline. Once completed you can explore outputs from the \"Model Results\" tab menu",
        footer = NULL
      )
    )
    # Run baseline
    model <<-
      e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #View(model)
    uuid <- UUIDgenerate()
    
    results_baseline <<-
      e2e_run(model, nyears = input$year, csv.output = TRUE)
    removeModal()
    resultDirBaseline <- toString(model$setup$resultsdir)
    output$downloadData_baseline1 <- downloadHandler(
      filename = function() {
        paste0("baseline_", uuid, ".zip")
      },
      content = function(file) {
        zip(zipfile=file, files=resultDirBaseline)
      },
      contentType = "application/zip"
    )
    if (!is.null(model)) {
      enable("downloadData_baseline1")
      runjs("$('#dwnbutton_b').removeAttr('title');")
    } else{
      disable("downloadData_baseline1")
      runjs("$('#dwnbutton_b').attr('title', 'Data not available');")
    }
    
    output$downloadData_baselinePlots <- downloadHandler(
      filename = function() {
        paste0("baselinePlots_", uuid, ".zip")
      },
      content = function(file) {
        plotsPath = file.path(model$setup$model.path,"Plots")
        zip(zipfile=file, files=plotsPath)
      },
      contentType = "application/zip"
    )
    if (!is.null(model)) {
      enable("downloadData_baselinePlots")
      runjs("$('#dwnbutton_bp').removeAttr('title');")
    } else{
      disable("downloadData_baselinePlots")
      runjs("$('#dwnbutton_bp').attr('title', 'Data not available');")
    }
    
    output$ecoPlot_nut_phyt_Detritus <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "NUT_PHYT",
          subSelection = "Detritus"
        )
      })
    output$ecoPlot_nut_phyt_Ammonia <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "NUT_PHYT",
          subSelection = "Ammonia"
        )
      })
    output$ecoPlot_nut_phyt_Nitrate <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "NUT_PHYT",
          subSelection = "Nitrate"
        )
      })
    output$ecoPlot_nut_phyt_Phytoplankton <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "NUT_PHYT",
          subSelection = "Phytoplankton"
        )
      })
    
    output$ecoPlot_sediment_InAmm <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Inshore ammonia"
        )
      })
    
    output$ecoPlot_sediment_OffAmm <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Offshore ammonia"
        )
      })
    
    output$ecoPlot_sediment_InNit <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Inshore nitrate"
        )
      })
    
    output$ecoPlot_sediment_OffNit <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Offshore nitrate"
        )
      })
    
    output$ecoPlot_sediment_InDet <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Inshore detritus"
        )
      })
    
    output$ecoPlot_sediment_OffDet <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Offshore detritus"
        )
      })
    
    output$ecoPlot_sediment_InCorp <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Inshore corpses"
        )
      })
    
    output$ecoPlot_sediment_OffCorp <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "SEDIMENT",
          subSelection = "Offshore corpses"
        )
      })
    
    output$ecoPlot_zooplankton_OmnZoo <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "ZOOPLANKTON",
          subSelection = "Omnivorous zooplankton"
        )
      })
    
    output$ecoPlot_zooplankton_CarnZoo <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "ZOOPLANKTON",
          subSelection = "Carnivorous zooplankton"
        )
      })
    
    output$ecoPlot_fish_PlankFish <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "FISH",
          subSelection = "Planktivorous fish"
        )
      })
    
    output$ecoPlot_fish_PlankFishLarv <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "FISH",
          subSelection = "Planktivorous fish larvae"
        )
      })
    
    output$ecoPlot_fish_DemFish <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "FISH",
          subSelection = "Demersal fish"
        )
      })
    
    output$ecoPlot_fish_DemFishLarv <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "FISH",
          subSelection = "Demersal fish larvae"
        )
      })
    
    output$ecoPlot_benthos_BenSusFeed <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "BENTHOS",
          subSelection = "Benthos susp/dep feeders"
        )
      })
    
    output$ecoPlot_benthos_BenSusFeedLarv <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "BENTHOS",
          subSelection = "Benthos susp/dep feeders larvae"
        )
      })
    
    output$ecoPlot_benthos_BenCarnFeed <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "BENTHOS",
          subSelection = "Benthos carn/scav feeders"
        )
      })
    
    output$ecoPlot_benthos_BenCarnFeed <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "BENTHOS",
          subSelection = "Benthos carn/scav feeders"
        )
      })
    
    output$ecoPlot_benthos_BenCarnFeedLarv <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "BENTHOS",
          subSelection = "Benthos carn/scav feeders larvae"
        )
      })
    
    output$ecoPlot_predator_birds <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "PREDATORS",
          subSelection = "Birds"
        )
      })
    
    output$ecoPlot_predator_pinnipeds <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "PREDATORS",
          subSelection = "Pinnipeds"
        )
      })
    
    output$ecoPlot_predator_cetaceans <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "PREDATORS",
          subSelection = "Cetaceans"
        )
      })
    
    output$ecoPlot_predator_migFish <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "PREDATORS",
          subSelection = "Migratory fish"
        )
      })
    
    output$ecoPlot_corpdisc_corpses <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "CORP_DISC",
          subSelection = "Corpses"
        )
      })
    
    output$ecoPlot_corpdisc_discard <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "CORP_DISC",
          subSelection = "Discards"
        )
      })
    
    output$ecoPlot_macrophyte_inshore <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "MACROPHYTE",
          subSelection = "Inshore macrophytes"
        )
      })
    
    output$ecoPlot_macrophyte_inshoreDeb <-
      renderPlot({
        createEcoplots(
          model,
          results = results_baseline,
          selection = "MACROPHYTE",
          subSelection = "Inshore macrophyte debris"
        )
      })
    
    output$ecoPlot_catch_guild_1 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 1
        )
      })
    
    output$ecoPlot_catch_guild_2 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 2
        )
      })
    
    output$ecoPlot_catch_guild_3 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 3
        )
      })
    
    output$ecoPlot_catch_guild_4 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 4
        )
      })
    
    output$ecoPlot_catch_guild_5 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 5
        )
      })
    
    output$ecoPlot_catch_guild_6 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 6
        )
      })
    
    output$ecoPlot_catch_guild_7 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 7
        )
      })
    
    output$ecoPlot_catch_guild_8 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 8
        )
      })
    
    output$ecoPlot_catch_guild_9 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 9
        )
      })
    
    output$ecoPlot_catch_guild_10 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 10
        )
      })
    
    output$ecoPlot_catch_guild_11 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 11
        )
      })
    
    output$ecoPlot_catch_guild_12 <- 
      renderPlot({
        createCatchPlotPerGuild(
          model,
          results = results_baseline,
          dsa = 12
        )
      })
    
    output$ecoPlot_catch_gear_1 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 1
        )
      })
    
    output$ecoPlot_catch_gear_2 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 2
        )
      })
    
    output$ecoPlot_catch_gear_3 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 3
        )
      })
    
    output$ecoPlot_catch_gear_4 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 4
        )
      })
    
    output$ecoPlot_catch_gear_5 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 5
        )
      })
    
    output$ecoPlot_catch_gear_6 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 6
        )
      })
    
    output$ecoPlot_catch_gear_7 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 7
        )
      })
    
    output$ecoPlot_catch_gear_8 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 8
        )
      })
    
    output$ecoPlot_catch_gear_9 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 9
        )
      })
    
    output$ecoPlot_catch_gear_10 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 10
        )
      })
    
    output$ecoPlot_catch_gear_11 <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 11
        )
      })
    
    output$ecoPlot_catch_gear_12_Whaler <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 12
        )
      })

    output$ecoPlot_catch_gear_12_KelpHarvester <- 
      renderPlot({
        createCatchPlotPerGear(
          model,
          results = results_baseline,
          dsa = 12
        )
      })
  })
  
  observeEvent(input$runScenario, {
    if (is.null(input$selectedlocation) || is.null(input$selectedVariant)){
      showModal(
        modalDialog(
          "Please choose a region, time period and run the scenario before exploring scenario output data",
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
    showModal(
      modalDialog(
        "Please wait whilst your scenario model runs.....once completed you can compare the results with the baseline model using options from the Scenario Results tab",
        footer = NULL
      )
    )
    # Run scenario
    model <-
      e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models", model.ident = "scenario")
    # Temperature
    scenario_model <- makeScenarioAdjustments(model,input)

    #print("Got this far 15")
    results_scenario <-
      e2e_run(scenario_model,
              nyears = input$year,
              csv.output = TRUE)
    
    results_baseline <-
      e2e_run(model, nyears = input$year, csv.output = TRUE)
    
    #model_reactive(model)
    results_baseline_reactive(results_baseline)
    results_scenario_reactive(results_scenario)
    scenario_model_reactive(scenario_model)
    
    biomassCompFilename <- paste0(input$selectedlocation, "_" ,input$selectedVariant, "_","biomassComparison.png")
    output$downloadData_biomassComp <- downloadHandler(
      filename = biomassCompFilename,
      content = function(file) {
        ggsave(file, plot = reactivePlot_e2e_compare_runs_bar_aam_plot(), width = 12, height = 10, dpi = 600, units = "in", device = 'png')
      }
    )
    
    biomassCompDataFilename <- paste0(input$selectedlocation, "_" ,input$selectedVariant, "_","biomassComparison.csv")
    output$downloadData_biomassCompData <- downloadHandler(
      filename = biomassCompDataFilename,
      content = function(file) {
        write.csv(reactivePlot_e2e_compare_runs_bar_aam_data(), file, row.names = TRUE)
      }
    )

    if (!is.null(scenario_model)) {
      enable("downloadData_biomassComp")
      runjs("$('#dwnbutton_bm').removeAttr('title');")
    } else {
      disable("downloadData_biomassComp")
      runjs("$('#dwnbutton_bm').attr('title', 'Data not available');")
    }
    
    if (!is.null(scenario_model)) {
      enable("downloadData_biomassCompData")
      runjs("$('#dwnbutton_bmd').removeAttr('title');")
    } else {
      disable("downloadData_biomassCompData")
      runjs("$('#dwnbutton_bmd').attr('title', 'Data not available');")
    }
    
    catchCompFilename <- paste0(input$selectedlocation, "_" ,input$selectedVariant, "_","catchComparison.png")
    output$downloadData_catchComp <- downloadHandler(
      filename = catchCompFilename,
      content = function(file) {
        ggsave(file, plot = reactivePlot_e2e_compare_runs_bar_catch_plot(), width = 12, height = 10, dpi = 600, units = "in", device = 'png')
      }
    )
    
    catchCompDataFilename <- paste0(input$selectedlocation, "_" ,input$selectedVariant, "_","catchComparison.csv")
    output$downloadData_catchCompData <- downloadHandler(
      filename = catchCompDataFilename,
      content = function(file) {
        write.csv(reactivePlot_e2e_compare_runs_bar_catch_data(), file, row.names = TRUE)
      }
    )
    
    if (!is.null(scenario_model)) {
      enable("downloadData_catchComp")
      runjs("$('#dwnbutton_catch').removeAttr('title');")
    } else {
      disable("downloadData_catchComp")
      runjs("$('#dwnbutton_catch').attr('title', 'Data not available');")
    }
    
    if (!is.null(scenario_model)) {
      enable("downloadData_catchCompData")
      runjs("$('#dwnbutton_catchdata').removeAttr('title');")
    } else {
      disable("downloadData_catchCompData")
      runjs("$('#dwnbutton_catchdata').attr('title', 'Data not available');")
    }
    
    
    output$e2e_compare_runs_bar_aam <-
      renderPlot({
        if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runScenario == 0){
          showModal(
            modalDialog(
              "Please choose a region, time period and run the scenario before viewing comparison plots",
              footer = NULL,
              easyClose = TRUE
            )
          )
        }
        reactivePlot_e2e_compare_runs_bar_aam_plot()
      })
    
    output$e2e_compare_runs_bar_catch <-
      renderPlot({
        if (is.null(input$selectedlocation) || is.null(input$selectedVariant) || input$runScenario == 0){
          showModal(
            modalDialog(
              "Please choose a region, time period and run the scenario before viewing comparison plots",
              footer = NULL,
              easyClose = TRUE
            )
          )
        }
        reactivePlot_e2e_compare_runs_bar_catch_plot()
      })
    
    removeModal()
    uuid_s <- UUIDgenerate()
    resultDirScenario <- toString(scenario_model$setup$resultsdir)
    output$downloadData_scenario1 <- downloadHandler(
      filename = function() {
        paste0("scenario_", uuid_s, ".zip")
      },
      content = function(file) {
        zip(zipfile=file, files=resultDirBaseline)
      },
      contentType = "application/zip"
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

shinyApp(ui, server)
