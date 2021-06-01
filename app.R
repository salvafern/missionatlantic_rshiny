library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(tidyr)
library(uuid)
#library(shinyAce)
#library(sendmailR)

#library(promises)
#library(future)
#plan(multisession)

source("createEcoplots.R")
source("createCatchPlotPerGuild.R")
source("createCatchPlotPerGear.R")
source("createEDriversPlots.R")

ui <- navbarPage(
  "StrathE2E-app",
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
               fluidRow(
                  column(
                    6,
                    h4("Marine Ecosystem Modelling Online"),
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
                   tags$a(img(src = "Mission Atlantic Logo Col-01.png", width = '60%',style = "text-align: center; padding:10px;10px;10px;10px;"),href="https://missionatlantic.eu/")
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
                   h4("About StrathE2E Model"),
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
                  h4("Model Overview"),
                  br(),
                  img(src = "Temperate model joined.svg", width = '50%', style = "display: block; margin-left: auto; margin-right: auto;"),
                  br(),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \">StrathE2E is a so-called end-to-end model – it aims to represent the entire interconnected marine ecosystem from physics and chemistry, through microbes and plankton to whales and fisheries in continental shelf regions. To make this feasible, we simplify the ecology - all the plants and animals in the sea are grouped together into what we call 'guilds' of species that have similar properties <a href='#diagram1'>(Diagram 1)</a>.</p>"),
                   p(
                     "The region covered by each model is divided into a shallow inshore and a deeper offshore zone. The water column in the offshore zone is further divided into an upper (surface) layer, and a lower (deep) layer. The seabed in each zone is divided into up to four different sediment habitat types e.g. muddy, sandy, gravel, rocky.",
                     style = "font-family: 'calibri'; font-si16pt"
                   ),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \">Fisheries in StrathE2E are represented by a separate sub-model which is connected to the ecology part. In the sub-model, all of fishing gears used in a region are grouped together into up to 12 different types defined by their effectiveness at catching each of the ecology guilds, the spatial distribution of their activity, <span class=\"mytooltip\">seabed abrasion<span class=\"tooltiptext\">“Seabed abrasion” refers to the scraping or ploughing effects of dragging fishing gear along the seabed. The disturbance re-suspends sediment, releases nutrients and causes mortality  of seabed animals.</span></span> rates, and <span class=\"mytooltip\">discarding<span class=\"tooltiptext\">“Discards” are the components of catch which are returned to the sea due to being of no commercial value, or legal restrictions on size or catch limits. In the model, all discards are assumed to be dead.</span></span> patterns.</p>"),
                  HTML("<p style = \"font-family: 'calibri'; font-si16pt \"><b>Diagram 1:</b> <i>Ecological guilds or classes of dead and living material included in the StrathE2E model</i>"),
                  br(),
                  column(
                    9,
                  img(src = "guilds2.svg", width = '75%', style = "display: block !important; margin: 0 auto !important; float:none !important;" ,id="diagram1")
                  )
                 )
                 )
               ),
             tabPanel(
               title = "How the Model Works",
               fluidRow(
                 column(
                   8,
                   h4("How the Model Works"),
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
             #       img(src = "Workflow.svg", width = '150%'),#, style = "display: block; margin-left: auto; margin-right: auto;"),
             #       )
             #   )
             # )
             ),
  tabPanel(
    title = "Using This Website",
    fluidRow(
      column(
        6,
        h4("There are four stages to using this website:"),
        img(src = "Workflow.svg", width = '150%'),#, style = "display: block; margin-left: auto; margin-right: auto;"),
      )
    )
  ),
  navbarMenu(
    "Setup Model",
    tabPanel(
      title = "Select Region and Time Period",
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
    "View Model Inputs",
    tabPanel(
      title = "Environmental inputs",
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
      title = "Fishery inputs",
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
    "Run Model",
    tabPanel(
      title = "Run Model",
      fluidRow(
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
        h3("Baseline output"),
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
      )
      )
    ),
    "----",
    "Plot Outputs",
    tabPanel(
      title = "Ecological outputs",
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
    "Modify Inputs",
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
      title = "Nutrient Inputs",
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
    tabPanel(title = "Seabed Abrasion",
      uiOutput("uiSeabedAbrasian")),
    tabPanel(
      title = "Guild Discard Per Gear",
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
    "----",
    "Run scenario",
    tabPanel(
      title = "Run Scenario",
      sliderInput(
        "year",
        "Year:",
        min = 1,
        max = 50,
        value = 5,
        width = "100%"
      ),
      fluidRow(
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
      title = "Download Scenario Output",
      # sliderInput(
      #   "year",
      #   "Year:",
      #   min = 1,
      #   max = 50,
      #   value = 5,
      #   width = "100%"
      # ),
      fluidRow(
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
    "Compare Baseline With Scenario",
    tabPanel(
      title = "Compare Biomasses",
      fluidRow(column(width = 5, wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">‘Tornado’ bar-plot of the differences in annual averaged quantities of model components in the whole model region between your scenario model run and the baseline."),
                                           HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Green bars to the right indicate that the (bio)mass was higher in the scenario run than the baseline, and vice-versa for red bars to the left.")))),
      fluidRow(column(
        9,
        plotOutput("e2e_compare_runs_bar_aam",height = "600px")
      ),
      column(
        6,
        useShinyjs(),
        div(
          id = "dwnbutton_bm",
          downloadButton(
            "downloadData_biomassComp",
            "Download biomass comparison",
            disabled = "disabled"
          )
        )
      ))
    ),
    tabPanel(
      title = "Compare Fishery Catches",
      fluidRow(column(width = 5, wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">‘Tornado’ bar-plot of the differences in annual fishery landings and discards in the whole model region between your scenario model run and the baseline."),
                                           HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Green and black bars to the right indicate that the landings or discards were higher in the scenario run than the baseline, and vice-versa for red and grey bars to the left.")))),
      fluidRow(
        column(
          9,
          plotOutput("e2e_compare_runs_bar_catch",height = "600px")
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

server <- function(input, output, session) {
  
  output$uiFishingActivity <- renderUI({
    # current chosen model on dropdown lists
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
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
  })
    
  
  output$uiSeabedAbrasian <- renderUI({
    # current chosen model on dropdown lists
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
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
    gear1Plough <- model$data$fleet.model$gear_ploughing_rate[1]
    gear2Plough <- model$data$fleet.model$gear_ploughing_rate[2]
    gear3Plough <- model$data$fleet.model$gear_ploughing_rate[3]
    gear4Plough <- model$data$fleet.model$gear_ploughing_rate[4]
    gear5Plough <- model$data$fleet.model$gear_ploughing_rate[5]
    gear6Plough <- model$data$fleet.model$gear_ploughing_rate[6]
    gear7Plough <- model$data$fleet.model$gear_ploughing_rate[7]
    gear8Plough <- model$data$fleet.model$gear_ploughing_rate[8]
    gear9Plough <- model$data$fleet.model$gear_ploughing_rate[9]
    gear10Plough <- model$data$fleet.model$gear_ploughing_rate[10]
    gear11Plough <- model$data$fleet.model$gear_ploughing_rate[11]
    gear12Plough <- model$data$fleet.model$gear_ploughing_rate[12]
    
    fluidRow(
      column(width = 5,
             wellPanel(style = "overflow-y:scroll; max-height: 600px",
        if (gear1Plough > 0) {
                         wellPanel(
                           sliderInput(
                             "pelTrawlPlough",
                             paste(gear1, "seabed abrasion", sep=" "),
                             min = 0,
                             max = 2.0,
                             value = 0.0,
                             step = 0.2,
                             width = "100%"
                           )
                         )
        },
        if (gear2Plough > 0) {
          wellPanel(
          sliderInput(
            "sanSpratTrawlPlough",
            paste(gear2, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear3Plough > 0) {
          wellPanel(
          sliderInput(
            "llMackerelPlough",
            paste(gear3, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear4Plough > 0) {
        wellPanel(
          sliderInput(
            "beamTrawlPlough",
            paste(gear4, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear5Plough > 0) {
        wellPanel(
          sliderInput(
            "demersalSeinePlough",
            paste(gear5, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear6Plough > 0) {
        wellPanel(
          sliderInput(
            "demersalOtterTrawlPlough",
            paste(gear6, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear7Plough > 0) {
        wellPanel(
          sliderInput(
            "gillLongDemersalPlough",
            paste(gear7, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear8Plough > 0) {
        wellPanel(
          sliderInput(
            "beamTrawlShrimpPlough",
            paste(gear8, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear9Plough > 0) {
        wellPanel(
          sliderInput(
            "nephropsTrawlPlough",
            paste(gear9, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear10Plough > 0) {
        wellPanel(
          sliderInput(
            "creelsPlough",
            paste(gear10, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear11Plough > 0) {
        wellPanel(
          sliderInput(
            "molluscDredgePlough",
            paste(gear11, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        },
        if (gear12Plough > 0) {
        wellPanel(
          sliderInput(
            "whalerPlough",
            paste(gear12, "seabed abrasion", sep=" "),
            min = 0,
            max = 2.0,
            value = 1.0,
            step = 0.2,
            width = "100%"
          )
        )
        }
       )
      ),
      column(width = 5,
      wellPanel(
        HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Use the slider bars to rescale the seabed abrasion rate of each fishing gear."),
        HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">1 represents no change from the baseline model rates; 0.5 means that abrasion rates are halved; 2 means that rates are doubled."),
        HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Seabed abrasion rate of each gear is measured in m<sup>2</sup> of seabed abraded per second of gear deployment."),
        HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">In the model, seabed abrasion re-suspends organic matter and releases nutrients from the sediment, and causes collateral mortality of benthos guilds.")
      )
      )
    )
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
  
  output$textRunBaselineModel <- renderUI({fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Run your selected  model  here in its 'out of the box' state"),
                                                    HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Running the baseline will take a few seconds on our computer server. The results are saved in memory for you to explore using our graph drawing tools, or you can download the output as .csv files to analyse yourself if you wish (e.g. read them into Excel)."),
                                                    HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Progress to the “Setup Scenario” tab to configure a new set of model inputs and then rerun the model and compare the results with your baseline run")))}) 
  
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

  output$textRunScenarioFiles <- renderUI({fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">The following .csv files are available to download : WHOLEDOMAIN_model_anav_biomass-scenario, OFFSHORE_model_anav_biomass-scenario, INSHORE_model_anav_biomass-scenario, OFFSHORE_landingcomposition_by_gear-scenario, OFFSHORE_discardcomposition_by_gear-scenario, INSHORE_landingcomposition_by_gear-scenario, INSHORE_discardcomposition_by_gear-scenario where scenario is an identifier for your scenario data."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Data are scaled to represent the quantity per m<sup>2</sup> of the whole region for reach model."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"anav_biomass files\" contain annual average mass of each component of the model in the named zone. The files includes data on layer and zone thickness and area so that the mass data can be converted to concentrations."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">\"landingcomposition\" and \"discardcomposition\" files contain a matrix of landed and discarded quantity by living guild and gear type. Units: (milli-Moles of nitrogen per m<sup>2</sup> of whole model region per year).")))}) 
  
  output$textRunScenarioModel <- renderUI({fluidRow(wellPanel(HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px \">Run your scenario  model  here."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">Use the slider bar to select the number of years to run. Up to 40 years may be required to arrive at a new steady state depending on the changes you have made from the baseline. However the extent of changes within shorter period may also be of interest."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">The model will take about 2 sec per year to run on our server."),
                                                              HTML("<p style = \"font-family: 'calibri'; font-si16pt; padding:5px  \">The results are saved in memory for you to compare with your baseline model run, or you can download the output as .csv files to analyse yourself if you wish (e.g. read them into Excel).")))}) 
  
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
  
  output$ui <- renderUI({
    model <- e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models")
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
    switch(
      input$selectedParameter,
      "Planktivorous fish" = fluidRow(
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_pel",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_pel",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_pel",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_pel",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_pel",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_pel",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_pel",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_pel",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_pel",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_pel",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_pel",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_pel_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_pel",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$pelagic[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_pel_reset", "Reset")
        )
        )
      ),
      "Demersal fish" = fluidRow(
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_dem",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_dem",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_dem",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_dem",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_dem",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_dem_reset", "Reset")
        ),
        #column(
        #  width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_dem",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_dem",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_dem",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_dem",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_dem",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_dem",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_dem_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_dem",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$demersal[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_dem_reset", "Reset")
        )
        )
      ),
      "Migratory fish" = fluidRow(
        #column(
        #  width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_mig",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_mig",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_mig",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_mig",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_mig",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_mig_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_mig",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_mig",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_mig",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_mig",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_mig",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_mig",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_mig_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_mig",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$migratory[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_mig_reset", "Reset")
        )
        )
      ),
      "Suspension/deposit feeding benthos" = fluidRow(
        #       column(
        #         width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_fb",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_fb",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_fb",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_fb",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_fb",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_fb_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_fb",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_fb",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_fb",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_fb",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_fb",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_fb",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_fb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_fb",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$filtben[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_fb_reset", "Reset")
        )
        )
      ),
      "Carnivore/scavenge feeding benthos" = fluidRow(
        #       column(
        #         width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_cb",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_cb",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_cb",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_cb",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_cb",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_cb_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_cb",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_cb",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_cb",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_cb",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_cb",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_cb",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_cb_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_cb",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnben[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_cb_reset", "Reset")
        )
        )
      ),
      "Carnivorous zooplankton (e.g. squids)" = fluidRow(
        #       column(
        #         width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_cz",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_cz",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_cz",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[2],
            step = 0.1,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_cz",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_cz",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_cz_reset", "Reset")
          #)
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_cz",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_cz",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_cz",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_cz",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_cz",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_cz",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_cz_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_cz",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$carnzoo[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_cz_reset", "Reset")
        )
        )
      ),
      "Seabirds" = fluidRow(
        #       column(
        #         width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_b",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_b",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_b",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_b",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_b",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_b_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_b",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_b",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_b",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_b",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_b",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_b",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_b_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_b",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$bird[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_b_reset", "Reset")
        )
        )
      ),
      "Pinnipeds (seals)" = fluidRow(
        #        column(
        #          width = 5
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_s",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_s",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_s",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_s",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_s",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_s_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_s",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_s",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_s",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_s",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_s",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_s",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_s_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_s",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$seal[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_s_reset", "Reset")
        )
        )
      ),
      "Cetaceans" = fluidRow(
        #        column(
        #          width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_ceta",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_ceta",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_ceta",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_ceta",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_ceta",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_ceta_reset", "Reset")
        ),
        #        column(
        #          width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_ceta",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_ceta",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_ceta",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_ceta",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_ceta",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_ceta",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_ceta_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_ceta",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$ceta[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_ceta_reset", "Reset")
        )
        )
      ),
      "Macrophytes (kelp)" = fluidRow(
        #        column(
        #          width = 5,
        wellPanel(style = "overflow-y:scroll; max-height: 600px",
        wellPanel(
          sliderInput(
            "pelagicTrawlDiscard_kelp",
            paste(gear1, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[1],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("pelagicTrawlDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "sanSpratTrawlDiscard_kelp",
            paste(gear2, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[2],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("sanSpratTrawlDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "llMackerelDiscard_kelp",
            paste(gear3, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[3],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("llMackerelDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlDiscard_kelp",
            paste(gear4, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[4],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "demersalSeineDiscard_kelp",
            paste(gear5, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[5],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalSeineDiscard_kelp_reset", "Reset")
        ),
        #        column(
        #         width = 5,
        wellPanel(
          sliderInput(
            "demersalOtterTrawlDiscard_kelp",
            paste(gear6, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[6],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("demersalOtterTrawlDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "gillLongDemersalDiscard_kelp",
            paste(gear7, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[7],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("gillLongDemersalDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "beamTrawlShrimpDiscard_kelp",
            paste(gear8, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[8],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("beamTrawlShrimpDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "nephropsTrawlDiscard_kelp",
            paste(gear9, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[9],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("nephropsTrawlDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "creelsDiscard_kelp",
            paste(gear10, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[10],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("creelsDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "molluscDredgeDiscard_kelp",
            paste(gear11, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[11],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("molluscDredgeDiscard_kelp_reset", "Reset")
        ),
        wellPanel(
          sliderInput(
            "whalerDiscard_kelp",
            paste(gear12, "Discard", sep=" "),
            min = 0,
            max = 1.0,
            value = model$data$fleet.model$gear_group_discard$kelp[12],
            step = 0.000001,
            width = "100%"
          ),
          actionButton("whalerDiscard_kelp_reset", "Reset")
        )
        )
      )
    )
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
        "Please wait whilst model runs baseline. Once completed you can explore plots on model exploration tab menu",
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
    

    # output$edriver_plot_1 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Surface irradiance"
    #     )
    #   })
    # 
    # output$edriver_plot_2 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Susp.partic. matter"
    #     )
    #   })
    # 
    # output$edriver_plot_3 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Temperature"
    #     )
    #   })
    # 
    # output$edriver_plot_4 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Diffusivity gradient"
    #     )
    #   })
    # 
    # output$edriver_plot_5 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "External Inflow"
    #     )
    #   })
    # 
    # output$edriver_plot_6 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "River discharge"
    #     )
    #   })
    # 
    # output$edriver_plot_7 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Wave height"
    #     )
    #   })
    # 
    # output$edriver_plot_8 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Sediment disturbance"
    #     )
    #   })
    # 
    # output$edriver_plot_9 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Boundary nitrate"
    #     )
    #   })
    # 
    # output$edriver_plot_10 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Boundary ammonia"
    #     )
    #   })
    # 
    # output$edriver_plot_11 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Boundary phytoplankton"
    #     )
    #   })
    # 
    # output$edriver_plot_12 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Boundary detritus"
    #     )
    #   })
    # 
    # output$edriver_plot_13 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "River nitrate"
    #     )
    #   })
    # 
    # output$edriver_plot_14 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "River ammonia"
    #     )
    #   })
    # 
    # output$edriver_plot_15 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Atmospheric nitrate"
    #     )
    #   })
    # 
    # output$edriver_plot_16 <- 
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     createEDriversPlots(
    #       model,
    #       selection = "Atmospheric ammonia"
    #     )
    #   })
    # 
    # output$basePlot_fdriver_activity <-
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     e2e_plot_fdrivers(model, selection = "ACTIVITY")
    #   })
    # 
    # output$basePlot_fdriver_abrasion <-
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     e2e_plot_fdrivers(model, selection = "ABRASION")
    #   })
    # 
    # output$basePlot_fdriver_harvestr <-
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     e2e_plot_fdrivers(model, selection = "HARVESTR")
    #   })
    # 
    # output$basePlot_fdriver_discards <-
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     e2e_plot_fdrivers(model, selection = "DISCARDS")
    #   })
    # 
    # output$basePlot_fdriver_offal <-
    #   renderPlot({
    #     model <- e2e_read(input$selectedlocation, input$selectedVariant,models.path="Models",model.ident = "baseline")
    #     e2e_plot_fdrivers(model, selection = "OFFAL")
    #   })
    
    # output$ecoPlot_catch_gear <-
    #   renderPlot({
    #     e2e_plot_catch(model, results_baseline, selection = "BY_GEAR")
    #   })
    # 
    # output$ecoPlot_catch_guild <-
    #   renderPlot({
    #     e2e_plot_catch(model, results_baseline, selection = "BY_GUILD")
    #   })
    # 
    # output$ecoPlot_biomass <-
    #   renderPlot({
    #     e2e_plot_biomass(
    #       model,
    #       ci.data = FALSE,
    #       use.saved = FALSE,
    #       use.example = FALSE,
    #       results = results_baseline
    #     )
    #   })
  })
  
  observeEvent(input$runScenario, {
    showModal(
      modalDialog(
        "Please wait whilst your scenario model runs.....once completed you can compare the results with the baseline model using options from the Scenario Results tab",
        footer = NULL
      )
    )
    # Run scenario
    model <-
      e2e_read(input$selectedlocation, input$selectedVariant, models.path="Models", model.ident = "scenario")
    # If simultaneous run - do baseline first
    #if (input$runBaselinePlusScenario == 1) {
      #print("Running baseline within scenario run")
      results_baseline <- e2e_run(model, nyears = input$year, csv.output = TRUE)
    #}
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
      scenario_model$data$fleet.model$gear_mult[10] <-
      input$creels
    if (!is.null(input$molluscDredge))
      scenario_model$data$fleet.model$gear_mult[11] <-
      input$molluscDredge
    if (!is.null(input$whaler))
      scenario_model$data$fleet.model$gear_mult[12] <-
      input$whaler
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
    
    # biomassPlot <- e2e_compare_runs_bar(
    #   selection = "AAM",
    #   model1 = model,
    #   use.saved1 = FALSE,
    #   results_baseline,
    #   model2 = scenario_model,
    #   use.saved2 = FALSE,
    #   results_scenario,
    #   log.pc = "PC",
    #   zone = "W",
    #   bpmin = (-50),
    #   bpmax = (+50),
    #   maintitle = ""
    # )
    
    #ggsave("biomassComparison.pdf", biomassPlot)
    # output$downloadData_biomassComp <- downloadHandler(
    #   filename = function() {
    #     "biomassComparison.pdf"
    #   },
    #   content = function(file) {
    #     file.copy("biomassComparison.pdf", file, overwrite=TRUE)
    #   }
    # )
    # 
    # if (!is.null(model)) {
    #   enable("downloadData_biomassComp")
    #   runjs("$('#dwnbutton_bm').removeAttr('title');")
    # } else {
    #   disable("downloadData_biomassComp")
    #   runjs("$('#dwnbutton_bm').attr('title', 'Data not available');")
    # }
    
    output$e2e_compare_runs_bar_aam <-
      renderPlot({
        e2e_compare_runs_bar(
          selection = "AAM",
          model1 = model,
          use.saved1 = FALSE,
          results_baseline,
          model2 = scenario_model,
          use.saved2 = FALSE,
          results_scenario,
          log.pc = "PC",
          zone = "W",
          bpmin = (-50),
          bpmax = (+50),
          maintitle = ""
        )
      })
    
    
    results_baseline <-
      e2e_run(model, nyears = input$year, csv.output = TRUE)
    
    output$e2e_compare_runs_bar_catch <-
      renderPlot({
        e2e_compare_runs_bar(
          selection = "CATCH",
          model1 = model,
          use.saved1 = FALSE,
          results_baseline,
          model2 = scenario_model,
          use.saved2 = FALSE,
          results_scenario,
          log.pc = "PC",
          zone = "W",
          bpmin = (-50),
          bpmax = (+50),
          maintitle = ""
        )
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
