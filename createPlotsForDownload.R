library(shiny)
library(StrathE2E2)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(shinyjs)
library(ggplot2)
library(tidyr)
library(uuid)

source("createEcoplots.R")
source("createCatchPlotPerGuild.R")
source("createCatchPlotPerGear.R")
source("createEDriversPlots.R")

selectedLocation = "Celtic_Sea"
selectedVariant <- "2003-2013" 

#model <- e2e_read(selectedlocation, selectedVariant,models.path="Models",model.ident = "baseline")

model <- e2e_read("Celtic_Sea", "2003-2013",models.path="Models",model.ident = "baseline")
results_baseline <- e2e_run(model, nyears = 5, csv.output = TRUE)
resultDirBaseline <- toString(model$setup$resultsdir)

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","SurfaceIrradiance.png"))
surfaceIrr <- createEDriversPlots(model,selection = "Surface irradiance")
dev.off()

# png(filename = file.path("EnvironmentalInput", paste0(selectedLocation, "_" ,selectedVariant,"_","SurfaceIrradiance.png")))
# surfaceIrr <- createEDriversPlots(model,selection = "Surface irradiance")
# dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","SuspParticMatter.png"))
suspPart <- createEDriversPlots(model,selection = "Susp.partic. matter")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","Temperature.png"))
temp <- createEDriversPlots(model,selection = "Temperature")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","DiffusivityGradient.png"))
diffGrad <- createEDriversPlots(model,selection = "Diffusivity gradient")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","ExternalInflow.png"))
extInflow <- createEDriversPlots(model,selection = "External Inflow")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","RiverDischarge.png"))
riverDisch <- createEDriversPlots(model,selection = "River discharge")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","WaveHeight.png"))
waveHeight <- createEDriversPlots(model,selection = "Wave height")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","SedimentDisturbance.png"))
sedDist <- createEDriversPlots(model,selection = "Sediment disturbance")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","BoundaryNitrate.png"))
boundNitrate <- createEDriversPlots(model,selection = "Boundary nitrate")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","BoundaryAmmonia.png"))
boundAmmonia <- createEDriversPlots(model,selection = "Boundary ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","BoundaryPhytoplankton.png"))
boundPytho <- createEDriversPlots(model,selection = "Boundary phytoplankton")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","BoundaryDetritus.png"))
boundDet <- createEDriversPlots(model,selection = "Boundary detritus")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","RiverNitrate.png"))
riverNit <- createEDriversPlots(model,selection = "River nitrate")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","RiverAmmonia.png"))
riverAmmonia <- createEDriversPlots(model,selection = "River ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","AtmosphericNitrate.png"))
atmosNit <- createEDriversPlots(model,selection = "Atmospheric nitrate")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EnvironmentalInput","_","AtmosphericAmmonia.png"))
atmosAmmonia <- createEDriversPlots(model,selection = "Atmospheric ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "FisheryInput","_","Activity.png"))
fisheryActivity <- e2e_plot_fdrivers(model, selection = "ACTIVITY")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "FisheryInput","_","Abrasion.png"))
fisheryAbrasion <- e2e_plot_fdrivers(model, selection = "ABRASION")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "FisheryInput","_","HarvestR.png"))
fisheryHarvestr <- e2e_plot_fdrivers(model, selection = "HARVESTR")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "FisheryInput","_","Discards.png"))
fisheryDiscards <- e2e_plot_fdrivers(model, selection = "DISCARDS")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "FisheryInput","_","Offal.png"))
fisheryOffal <- e2e_plot_fdrivers(model, selection = "OFFAL")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_nut_phyt_Detritus.png"))
ecoPlot_nut_phyt_Detritus <- createEcoplots(model, results = results_baseline, selection = "NUT_PHYT",subSelection = "Detritus")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_nut_phyt_Ammonia.png"))
ecoPlot_nut_phyt_Ammonia <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "NUT_PHYT",
      subSelection = "Ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_nut_phyt_Nitrate.png"))
ecoPlot_nut_phyt_Nitrate <- createEcoplots(
      model,
      results = results_baseline,
      selection = "NUT_PHYT",
      subSelection = "Nitrate")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_nut_phyt_Phytoplankton.png"))
ecoPlot_nut_phyt_Phytoplankton <- createEcoplots(
      model,
      results = results_baseline,
      selection = "NUT_PHYT",
      subSelection = "Phytoplankton")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_InAmm.png"))
ecoPlot_sediment_InAmm <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Inshore ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_OffAmm.png"))
ecoPlot_sediment_OffAmm <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Offshore ammonia")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_InNit.png"))
ecoPlot_sediment_InNit <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Inshore nitrate")
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_OffNit.png"))
ecoPlot_sediment_OffNit <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Offshore nitrate"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_InDet.png"))
ecoPlot_sediment_InDet <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Inshore detritus"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_OffDet.png"))
ecoPlot_sediment_OffDet <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Offshore detritus"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_InCorp.png"))
ecoPlot_sediment_InCorp <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Inshore corpses"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_sediment_OffCorp.png"))
ecoPlot_sediment_OffCorp <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "SEDIMENT",
      subSelection = "Offshore corpses"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_zooplankton_OmnZoo.png"))
ecoPlot_zooplankton_OmnZoo <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "ZOOPLANKTON",
      subSelection = "Omnivorous zooplankton"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_zooplankton_CarnZoo.png"))
ecoPlot_zooplankton_CarnZoo <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "ZOOPLANKTON",
      subSelection = "Carnivorous zooplankton"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_fish_PlankFish.png"))
ecoPlot_fish_PlankFish <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "FISH",
      subSelection = "Planktivorous fish"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_fish_PlankFishLarv.png"))
ecoPlot_fish_PlankFishLarv <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "FISH",
      subSelection = "Planktivorous fish larvae"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_fish_DemFish.png"))
ecoPlot_fish_DemFish <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "FISH",
      subSelection = "Demersal fish"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_fish_DemFishLarv.png"))
ecoPlot_fish_DemFishLarv <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "FISH",
      subSelection = "Demersal fish larvae"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_benthos_BenSusFeed.png"))
ecoPlot_benthos_BenSusFeed <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "BENTHOS",
      subSelection = "Benthos susp/dep feeders"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_benthos_BenSusFeedLarv.png"))
ecoPlot_benthos_BenSusFeedLarv <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "BENTHOS",
      subSelection = "Benthos susp/dep feeders larvae"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_benthos_BenCarnFeed.png"))
ecoPlot_benthos_BenCarnFeed <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "BENTHOS",
      subSelection = "Benthos carn/scav feeders"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_benthos_BenCarnFeed.png"))
ecoPlot_benthos_BenCarnFeed <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "BENTHOS",
      subSelection = "Benthos carn/scav feeders"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_benthos_BenCarnFeedLarv.png"))
ecoPlot_benthos_BenCarnFeedLarv <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "BENTHOS",
      subSelection = "Benthos carn/scav feeders larvae"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_predator_birds.png"))
ecoPlot_predator_birds <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "PREDATORS",
      subSelection = "Birds"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_predator_pinnipeds.png"))
ecoPlot_predator_pinnipeds <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "PREDATORS",
      subSelection = "Pinnipeds"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_predator_cetaceans.png"))
ecoPlot_predator_cetaceans <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "PREDATORS",
      subSelection = "Cetaceans"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_predator_migFish.png"))
ecoPlot_predator_migFish <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "PREDATORS",
      subSelection = "Migratory fish"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_corpdisc_corpses.png"))
ecoPlot_corpdisc_corpses <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "CORP_DISC",
      subSelection = "Corpses"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_corpdisc_discard.png"))
ecoPlot_corpdisc_discard <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "CORP_DISC",
      subSelection = "Discards"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_macrophyte_inshore.png"))
ecoPlot_macrophyte_inshore <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "MACROPHYTE",
      subSelection = "Inshore macrophytes"
    )
dev.off()

png(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "EcologicalOutput","_","ecoPlot_macrophyte_inshoreDeb.png"))
ecoPlot_macrophyte_inshoreDeb <-    createEcoplots(
      model,
      results = results_baseline,
      selection = "MACROPHYTE",
      subSelection = "Inshore macrophyte debris"
    )
dev.off()

planktivorousFish <-    createCatchPlotPerGuild(
  model,
  results = results_baseline,
  dsa = 1
)
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","planktivorousFish.png"),planktivorousFish,  width = 12, height = 4, dpi = 600, units = "in", device= "png")


Quota_limited_Demersal_fish <-    createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 2
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Quota_limited_Demersal_fish.png"),Quota_limited_Demersal_fish,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Non_quota_demersal_fish <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 3
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Non_quota_demersal_fish.png"),Non_quota_demersal_fish,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Migratory_fish <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 4
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Migratory_fish.png"),Migratory_fish,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Susp_deposit_feeding_benthos <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 5
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Susp_deposit_feeding_benthos.png"),Susp_deposit_feeding_benthos,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Carn_scavebge_feeding_benthos <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 6
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Carn_scavebge_feeding_benthos.png"),Carn_scavebge_feeding_benthos,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Pelagic_invertebrates <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 7
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Pelagic_invertebrates.png"),Pelagic_invertebrates,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

birds <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 8
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","birds.png"),birds,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Pinnipeds <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 9
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Pinnipeds.png"),Pinnipeds,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Cetaceans <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 10
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Cetaceans.png"),Cetaceans,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Macrophytes <-     createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 11
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","Macrophytes.png"),Macrophytes,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

All_guilds_combined <-    createCatchPlotPerGuild(
      model,
      results = results_baseline,
      dsa = 12
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGuildOutput","_","All_guilds_combined.png"),All_guilds_combined,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Pelagic_Trawl_Seine <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 1
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Pelagic_Trawl_Seine.png"),Pelagic_Trawl_Seine,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

if(selectedLocation == "North_Sea"){
Sandeel_sprat_trawl_Otter30_70mm_TR3 <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 2
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Sandeel_sprat_trawl_Otter30_70mm_TR3.png"),Sandeel_sprat_trawl_Otter30_70mm_TR3,  width = 12, height = 4, dpi = 600, units = "in", device= "png")
}

if(selectedLocation == "Celtic_Sea"){
Otter30_70mm_TR3_sandeel_sprat <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 2
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Otter30_70mm_TR3_sandeel_sprat.png"),Otter30_70mm_TR3_sandeel_sprat,  width = 12, height = 4, dpi = 600, units = "in", device= "png")
}

Longline_mackerel <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 3
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Longline_mackerel.png"),Longline_mackerel,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Beam_Trawl_BT1BT2 <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 4
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Beam_Trawl_BT1BT2.png"),Beam_Trawl_BT1BT2,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Demersal_Seine <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 5
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Demersal_Seine.png"),Demersal_Seine,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Demersal_Otter_Trawl_TR1 <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 6
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Demersal_Otter_Trawl_TR1.png"),Demersal_Otter_Trawl_TR1,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Gill_Nets_Longline_demersal <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 7
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Gill_Nets_Longline_demersal.png"),Gill_Nets_Longline_demersal,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Beam_Trawl_shrimp <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 8
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Beam_Trawl_shrimp.png"),Beam_Trawl_shrimp,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Nephrops_Trawl_TR2 <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 9
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Nephrops_Trawl_TR2.png"),Nephrops_Trawl_TR2,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Creels  <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 10
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Creels.png"),Creels,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

Mollusc_Dredge <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 11
    )
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Mollusc_Dredge.png"),Mollusc_Dredge,  width = 12, height = 4, dpi = 600, units = "in", device= "png")

if(selectedLocation == "North_Sea"){
  Whaler <-    createCatchPlotPerGear(
      model,
      results = results_baseline,
      dsa = 12
    )
  ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","Whaler.png"),Whaler,  width = 12, height = 4, dpi = 600, units = "in", device= "png")
}

if(selectedLocation == "Celtic_Sea"){
  KelpHarvester <-    createCatchPlotPerGear(
  model,
  results = results_baseline,
  dsa = 12
)
ggsave(filename = paste0(selectedLocation, "_" ,selectedVariant, "_", "CatchPerGearOutput","_","KelpHarvester.png"),KelpHarvester,  width = 12, height = 4, dpi = 600, units = "in", device= "png")
}


