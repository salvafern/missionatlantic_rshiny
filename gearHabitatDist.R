# Function to create discard per gear UI


createGearHabDistUI <- function(model,input,jsCode) {
  rockInArea <- model$data$physical.parameters$x_area_s0
  fineInArea <- model$data$physical.parameters$x_area_s1 
  medInArea <-  model$data$physical.parameters$x_area_s2 
  coarseInArea <- model$data$physical.parameters$x_area_s3
  rockOffArea <- model$data$physical.parameters$x_area_d0 
  fineOffArea <- model$data$physical.parameters$x_area_d1 
  medOffArea <- model$data$physical.parameters$x_area_d2
  coarseOffArea <- model$data$physical.parameters$x_area_d3 
  
  pelInRock <- model$data$fleet.model$gear_habitat_activity$s0[1] 
  pelInFine <- model$data$fleet.model$gear_habitat_activity$s1[1] 
  pelInMed <- model$data$fleet.model$gear_habitat_activity$s2[1] 
  pelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[1]
  pelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[1] 
  pelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[1] 
  pelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[1] 
  pelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[1] 
  
  # Getting Total Inshore/Offshore here
  totalInPel <- pelInRock + pelInFine + pelInMed + pelInCoarse
  totalOffPel <- pelOffRock + pelOffFine + pelOffMed + pelOffCoarse
  totalOverallPel <- totalInPel + totalOffPel
  percentageInPelDefault <- totalInPel/totalOverallPel * 100
  percentageOutPelDefault <- 100 - percentageInPelDefault
  
  # Now getting percentages Inshore
  if(rockInArea==0){
    percentagePelInRockDefault <- 0
  } else if(coarseInArea==0 && medInArea==0 && fineInArea==0){
    percentagePelInRockDefault <- 100
  } else {
    percentagePelInRockDefault <- pelInRock/totalInPel * 100
  }
  percentagePelInFineDefault <- pelInFine/totalInPel * 100
  percentagePelInMedDefault <- pelInMed/totalInPel * 100
  percentagePelInCoarseDefault <- pelInCoarse/totalInPel * 100
  #totalPercentageInPelDefault <- percentagePelInRockDefault + percentagePelInFineDefault + percentagePelInMedDefault + percentagePelInCoarseDefault
  # Now getting percentages Offshore
  percentagePelOffRockDefault <- pelOffRock/totalOffPel * 100
  percentagePelOffFineDefault <- pelOffFine/totalOffPel * 100
  percentagePelOffMedDefault <- pelOffMed/totalOffPel * 100
  percentagePelOffCoarseDefault <- pelOffCoarse/totalOffPel * 100
  #totalPercentageOffPelDefault <- percentagePelOffRockDefault + percentagePelOffFineDefault + percentagePelOffMedDefault + percentagePelOffCoarseDefault
  
  sandeelInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
  sandeelInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
  sandeelInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
  sandeelInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
  sandeelOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
  sandeelOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
  sandeelOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2] 
  sandeelOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
  # Getting Total Inshore/Offshore here
  totalInSandeel <- sandeelInRock + sandeelInFine + sandeelInMed + sandeelInCoarse
  totalOffSandeel <- sandeelOffRock + sandeelOffFine + sandeelOffMed + sandeelOffCoarse
  totalOverallSandeel <- totalInSandeel + totalOffSandeel
  percentageInSandeelDefault <- totalInSandeel/totalOverallSandeel * 100
  percentageOutSandeelDefault <- 100 - percentageInSandeelDefault
  # Now getting percentages Inshore
  percentageSandeelInRockDefault <- sandeelInRock/totalInSandeel * 100
  percentageSandeelInFineDefault <- sandeelInFine/totalInSandeel * 100
  percentageSandeelInMedDefault <- sandeelInMed/totalInSandeel * 100
  percentageSandeelInCoarseDefault <- sandeelInCoarse/totalInSandeel * 100
  totalPercentageInSandeelDefault <- percentageSandeelInRockDefault + percentageSandeelInFineDefault + percentageSandeelInMedDefault + percentageSandeelInCoarseDefault
  # Now getting percentages Offshore
  percentageSandeelOffRockDefault <- sandeelOffRock/totalOffSandeel * 100
  percentageSandeelOffFineDefault <- sandeelOffFine/totalOffSandeel * 100
  percentageSandeelOffMedDefault <- sandeelOffMed/totalOffSandeel * 100
  percentageSandeelOffCoarseDefault <- sandeelOffCoarse/totalOffSandeel * 100
  totalPercentageOffSandeelDefault <- percentageSandeelOffRockDefault + percentageSandeelOffFineDefault + percentageSandeelOffMedDefault + percentageSandeelOffCoarseDefault
  
  otterInRock <- model$data$fleet.model$gear_habitat_activity$s0[2]
  otterInFine <- model$data$fleet.model$gear_habitat_activity$s1[2]
  otterInMed <- model$data$fleet.model$gear_habitat_activity$s2[2]
  otterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[2]
  otterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[2]
  otterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[2]
  otterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[2] 
  otterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[2]
  # Getting Total Inshore/Offshore here
  totalInOtter <- otterInRock + otterInFine + otterInMed + otterInCoarse
  totalOffOtter <- otterOffRock + otterOffFine + otterOffMed + otterOffCoarse
  totalOverallOtter <- totalInOtter + totalOffOtter
  percentageInOtterDefault <- totalInOtter/totalOverallOtter * 100
  percentageOutOtterDefault <- 100 - percentageInOtterDefault
  # Now getting percentages Inshore
  percentageOtterInRockDefault <- otterInRock/totalInOtter * 100
  percentageOtterInFineDefault <- otterInFine/totalInOtter * 100
  percentageOtterInMedDefault <- otterInMed/totalInOtter * 100
  percentageOtterInCoarseDefault <- otterInCoarse/totalInOtter * 100
  totalPercentageInOtterDefault <- percentageOtterInRockDefault + percentageOtterInFineDefault + percentageOtterInMedDefault + percentageOtterInCoarseDefault
  # Now getting percentages Offshore
  percentageOtterOffRockDefault <- otterOffRock/totalOffOtter * 100
  percentageOtterOffFineDefault <- otterOffFine/totalOffOtter * 100
  percentageOtterOffMedDefault <- otterOffMed/totalOffOtter * 100
  percentageOtterOffCoarseDefault <- otterOffCoarse/totalOffOtter * 100
  totalPercentageOffOtterDefault <- percentageOtterOffRockDefault + percentageOtterOffFineDefault + percentageOtterOffMedDefault + percentageOtterOffCoarseDefault
  
  lonMackInRock <- model$data$fleet.model$gear_habitat_activity$s0[3]
  lonMackInFine <- model$data$fleet.model$gear_habitat_activity$s1[3]
  lonMackInMed <- model$data$fleet.model$gear_habitat_activity$s2[3]
  lonMackInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[3]
  lonMackOffRock <- model$data$fleet.model$gear_habitat_activity$d0[3]
  lonMackOffFine <- model$data$fleet.model$gear_habitat_activity$d1[3]
  lonMackOffMed <- model$data$fleet.model$gear_habitat_activity$d2[3] 
  lonMackOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[3]
  # Getting Total Inshore/Offshore here
  totalInLonMack <- lonMackInRock + lonMackInFine + lonMackInMed + lonMackInCoarse
  totalOffLonMack <- lonMackOffRock + lonMackOffFine + lonMackOffMed + lonMackOffCoarse
  totalOverallLonMack <- totalInLonMack + totalOffLonMack
  percentageInLonMackDefault <- totalInLonMack/totalOverallLonMack * 100
  percentageOutLonMackDefault <- 100 - percentageInLonMackDefault
  # Now getting percentages Inshore
  percentageLonMackInRockDefault <- lonMackInRock/totalInLonMack * 100
  percentageLonMackInFineDefault <- lonMackInFine/totalInLonMack * 100
  percentageLonMackInMedDefault <- lonMackInMed/totalInLonMack * 100
  percentageLonMackInCoarseDefault <- lonMackInCoarse/totalInLonMack * 100
  totalPercentageInLonMackDefault <- percentageLonMackInRockDefault + percentageLonMackInFineDefault + percentageLonMackInMedDefault + percentageLonMackInCoarseDefault
  # Now getting percentages Offshore
  percentageLonMackOffRockDefault <- lonMackOffRock/totalOffLonMack * 100
  percentageLonMackOffFineDefault <- lonMackOffFine/totalOffLonMack * 100
  percentageLonMackOffMedDefault <- lonMackOffMed/totalOffLonMack * 100
  percentageLonMackOffCoarseDefault <- lonMackOffCoarse/totalOffLonMack * 100
  totalPercentageOffLonMackDefault <- percentageLonMackOffRockDefault + percentageLonMackOffFineDefault + percentageLonMackOffMedDefault + percentageLonMackOffCoarseDefault
  
  beamTrawlInRock <- model$data$fleet.model$gear_habitat_activity$s0[4]
  beamTrawlInFine <- model$data$fleet.model$gear_habitat_activity$s1[4]
  beamTrawlInMed <- model$data$fleet.model$gear_habitat_activity$s2[4]
  beamTrawlInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[4]
  beamTrawlOffRock <- model$data$fleet.model$gear_habitat_activity$d0[4]
  beamTrawlOffFine <- model$data$fleet.model$gear_habitat_activity$d1[4]
  beamTrawlOffMed <- model$data$fleet.model$gear_habitat_activity$d2[4] 
  beamTrawlOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[4]
  # Getting Total Inshore/Offshore here
  totalInBeamTrawl <- beamTrawlInRock + beamTrawlInFine + beamTrawlInMed + beamTrawlInCoarse
  totalOffBeamTrawl <- beamTrawlOffRock + beamTrawlOffFine + beamTrawlOffMed + beamTrawlOffCoarse
  totalOverallBeamTrawl <- totalInBeamTrawl + totalOffBeamTrawl
  percentageInBeamTrawlDefault <- totalInBeamTrawl/totalOverallBeamTrawl * 100
  percentageOutBeamTrawlDefault <- 100 - percentageInBeamTrawlDefault
  # Now getting percentages Inshore
  percentageBeamTrawlInRockDefault <- beamTrawlInRock/totalInBeamTrawl * 100
  percentageBeamTrawlInFineDefault <- beamTrawlInFine/totalInBeamTrawl * 100
  percentageBeamTrawlInMedDefault <- beamTrawlInMed/totalInBeamTrawl * 100
  percentageBeamTrawlInCoarseDefault <- beamTrawlInCoarse/totalInBeamTrawl * 100
  totalPercentageInBeamTrawlDefault <- percentageBeamTrawlInRockDefault + percentageBeamTrawlInFineDefault + percentageBeamTrawlInMedDefault + percentageBeamTrawlInCoarseDefault
  # Now getting percentages Offshore
  percentageBeamTrawlOffRockDefault <- beamTrawlOffRock/totalOffBeamTrawl * 100
  percentageBeamTrawlOffFineDefault <- beamTrawlOffFine/totalOffBeamTrawl * 100
  percentageBeamTrawlOffMedDefault <- beamTrawlOffMed/totalOffBeamTrawl * 100
  percentageBeamTrawlOffCoarseDefault <- beamTrawlOffCoarse/totalOffBeamTrawl * 100
  totalPercentageOffBeamTrawlDefault <- percentageBeamTrawlOffRockDefault + percentageBeamTrawlOffFineDefault + percentageBeamTrawlOffMedDefault + percentageBeamTrawlOffCoarseDefault
  
  demSeineInRock <- model$data$fleet.model$gear_habitat_activity$s0[5]
  demSeineInFine <- model$data$fleet.model$gear_habitat_activity$s1[5]
  demSeineInMed <- model$data$fleet.model$gear_habitat_activity$s2[5]
  demSeineInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[5]
  demSeineOffRock <- model$data$fleet.model$gear_habitat_activity$d0[5]
  demSeineOffFine <- model$data$fleet.model$gear_habitat_activity$d1[5]
  demSeineOffMed <- model$data$fleet.model$gear_habitat_activity$d2[5] 
  demSeineOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[5]
  # Getting Total Inshore/Offshore here
  totalInDemSeine <- demSeineInRock + demSeineInFine + demSeineInMed + demSeineInCoarse
  totalOffDemSeine <- demSeineOffRock + demSeineOffFine + demSeineOffMed + demSeineOffCoarse
  totalOverallDemSeine <- totalInDemSeine + totalOffDemSeine
  percentageInDemSeineDefault <- totalInDemSeine/totalOverallDemSeine * 100
  percentageOutDemSeineDefault <- 100 - percentageInDemSeineDefault
  # Now getting percentages Inshore
  percentageDemSeineInRockDefault <- demSeineInRock/totalInDemSeine * 100
  percentageDemSeineInFineDefault <- demSeineInFine/totalInDemSeine * 100
  percentageDemSeineInMedDefault <- demSeineInMed/totalInDemSeine * 100
  percentageDemSeineInCoarseDefault <- demSeineInCoarse/totalInDemSeine * 100
  totalPercentageInDemSeineDefault <- percentageDemSeineInRockDefault + percentageDemSeineInFineDefault + percentageDemSeineInMedDefault + percentageDemSeineInCoarseDefault
  # Now getting percentages Offshore
  percentageDemSeineOffRockDefault <- demSeineOffRock/totalOffDemSeine * 100
  percentageDemSeineOffFineDefault <- demSeineOffFine/totalOffDemSeine * 100
  percentageDemSeineOffMedDefault <- demSeineOffMed/totalOffDemSeine * 100
  percentageDemSeineOffCoarseDefault <- demSeineOffCoarse/totalOffDemSeine * 100
  totalPercentageOffDemSeineDefault <- percentageDemSeineOffRockDefault + percentageDemSeineOffFineDefault + percentageDemSeineOffMedDefault + percentageDemSeineOffCoarseDefault
  
  demOtterInRock <- model$data$fleet.model$gear_habitat_activity$s0[6]
  demOtterInFine <- model$data$fleet.model$gear_habitat_activity$s1[6]
  demOtterInMed <- model$data$fleet.model$gear_habitat_activity$s2[6]
  demOtterInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[6]
  demOtterOffRock <- model$data$fleet.model$gear_habitat_activity$d0[6]
  demOtterOffFine <- model$data$fleet.model$gear_habitat_activity$d1[6]
  demOtterOffMed <- model$data$fleet.model$gear_habitat_activity$d2[6] 
  demOtterOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[6]
  # Getting Total Inshore/Offshore here
  totalInDemOtter <- demOtterInRock + demOtterInFine + demOtterInMed + demOtterInCoarse
  totalOffDemOtter <- demOtterOffRock + demOtterOffFine + demOtterOffMed + demOtterOffCoarse
  totalOverallDemOtter <- totalInDemOtter + totalOffDemOtter
  percentageInDemOtterDefault <- totalInDemOtter/totalOverallDemOtter * 100
  percentageOutDemOtterDefault <- 100 - percentageInDemOtterDefault
  # Now getting percentages Inshore
  percentageDemOtterInRockDefault <- demOtterInRock/totalInDemOtter * 100
  percentageDemOtterInFineDefault <- demOtterInFine/totalInDemOtter * 100
  percentageDemOtterInMedDefault <- demOtterInMed/totalInDemOtter * 100
  percentageDemOtterInCoarseDefault <- demOtterInCoarse/totalInDemOtter * 100
  totalPercentageInDemOtterDefault <- percentageDemOtterInRockDefault + percentageDemOtterInFineDefault + percentageDemOtterInMedDefault + percentageDemOtterInCoarseDefault
  # Now getting percentages Offshore
  percentageDemOtterOffRockDefault <- demOtterOffRock/totalOffDemOtter * 100
  percentageDemOtterOffFineDefault <- demOtterOffFine/totalOffDemOtter * 100
  percentageDemOtterOffMedDefault <- demOtterOffMed/totalOffDemOtter * 100
  percentageDemOtterOffCoarseDefault <- demOtterOffCoarse/totalOffDemOtter * 100
  totalPercentageOffDemOtterDefault <- percentageDemOtterOffRockDefault + percentageDemOtterOffFineDefault + percentageDemOtterOffMedDefault + percentageDemOtterOffCoarseDefault
  
  gillNetInRock <- model$data$fleet.model$gear_habitat_activity$s0[7]
  gillNetInFine <- model$data$fleet.model$gear_habitat_activity$s1[7]
  gillNetInMed <- model$data$fleet.model$gear_habitat_activity$s2[7]
  gillNetInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[7]
  gillNetOffRock <- model$data$fleet.model$gear_habitat_activity$d0[7]
  gillNetOffFine <- model$data$fleet.model$gear_habitat_activity$d1[7]
  gillNetOffMed <- model$data$fleet.model$gear_habitat_activity$d2[7] 
  gillNetOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[7]
  # Getting Total Inshore/Offshore here
  totalInGillNet <- gillNetInRock + gillNetInFine + gillNetInMed + gillNetInCoarse
  totalOffGillNet <- gillNetOffRock + gillNetOffFine + gillNetOffMed + gillNetOffCoarse
  totalOverallGillNet <- totalInGillNet + totalOffGillNet
  percentageInGillNetDefault <- totalInGillNet/totalOverallGillNet * 100
  percentageOutGillNetDefault <- 100 - percentageInGillNetDefault
  # Now getting percentages Inshore
  percentageGillNetInRockDefault <- gillNetInRock/totalInGillNet * 100
  percentageGillNetInFineDefault <- gillNetInFine/totalInGillNet * 100
  percentageGillNetInMedDefault <- gillNetInMed/totalInGillNet * 100
  percentageGillNetInCoarseDefault <- gillNetInCoarse/totalInGillNet * 100
  totalPercentageInGillNetDefault <- percentageGillNetInRockDefault + percentageGillNetInFineDefault + percentageGillNetInMedDefault + percentageGillNetInCoarseDefault
  # Now getting percentages Offshore
  percentageGillNetOffRockDefault <- gillNetOffRock/totalOffGillNet * 100
  percentageGillNetOffFineDefault <- gillNetOffFine/totalOffGillNet * 100
  percentageGillNetOffMedDefault <- gillNetOffMed/totalOffGillNet * 100
  percentageGillNetOffCoarseDefault <- gillNetOffCoarse/totalOffGillNet * 100
  totalPercentageOffGillNetDefault <- percentageGillNetOffRockDefault + percentageGillNetOffFineDefault + percentageGillNetOffMedDefault + percentageGillNetOffCoarseDefault
  
  beamShrimpInRock <- model$data$fleet.model$gear_habitat_activity$s0[8]
  beamShrimpInFine <- model$data$fleet.model$gear_habitat_activity$s1[8]
  beamShrimpInMed <- model$data$fleet.model$gear_habitat_activity$s2[8]
  beamShrimpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[8]
  beamShrimpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[8]
  beamShrimpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[8]
  beamShrimpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[8] 
  beamShrimpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[8]
  # Getting Total Inshore/Offshore here
  totalInbeamShrimp <- beamShrimpInRock + beamShrimpInFine + beamShrimpInMed + beamShrimpInCoarse
  totalOffbeamShrimp <- beamShrimpOffRock + beamShrimpOffFine + beamShrimpOffMed + beamShrimpOffCoarse
  totalOverallBeamShrimp <- totalInbeamShrimp + totalOffbeamShrimp
  percentageInBeamShrimpDefault <- totalInbeamShrimp/totalOverallBeamShrimp * 100
  percentageOutBeamShrimpDefault <- 100 - percentageInBeamShrimpDefault
  # Now getting percentages Inshore
  percentageBeamShrimpInRockDefault <- beamShrimpInRock/totalInbeamShrimp * 100
  percentageBeamShrimpInFineDefault <- beamShrimpInFine/totalInbeamShrimp * 100
  percentageBeamShrimpInMedDefault <- beamShrimpInMed/totalInbeamShrimp * 100
  percentageBeamShrimpInCoarseDefault <- beamShrimpInCoarse/totalInbeamShrimp * 100
  totalPercentageInBeamShrimpDefault <- percentageBeamShrimpInRockDefault + percentageBeamShrimpInFineDefault + percentageBeamShrimpInMedDefault + percentageBeamShrimpInCoarseDefault
  # Now getting percentages Offshore
  percentageBeamShrimpOffRockDefault <- beamShrimpOffRock/totalOffbeamShrimp * 100
  percentageBeamShrimpOffFineDefault <- beamShrimpOffFine/totalOffbeamShrimp * 100
  percentageBeamShrimpOffMedDefault <- beamShrimpOffMed/totalOffbeamShrimp * 100
  percentageBeamShrimpOffCoarseDefault <- beamShrimpOffCoarse/totalOffbeamShrimp * 100
  totalPercentageOffBeamShrimpDefault <- percentageBeamShrimpOffRockDefault + percentageBeamShrimpOffFineDefault + percentageBeamShrimpOffMedDefault + percentageBeamShrimpOffCoarseDefault
  
  nephropsTR2InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
  nephropsTR2InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
  nephropsTR2InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
  nephropsTR2InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
  nephropsTR2OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
  nephropsTR2OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
  nephropsTR2OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9] 
  nephropsTR2OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
  # Getting Total Inshore/Offshore here
  totalInNephropsTR2 <- nephropsTR2InRock + nephropsTR2InFine + nephropsTR2InMed + nephropsTR2InCoarse
  totalOffNephropsTR2 <- nephropsTR2OffRock + nephropsTR2OffFine + nephropsTR2OffMed + nephropsTR2OffCoarse
  totalOverallNephropsTR2 <- totalInNephropsTR2 + totalOffNephropsTR2
  percentageInNephropsTR2Default <- totalInNephropsTR2/totalOverallNephropsTR2 * 100
  percentageOutNephropsTR2Default <- 100 - percentageInNephropsTR2Default
  # Now getting percentages Inshore
  percentageNephropsTR2InRockDefault <- nephropsTR2InRock/totalInNephropsTR2 * 100
  percentageNephropsTR2InFineDefault <- nephropsTR2InFine/totalInNephropsTR2 * 100
  percentageNephropsTR2InMedDefault <- nephropsTR2InMed/totalInNephropsTR2 * 100
  percentageNephropsTR2InCoarseDefault <- nephropsTR2InCoarse/totalInNephropsTR2 * 100
  totalPercentageInNephropsTR2Default <- percentageNephropsTR2InRockDefault + percentageNephropsTR2InFineDefault + percentageNephropsTR2InMedDefault + percentageNephropsTR2InCoarseDefault
  # Now getting percentages Offshore
  percentageNephropsTR2OffRockDefault <- nephropsTR2OffRock/totalOffNephropsTR2 * 100
  percentageNephropsTR2OffFineDefault <- nephropsTR2OffFine/totalOffNephropsTR2 * 100
  percentageNephropsTR2OffMedDefault <- nephropsTR2OffMed/totalOffNephropsTR2 * 100
  percentageNephropsTR2OffCoarseDefault <- nephropsTR2OffCoarse/totalOffNephropsTR2 * 100
  totalPercentageOffNephropsTR2Default <- percentageNephropsTR2OffRockDefault + percentageNephropsTR2OffFineDefault + percentageNephropsTR2OffMedDefault + percentageNephropsTR2OffCoarseDefault
  
  nephropsTR3InRock <- model$data$fleet.model$gear_habitat_activity$s0[9]
  nephropsTR3InFine <- model$data$fleet.model$gear_habitat_activity$s1[9]
  nephropsTR3InMed <- model$data$fleet.model$gear_habitat_activity$s2[9]
  nephropsTR3InCoarse <- model$data$fleet.model$gear_habitat_activity$s3[9]
  nephropsTR3OffRock <- model$data$fleet.model$gear_habitat_activity$d0[9]
  nephropsTR3OffFine <- model$data$fleet.model$gear_habitat_activity$d1[9]
  nephropsTR3OffMed <- model$data$fleet.model$gear_habitat_activity$d2[9] 
  nephropsTR3OffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[9]
  # Getting Total Inshore/Offshore here
  totalInNephropsTR3 <- nephropsTR3InRock + nephropsTR3InFine + nephropsTR3InMed + nephropsTR3InCoarse
  totalOffNephropsTR3 <- nephropsTR3OffRock + nephropsTR3OffFine + nephropsTR3OffMed + nephropsTR3OffCoarse
  totalOverallNephropsTR3 <- totalInNephropsTR3 + totalOffNephropsTR3
  percentageInNephropsTR3Default <- totalInNephropsTR3/totalOverallNephropsTR3 * 100
  percentageOutNephropsTR3Default <- 100 - percentageInNephropsTR3Default
  # Now getting percentages Inshore
  percentageNephropsTR3InRockDefault <- nephropsTR3InRock/totalInNephropsTR3 * 100
  percentageNephropsTR3InFineDefault <- nephropsTR3InFine/totalInNephropsTR3 * 100
  percentageNephropsTR3InMedDefault <- nephropsTR3InMed/totalInNephropsTR3 * 100
  percentageNephropsTR3InCoarseDefault <- nephropsTR3InCoarse/totalInNephropsTR3 * 100
  totalPercentageInNephropsTR3Default <- percentageNephropsTR3InRockDefault + percentageNephropsTR3InFineDefault + percentageNephropsTR3InMedDefault + percentageNephropsTR3InCoarseDefault
  # Now getting percentages Offshore
  percentageNephropsTR3OffRockDefault <- nephropsTR3OffRock/totalOffNephropsTR3 * 100
  percentageNephropsTR3OffFineDefault <- nephropsTR3OffFine/totalOffNephropsTR3 * 100
  percentageNephropsTR3OffMedDefault <- nephropsTR3OffMed/totalOffNephropsTR3 * 100
  percentageNephropsTR3OffCoarseDefault <- nephropsTR3OffCoarse/totalOffNephropsTR3 * 100
  totalPercentageOffNephropsTR3Default <- percentageNephropsTR3OffRockDefault + percentageNephropsTR3OffFineDefault + percentageNephropsTR3OffMedDefault + percentageNephropsTR3OffCoarseDefault
  
  creelsInRock <- model$data$fleet.model$gear_habitat_activity$s0[10]
  creelsInFine <- model$data$fleet.model$gear_habitat_activity$s1[10]
  creelsInMed <- model$data$fleet.model$gear_habitat_activity$s2[10]
  creelsInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[10]
  creelsOffRock <- model$data$fleet.model$gear_habitat_activity$d0[10]
  creelsOffFine <- model$data$fleet.model$gear_habitat_activity$d1[10]
  creelsOffMed <- model$data$fleet.model$gear_habitat_activity$d2[10] 
  creelsOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[10]
  # Getting Total Inshore/Offshore here
  totalInCreels <- creelsInRock + creelsInFine + creelsInMed + creelsInCoarse
  totalOffCreels <- creelsOffRock + creelsOffFine + creelsOffMed + creelsOffCoarse
  totalOverallCreels <- totalInCreels + totalOffCreels
  percentageInCreelsDefault <- totalInCreels/totalOverallCreels * 100
  percentageOutCreelsDefault <- 100 - percentageInCreelsDefault
  # Now getting percentages Inshore
  percentageCreelsInRockDefault <- creelsInRock/totalInCreels * 100
  percentageCreelsInFineDefault <- creelsInFine/totalInCreels * 100
  percentageCreelsInMedDefault <- creelsInMed/totalInCreels * 100
  percentageCreelsInCoarseDefault <- creelsInCoarse/totalInCreels * 100
  totalPercentageInCreelsDefault <- percentageCreelsInRockDefault + percentageCreelsInFineDefault + percentageCreelsInMedDefault + percentageCreelsInCoarseDefault
  # Now getting percentages Offshore
  percentageCreelsOffRockDefault <- creelsOffRock/totalOffCreels * 100
  percentageCreelsOffFineDefault <- creelsOffFine/totalOffCreels * 100
  percentageCreelsOffMedDefault <- creelsOffMed/totalOffCreels * 100
  percentageCreelsOffCoarseDefault <- creelsOffCoarse/totalOffCreels * 100
  totalPercentageOffCreelsDefault <- percentageCreelsOffRockDefault + percentageCreelsOffFineDefault + percentageCreelsOffMedDefault + percentageCreelsOffCoarseDefault
  
  molluscInRock <- model$data$fleet.model$gear_habitat_activity$s0[11]
  molluscInFine <- model$data$fleet.model$gear_habitat_activity$s1[11]
  molluscInMed <- model$data$fleet.model$gear_habitat_activity$s2[11]
  molluscInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[11]
  molluscOffRock <- model$data$fleet.model$gear_habitat_activity$d0[11]
  molluscOffFine <- model$data$fleet.model$gear_habitat_activity$d1[11]
  molluscOffMed <- model$data$fleet.model$gear_habitat_activity$d2[11] 
  molluscOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[11]
  # Getting Total Inshore/Offshore here
  totalInMollusc <- molluscInRock + molluscInFine + molluscInMed + molluscInCoarse
  totalOffMollusc <- molluscOffRock + molluscOffFine + molluscOffMed + molluscOffCoarse
  totalOverallMollusc <- totalInMollusc + totalOffMollusc
  percentageInMolluscDefault <- totalInMollusc/totalOverallMollusc * 100
  percentageOutMolluscDefault <- 100 - percentageInMolluscDefault
  # Now getting percentages Inshore
  percentageMolluscInRockDefault <- molluscInRock/totalInMollusc * 100
  percentageMolluscInFineDefault <- molluscInFine/totalInMollusc * 100
  percentageMolluscInMedDefault <- molluscInMed/totalInMollusc * 100
  percentageMolluscInCoarseDefault <- molluscInCoarse/totalInMollusc * 100
  totalPercentageInMolluscDefault <- percentageMolluscInRockDefault + percentageMolluscInFineDefault + percentageMolluscInMedDefault + percentageMolluscInCoarseDefault
  # Now getting percentages Offshore
  percentageMolluscOffRockDefault <- molluscOffRock/totalOffMollusc * 100
  percentageMolluscOffFineDefault <- molluscOffFine/totalOffMollusc * 100
  percentageMolluscOffMedDefault <- molluscOffMed/totalOffMollusc * 100
  percentageMolluscOffCoarseDefault <- molluscOffCoarse/totalOffMollusc * 100
  totalPercentageOffMolluscDefault <- percentageMolluscOffRockDefault + percentageMolluscOffFineDefault + percentageMolluscOffMedDefault + percentageMolluscOffCoarseDefault
  
  whalerInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
  whalerInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
  whalerInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
  whalerInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
  whalerOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
  whalerOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
  whalerOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12] 
  whalerOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
  # Getting Total Inshore/Offshore here
  totalInWhaler <- whalerInRock + whalerInFine + whalerInMed + whalerInCoarse
  totalOffWhaler <- whalerOffRock + whalerOffFine + whalerOffMed + whalerOffCoarse
  totalOverallWhaler <- totalInWhaler + totalOffWhaler
  percentageInWhalerDefault <- totalInWhaler/totalOverallWhaler * 100
  percentageOutWhalerDefault <- 100 - percentageInWhalerDefault
  # Now getting percentages Inshore
  if(totalInWhaler>0){
    percentageWhalerInRockDefault <- whalerInRock/totalInWhaler * 100
  } else {
    percentageWhalerInRockDefault <- 0
  }
  if(totalInWhaler>0){
    percentageWhalerInFineDefault <- whalerInFine/totalInWhaler * 100
  } else {
    percentageWhalerInFineDefault <- 0
  }
  if(totalInWhaler>0){
    percentageWhalerInMedDefault <- whalerInMed/totalInWhaler * 100
  } else {
    percentageWhalerInMedDefault <- 0
  }
  if(totalInWhaler>0){
    percentageWhalerInCoarseDefault <- whalerInCoarse/totalInWhaler * 100
  } else {
    percentageWhalerInCoarseDefault <- 0
  }
  totalPercentageInWhalerDefault <- percentageWhalerInRockDefault + percentageWhalerInFineDefault + percentageWhalerInMedDefault + percentageWhalerInCoarseDefault
  # Now getting percentages Offshore
  if(totalOffWhaler>0){
    percentageWhalerOffRockDefault <- whalerOffRock/totalOffWhaler * 100
  } else {
    percentageWhalerOffRockDefault <- 0
  }
  if(totalOffWhaler>0){
    percentageWhalerOffFineDefault <- whalerOffFine/totalOffWhaler * 100
  } else {
    percentageWhalerOffFineDefault <- 0
  }
  if(totalOffWhaler>0){
    percentageWhalerOffMedDefault <- whalerOffMed/totalOffWhaler * 100
  } else {
    percentageWhalerOffMedDefault <- 0
  }
  if(totalOffWhaler>0){
    percentageWhalerOffCoarseDefault <- whalerOffCoarse/totalOffWhaler * 100
  } else {
    percentageWhalerOffCoarseDefault <- 0
  }
  totalPercentageOffWhalerDefault <- percentageWhalerOffRockDefault + percentageWhalerOffFineDefault + percentageWhalerOffMedDefault + percentageWhalerOffCoarseDefault
  
  kelpInRock <- model$data$fleet.model$gear_habitat_activity$s0[12]
  kelpInFine <- model$data$fleet.model$gear_habitat_activity$s1[12]
  kelpInMed <- model$data$fleet.model$gear_habitat_activity$s2[12]
  kelpInCoarse <- model$data$fleet.model$gear_habitat_activity$s3[12]
  kelpOffRock <- model$data$fleet.model$gear_habitat_activity$d0[12]
  kelpOffFine <- model$data$fleet.model$gear_habitat_activity$d1[12]
  kelpOffMed <- model$data$fleet.model$gear_habitat_activity$d2[12] 
  kelpOffCoarse <- model$data$fleet.model$gear_habitat_activity$d3[12]
  # Getting Total Inshore/Offshore here
  totalInKelp <- kelpInRock + kelpInFine + kelpInMed + kelpInCoarse
  totalOffKelp <- kelpOffRock + kelpOffFine + kelpOffMed + kelpOffCoarse
  totalOverallKelp <- totalInKelp + totalOffKelp
  percentageInKelpDefault <- totalInKelp/totalOverallKelp * 100
  percentageOutKelpDefault <- 100 - percentageInKelpDefault
  # Now getting percentages Inshore
  if(totalInKelp>0){
    percentageKelpInRockDefault <- kelpInRock/totalInKelp * 100
  } else {
    percentageKelpInRockDefault <- 0
  }
  if(totalInKelp>0){
    percentageKelpInFineDefault <- kelpInFine/totalInKelp * 100
  } else {
    percentageKelpInFineDefault <- 0
  }
  if(totalInKelp>0){
    percentageKelpInMedDefault <- kelpInMed/totalInKelp * 100
  } else {
    percentageKelpInMedDefault <- 0
  }
  if(totalInKelp>0){
    percentageKelpInCoarseDefault <- kelpInCoarse/totalInKelp * 100
  } else {
    percentageKelpInCoarseDefault <- 0
  }
  totalPercentageInKelpDefault <- percentageKelpInRockDefault + percentageKelpInFineDefault + percentageKelpInMedDefault + percentageKelpInCoarseDefault
  # Now getting percentages Offshore
  if(totalOffKelp>0){
    percentageKelpOffRockDefault <- kelpOffRock/totalOffKelp * 100
  } else {
    percentageKelpOffRockDefault <- 0
  }
  if(totalOffKelp>0){
    percentageKelpOffFineDefault <- kelpOffFine/totalOffKelp * 100
  } else {
    percentageKelpOffFineDefault <- 0
  }
  if(totalOffKelp>0){
    percentageKelpOffMedDefault <- kelpOffMed/totalOffKelp * 100
  } else {
    percentageKelpOffMedDefault <- 0
  }
  if(totalOffKelp>0){
    percentageKelpOffCoarseDefault <- kelpOffCoarse/totalOffKelp * 100
  } else {
    percentageKelpOffCoarseDefault <- 0
  }
  totalPercentageOffKelpDefault <- percentageKelpOffRockDefault + percentageKelpOffFineDefault + percentageKelpOffMedDefault + percentageKelpOffCoarseDefault
  gearType <- input$selectedGearForHabitatDist
  if (is.null(input$selectedGearForHabitatDist)) gearType <- c("Pelagic_Trawl+Seine")
  switch(
    gearType,
    "Pelagic_Trawl+Seine" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInPelDefault, percentageOutPelDefault,percentagePelInRockDefault, percentagePelInFineDefault,percentagePelInMedDefault, percentagePelInCoarseDefault,percentagePelOffRockDefault, percentagePelOffFineDefault,  percentagePelOffMedDefault, percentagePelOffCoarseDefault),
    "Sandeel+sprat_trawl(Otter30-70mm+TR3)" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInSandeelDefault, percentageOutSandeelDefault,percentageSandeelInRockDefault, percentageSandeelInFineDefault,percentageSandeelInMedDefault, percentageSandeelInCoarseDefault,percentageSandeelOffRockDefault, percentageSandeelOffFineDefault,  percentageSandeelOffMedDefault, percentageSandeelOffCoarseDefault),
    "Otter30-70mm+TR3(sandeel+sprat)" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInOtterDefault, percentageOutOtterDefault,percentageOtterInRockDefault, percentageOtterInFineDefault,percentageOtterInMedDefault, percentageOtterInCoarseDefault,percentageOtterOffRockDefault, percentageOtterOffFineDefault,  percentageOtterOffMedDefault, percentageOtterOffCoarseDefault),
    "Longline_mackerel" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInLonMackDefault, percentageOutLonMackDefault,percentageLonMackInRockDefault, percentageLonMackInFineDefault,percentageLonMackInMedDefault, percentageLonMackInCoarseDefault,percentageLonMackOffRockDefault, percentageLonMackOffFineDefault,  percentageLonMackOffMedDefault, percentageLonMackOffCoarseDefault),
    "Beam_Trawl_BT1+BT2" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInBeamTrawlDefault, percentageOutBeamTrawlDefault,percentageBeamTrawlInRockDefault, percentageBeamTrawlInFineDefault,percentageBeamTrawlInMedDefault, percentageBeamTrawlInCoarseDefault,percentageBeamTrawlOffRockDefault, percentageBeamTrawlOffFineDefault,  percentageBeamTrawlOffMedDefault, percentageBeamTrawlOffCoarseDefault),
    "Demersal_Seine" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInDemSeineDefault, percentageOutDemSeineDefault,percentageDemSeineInRockDefault, percentageDemSeineInFineDefault,percentageDemSeineInMedDefault, percentageDemSeineInCoarseDefault,percentageDemSeineOffRockDefault, percentageDemSeineOffFineDefault,  percentageDemSeineOffMedDefault, percentageDemSeineOffCoarseDefault),
    "Demersal_Otter_Trawl_TR1" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInDemOtterDefault, percentageOutDemOtterDefault,percentageDemOtterInRockDefault, percentageDemOtterInFineDefault,percentageDemOtterInMedDefault, percentageDemOtterInCoarseDefault,percentageDemOtterOffRockDefault, percentageDemOtterOffFineDefault,  percentageDemOtterOffMedDefault, percentageDemOtterOffCoarseDefault),
    "Gill_Nets+Longline_demersal" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInGillNetDefault, percentageOutGillNetDefault,percentageGillNetInRockDefault, percentageGillNetInFineDefault,percentageGillNetInMedDefault, percentageGillNetInCoarseDefault,percentageGillNetOffRockDefault, percentageGillNetOffFineDefault,  percentageGillNetOffMedDefault, percentageGillNetOffCoarseDefault),
    "Beam_Trawl_shrimp" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInBeamShrimpDefault, percentageOutBeamShrimpDefault,percentageBeamShrimpInRockDefault, percentageBeamShrimpInFineDefault,percentageBeamShrimpInMedDefault, percentageBeamShrimpInCoarseDefault,percentageBeamShrimpOffRockDefault, percentageBeamShrimpOffFineDefault,  percentageBeamShrimpOffMedDefault, percentageBeamShrimpOffCoarseDefault),
    "Nephrops_Trawl_TR2" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInNephropsTR2Default, percentageOutNephropsTR2Default,percentageNephropsTR2InRockDefault, percentageNephropsTR2InFineDefault,percentageNephropsTR2InMedDefault, percentageNephropsTR2InCoarseDefault,percentageNephropsTR2OffRockDefault, percentageNephropsTR2OffFineDefault,  percentageNephropsTR2OffMedDefault, percentageNephropsTR2OffCoarseDefault),
    "Nephrops_Trawl_TR3" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInNephropsTR3Default, percentageOutNephropsTR3Default,percentageNephropsTR3InRockDefault, percentageNephropsTR3InFineDefault,percentageNephropsTR3InMedDefault, percentageNephropsTR3InCoarseDefault,percentageNephropsTR3OffRockDefault, percentageNephropsTR3OffFineDefault,  percentageNephropsTR3OffMedDefault, percentageNephropsTR3OffCoarseDefault),
    "Creels" =  createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInCreelsDefault, percentageOutCreelsDefault,percentageCreelsInRockDefault, percentageCreelsInFineDefault,percentageCreelsInMedDefault, percentageCreelsInCoarseDefault,percentageCreelsOffRockDefault, percentageCreelsOffFineDefault,  percentageCreelsOffMedDefault, percentageCreelsOffCoarseDefault),
    "Mollusc_Dredge" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInMolluscDefault, percentageOutMolluscDefault,percentageMolluscInRockDefault, percentageMolluscInFineDefault,percentageMolluscInMedDefault, percentageMolluscInCoarseDefault,percentageMolluscOffRockDefault, percentageMolluscOffFineDefault,  percentageMolluscOffMedDefault, percentageMolluscOffCoarseDefault),
    "Whaler" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInWhalerDefault, percentageOutWhalerDefault,percentageWhalerInRockDefault, percentageWhalerInFineDefault,percentageWhalerInMedDefault, percentageWhalerInCoarseDefault,percentageWhalerOffRockDefault, percentageWhalerOffFineDefault,  percentageWhalerOffMedDefault, percentageWhalerOffCoarseDefault),
    "KelpHarvester" = createGearDistributionPerHabitat(input$selectedlocation,gearType, jsCode, percentageInKelpDefault, percentageOutKelpDefault,percentageKelpInRockDefault, percentageKelpInFineDefault,percentageKelpInMedDefault, percentageKelpInCoarseDefault,percentageKelpOffRockDefault, percentageKelpOffFineDefault,  percentageKelpOffMedDefault, percentageKelpOffCoarseDefault)
  )
}