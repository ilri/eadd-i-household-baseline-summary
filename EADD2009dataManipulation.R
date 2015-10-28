################################################
## Script running on R 3.2.1, 64 bit
# Purpose: extracting productivity and household 
# differentiating factors related to dairy
# from EADD I data set
#
# Writen by: Simon Fraval 
# Date: 24/8/2015
################################################
library(RODBC)

##Connect to access database
#con <- odbcConnect("MS Access Database") http://sandymuspratt.blogspot.co.ke/2013/01/getting-access-data-into-r.html
dbPath <- "D:/sfraval/Documents/ILRI documents/AfricaHHdata/EADD/EADD Household survey database-August 2009.mdb"
channel <- odbcConnectAccess2007(dbPath)

##Import relevant tables
# <- sqlQuery( channel , paste ("select * from sec5_2cattle_heads"))
#sqlTables(channel, tableType = "TABLE")


breedingServices <- sqlFetch( channel , "AIProviderbreedServices")
animalHealth <- sqlFetch(channel, "Section F-F1 F2 F3 F4")
landUsage <- sqlFetch(channel, "Section B4-plot use")
feedFodder <- sqlFetch( channel , "E1E5Table")
fodderConservation <- sqlFetch(channel, "E7Table")
water <- sqlFetch(channel, "Section E-E1 E5 E7 E9 E10 E13")
concentrates <- sqlFetch(channel, "E8FeedConcentrates")
HHroster <- sqlFetch(channel, "section a3-household members")
HHhead <- sqlFetch(channel, "Sections a1 a2 a5 b1 b3 b5")
HHlabor <- sqlFetch(channel, "LivestockLabour")
#employLabor <- sqlFetch(channel, "EmployedLabours")
landAcres <- sqlFetch(channel, "land tenure - B3 land tenure")
livestockNumbers <- sqlFetch(channel, "Livestock Inventory")
cattleDeath <- sqlFetch(channel, "Section C5-cattle died")
recordsTagging <- sqlFetch(channel, "CattleC6_C9")
extensionServ <- sqlFetch(channel, "LivestockExtension")
credit <- sqlFetch(channel, "CreaditLoan")
incomeSource <- sqlFetch(channel, "Section I-I1 I3 I5 Enum")
#milkProd <- sqlFetch(channel, "MilkProduction")
milkProd <- sqlFetch(channel, "Section d4-cow details")
milkSaleCons <- sqlFetch(channel, "Section d2-milk usage")
increaseMilkLimitationsSales <- sqlFetch(channel, "Section D-D1 D5 D9 D10")
#feedDisease <- sqlFetch(channel, "Section G5-pest diseases")
feedQuality <- sqlFetch(channel, "G4FeedQuality")
feedInformation <- sqlFetch(channel, "G6Information")
cattlePurch <- sqlFetch(channel, "Section C3-cattle kept")
cattleSell <- sqlFetch(channel, "Section c4-cattle sold")

odbcCloseAll()


library(dplyr)
library(tidyr)
psum <- function(..., na.rm=FALSE) { 
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}

breedingServices$AI <- ifelse(breedingServices$E13ExpendtAI>0,1,0)
breedingServices$AIwho <- ifelse(breedingServices$E13AIDecision %in% c(3:7, 9:11), 4, 
                          ifelse(breedingServices$E13AIDecision==8,3, breedingServices$E13AIDecision))
breedingServices <- select(breedingServices, RespID, AI, AIwho) # Who decides: 1= man, 2= woman, 3= joint, 4=other


animalHealth$RespID <- animalHealth$HealthDetails_RespID
animalHealth$deworm <- animalHealth$F2AnthelUsed
animalHealth$dewormWho <- ifelse(animalHealth$F2AnthelDecisn %in% c(3:7, 9:11), 4, 
                          ifelse(animalHealth$F2AnthelDecisn==8,3, animalHealth$F2AnthelDecisn))
animalHealth$tick <- animalHealth$F2TickUsed
animalHealth$tickWho <- ifelse(animalHealth$F2TickDecisn %in% c(3:7, 9:11), 4, 
                                       ifelse(animalHealth$F2TickDecisn==8,3, animalHealth$F2TickDecisn))
animalHealth$vaccinate <- animalHealth$F2VaccinUsed
animalHealth$vaccinateWho <- ifelse(animalHealth$F2VaccinDecisn %in% c(3:7, 9:11), 4, 
                                     ifelse(animalHealth$F2VaccinDecisn==8,3, animalHealth$F2VaccinDecisn))

animalHealthInputs <- select(animalHealth, RespID, deworm, dewormWho, tick, tickWho, vaccinate, vaccinateWho, F3TPAcaracide, F3TPGrazingRestriction, F3TPHandPicking)


landUsage$napier <- ifelse(grepl(".*36", landUsage$B4ShortRain)==T | grepl(".*36", landUsage$B4LongRain)==T,1,0)
landUsage$foddertree <- ifelse(grepl(".*42", landUsage$B4ShortRain)==T | grepl(".*42", landUsage$B4LongRain)==T,1,0)
landUsage$lucerne <- ifelse(grepl(".*38", landUsage$B4ShortRain)==T | grepl(".*38", landUsage$B4LongRain)==T,1,0)
landUsage$desmodium <- ifelse(grepl(".*37", landUsage$B4ShortRain)==T | grepl(".*37", landUsage$B4LongRain)==T,1,0)
landUsage$RespID <- landUsage$PlotUse_RespID
landUsage$fodderWhoCultiv <- ifelse(landUsage$B4Cultivated==1, 3,
                             ifelse(landUsage$B4Cultivated==3, 1, 
                             ifelse(landUsage$B4Cultivated %in% c(0,4,5),4,2))) #now, 1 man, 2 woman, 3 joinly, 4 other.
landUsage <- select(landUsage, RespID, napier, foddertree, lucerne, desmodium, fodderWhoCultiv)
landUsage <- group_by(landUsage, RespID)
landUsageSummary <- summarise_each(landUsage, funs(max))


fodderConservation$silage <- ifelse(fodderConservation$E7TypeConsavtn2==1 | 
                                      fodderConservation$E7TypeConsavtn3==1 |
                                      fodderConservation$E7TypeConsavtn4==1 , 1,0)
#I think enumerators filled out the constraints in two ways: a 1 in each of the boxes corresponding to the reason and a number in each sequential box related to any of the reasons
fodderConservation$E7ReasonNot2 <- ifelse(fodderConservation$E7ReasonNot2==1, 2, fodderConservation$E7ReasonNot2)
fodderConservation$E7ReasonNot3 <- ifelse(fodderConservation$E7ReasonNot3==1, 3, fodderConservation$E7ReasonNot3)
fodderConservation$E7ReasonNot4 <- ifelse(fodderConservation$E7ReasonNot4==1, 4, fodderConservation$E7ReasonNot4)
fodderConservation$E7ReasonNot5 <- ifelse(fodderConservation$E7ReasonNot5==1, 5, fodderConservation$E7ReasonNot5)
fodderConservation$reasonNoConcservTechnicalInfo <- ifelse(fodderConservation$E7ReasonNot1==1, 1,0)
fodderConservation$reasonNoConcservLackFeed <- ifelse(fodderConservation$E7ReasonNot1==2 | fodderConservation$E7ReasonNot2 ==2| 
                                                    fodderConservation$E7ReasonNot3==2 | fodderConservation$E7ReasonNot4==2 | 
                                                    fodderConservation$E7ReasonNot5==2, 1, 0)
fodderConservation$reasonNoConcservExpense <- ifelse(fodderConservation$E7ReasonNot1==3 | fodderConservation$E7ReasonNot2==3| 
                                                    fodderConservation$E7ReasonNot3==3 | fodderConservation$E7ReasonNot4==3 | 
                                                    fodderConservation$E7ReasonNot5==3, 1, 0)
fodderConservation$reasonNoConcservEnoughFeed <- ifelse(fodderConservation$E7ReasonNot1Other %in% c("Enough grazing pasture", "Fed directly from the shamba", "Feed on the shamba", "Enough pasture", "Enough pastures", "Has enough feeds", "Fresh fodder quite available al over the year", "No necessity", "no need", "No need to conserve feed", "No problem with feeds", "no reason to do it", "Pasture always available", "They are usually available for all seasons") | fodderConservation$E7ReasonNot2Other %in% c("food is enough n animals grazes") | fodderConservation$E7ReasonNot4Other %in% c("Fodder always in grazing field", "natural pasture is enough for the cows", "Pasture always available", "Pasture everywhere in the fields"), 1,0)
fodderConservation$reasonNoConcservNotAwareBenifits <- ifelse(fodderConservation$E7ReasonNot1Other %in% c("ignorant", "Ignorance of benefits", "Not aware") | fodderConservation$E7ReasonNot4Other %in% c("Never thought of it"), 1,0)
fodderConservation$reasonNoConcservLabor <- ifelse(fodderConservation$E7ReasonNot1Other %in% c("Its labour intensive", "Lack of enough family labour", "Tiresome") | fodderConservation$E7ReasonNot4Other %in% c("Tiresome", "it is tiresome"), 1,0)
fodderConservation <- select(fodderConservation, RespID, silage, reasonNoConcservTechnicalInfo, reasonNoConcservLackFeed, reasonNoConcservExpense, reasonNoConcservEnoughFeed, reasonNoConcservNotAwareBenifits, reasonNoConcservLabor)
fodderConservation[is.na(fodderConservation)==T] <-0


concentratesFed <- subset(concentrates, E8AnimalType==3)
concentratesFed <- group_by(concentratesFed, RespID)
concentratesFed <- summarise(concentratesFed, concentratesKG=sum(E8AnimalDayQty, na.rm=T))
concentratesFed$concentratesKG[concentratesFed$concentratesKG>9] <- NA
concentrates <- subset(concentrates, E8ConcType %in% c(1,11))
concentrates$dummy <- 1
concentrates <- spread(concentrates, E8ConcType, dummy)
concentrates <- group_by(concentrates, RespID)
concentrates <- summarise(concentrates, dairyMeal=max(`1`, na.rm=T), homeRation=max(`11`, na.rm=T))
concentrates[is.na(concentrates)==T] <-0


HHhead$HHheadGender <- HHhead$HeadSex
HHhead$tractor <- HHhead$TractorOwn
HHhead$RespID <- HHhead$Identification_RespID
HHhead <- select(HHhead, CountryCode, VillageCode, RespID, HHheadGender, HeadAge, HYrsExp, Headschyears, Headreadwrite, tractor)


HHmembers <- group_by(HHroster, RespID)
HHmembers <- summarise(HHmembers, HHpop=length(RespID))

HHlabor$male <- ifelse(HHlabor$C21Who==1, HHlabor$C21Hours, 0)
HHlabor$female <- ifelse(HHlabor$C21Who==2, HHlabor$C21Hours, 0)
HHlabor$children <- ifelse(HHlabor$C21Who==3|HHlabor$C21Who==4, HHlabor$C21Hours, 0)
HHlabor$hired <- ifelse(HHlabor$C21Who==5, HHlabor$C21Hours, 0) 
HHlabor$feedingHrs <- ifelse(HHlabor$C21Activity==1 |HHlabor$C21Activity==2 |HHlabor$C21Activity==3,HHlabor$C21Hours,0) 
HHlabor$milkingHrs <- ifelse(HHlabor$C21Activity==4,HHlabor$C21Hours,0) 
HHlabor$marketingHrs <- ifelse(HHlabor$C21Activity==5,HHlabor$C21Hours,0)
HHlabor$cropHrs <- ifelse(HHlabor$C21Activity==7,HHlabor$C21Hours,0)
HHlabor$feedingHrsMan <- ifelse(HHlabor$C21Who==1 & (HHlabor$C21Activity==1 |HHlabor$C21Activity==2 |HHlabor$C21Activity==3),HHlabor$C21Hours,0)
HHlabor$feedingHrsWoman <- ifelse(HHlabor$C21Who==2 & (HHlabor$C21Activity==1 |HHlabor$C21Activity==2 |HHlabor$C21Activity==3),HHlabor$C21Hours,0)
HHlabor$milkingHrsMan <- ifelse(HHlabor$C21Who==1 & (HHlabor$C21Activity==4),HHlabor$C21Hours,0)
HHlabor$milkingHrsWoman <- ifelse(HHlabor$C21Who==2 & (HHlabor$C21Activity==4),HHlabor$C21Hours,0)
HHlabor$marketingHrsMan <- ifelse(HHlabor$C21Who==1 & (HHlabor$C21Activity==5),HHlabor$C21Hours,0)
HHlabor$marketingHrsWoman <- ifelse(HHlabor$C21Who==2 & (HHlabor$C21Activity==5),HHlabor$C21Hours,0)
#HHlabor$cropHrsMan <- ifelse(HHlabor$C21Who==1 & (HHlabor$C21Activity==7),HHlabor$C21Hours,0) #No data
#HHlabor$cropHrsWoman <- ifelse(HHlabor$C21Who==2 & (HHlabor$C21Activity==7),HHlabor$C21Hours,0)

HHlabor <- group_by(HHlabor, RespID)
HHlaborTotalhrs <- summarise(HHlabor, agHrs=(sum(C21Hours, na.rm=T)), laborHrs=(sum(hired, na.rm=T)), 
                  feedingHrs=(sum(feedingHrs, na.rm=T)), feedingHrsMan=sum(feedingHrsMan, na.rm=T), 
                  feedingHrsWoman=sum(feedingHrsWoman, na.rm=T), milkingHrsMan=sum(milkingHrsMan, na.rm=T), 
                  milkingHrsWoman=sum(milkingHrsWoman, na.rm=T), marketingHrsMan=sum(marketingHrsMan, na.rm=T), marketingHrsWoman=sum(marketingHrsWoman, na.rm=T))
HHlaborTotalhrs$portionLabor <- HHlaborTotalhrs$laborHrs/HHlaborTotalhrs$agHrs
HHlaborTotalhrs$portionFeedingMan <- HHlaborTotalhrs$feedingHrsMan/ psum(HHlaborTotalhrs$feedingHrsMan, HHlaborTotalhrs$feedingHrsWoman, na.rm=T) 
HHlaborTotalhrs$portionmilkingMan <- HHlaborTotalhrs$milkingHrsMan/ psum(HHlaborTotalhrs$milkingHrsMan, HHlaborTotalhrs$milkingHrsWoman, na.rm=T) 
HHlaborTotalhrs$portionMarketingMan <- HHlaborTotalhrs$marketingHrsMan/ psum(HHlaborTotalhrs$marketingHrsMan, HHlaborTotalhrs$marketingHrsWoman, na.rm=T) 


landAcres$percRented <- landAcres$B3LandRentIn/landAcres$B3TotalAcres
landAcres$totalAcres <- ifelse(landAcres$B3LandSizeUnit==2, landAcres$B3TotalAcres/0.404, landAcres$B3TotalAcres)
landAcres <- select(landAcres, RespID, totalAcres, percRented, B3LandRentIn)

livestockNumbers$TLUruminant <-  psum((livestockNumbers$C1LocCattleNum*1), 
                                      (livestockNumbers$C1crosCattleNum*1.29), 
                                      (livestockNumbers$C1ExoCattleNum*1.29),
                                      (livestockNumbers$C1LocGoatsNum*0.12), 
                                      (livestockNumbers$C1SheepNum*0.15), na.rm=T)
livestockNumbers$percLocalCattle <- ifelse(livestockNumbers$C1LocCattleNum>0, livestockNumbers$C1LocCattleNum / psum(livestockNumbers$C1LocCattleNum, livestockNumbers$C1crosCattleNum, livestockNumbers$C1ExoCattleNum, na.rm=T),0)
livestockNumbers <- select(livestockNumbers, RespID, TLUruminant, percLocalCattle)
livestockNumbers[is.na(livestockNumbers)] <- 0


cattleDeath$matureCattleDeaths <- ifelse(cattleDeath$C5CattleType %in% c(1,2,4), cattleDeath$C5CattleDied, 0)
cattleDeath$immatureCattleDeaths <- ifelse(cattleDeath$C5CattleType %in% c(3,5,6,7), cattleDeath$C5CattleDied, 0)
cattleDeath$RespID <- cattleDeath$CattleDied_RespID
cattleDeath <- group_by(cattleDeath, RespID)
cattleDeathSummary <- summarise(cattleDeath, matureCattleDeaths=sum(matureCattleDeaths, na.rm=T), immatureCattleDeaths=sum(immatureCattleDeaths, na.rm=T))
#!Cattle deaths may be binary for each cattle type

water$RespID <- water$Identification_RespID
water <- select(water, RespID, E10WaterAnimal, E1ShortgeFeed)

extensionServ$lstkExtensionUsed <- ifelse(extensionServ$F4VisitsNum>0| extensionServ$F4NGOsNum>0 | extensionServ$F4PractnNum>0 | extensionServ$F4CoopNum>0, 1,0)
extensionServ <- select(extensionServ, RespID, lstkExtensionUsed)


incomeCat <- select(incomeSource, Identification_RespID, I5AlIncomEstmt, I4DairyIncom)
incomeCat$RespID <- incomeCat$Identification_RespID
incomeCat$incomeCat <- incomeCat$I5AlIncomEstmt
incomeCat$dairyIncome1 <- ifelse(incomeCat$I4DairyIncom==1, 1,0)
incomeCat <- select(incomeCat, RespID, incomeCat, dairyIncome1)
#!Add % controlled joinly - but can't find this variable

credit$creditUsedDairy <- credit$C10LoanForDairy
credit$creditDecide <- credit$C10Sex
credit <- select(credit, RespID, creditUsedDairy, creditDecide)

milkProd$D4Age[milkProd$D4Age==0 | milkProd$D4Age==1] <-  NA
milkProd$D4Age1stCalving[milkProd$D4Age1stCalving==0] <-  NA
milkProd$D4CalvingIntval[milkProd$D4CalvingIntval==0 | milkProd$D4CalvingIntval==3] <-  NA
milkProd$D4MilkAtCalving[milkProd$D4MilkAtCalving==0] <-  NA
milkProd$RespID <- milkProd$D4Table_RespID
milkProd$AFCexotic <- ifelse(milkProd$D4Breed %in% 1:8, milkProd$D4Age1stCalving, 0)
milkProd$AFClocal <- ifelse(milkProd$D4Breed %in% 9:18, milkProd$D4Age1stCalving, 0)
milkProd$calvingIntExotic <- ifelse(milkProd$D4Breed %in% 1:8, milkProd$D4CalvingIntval, 0)
milkProd$calvingIntLocal <- ifelse(milkProd$D4Breed %in% 9:18, milkProd$D4CalvingIntval, 0)
milkProd$milkCalvingExotic <- ifelse(milkProd$D4Breed %in% 1:8, milkProd$D4MilkAtCalving, 0)
milkProd$milkCalvingLocal <- ifelse(milkProd$D4Breed %in% 9:18, milkProd$D4MilkAtCalving, 0)
milkProd$ageCattleMean <- milkProd$D4Age
milkProd <- group_by(milkProd, RespID)
milkProdSummary <- summarise_each(milkProd, funs(mean))
milkProdSummary <- select(milkProdSummary, RespID, ageCattleMean, AFCexotic, AFClocal, calvingIntExotic, calvingIntLocal, milkCalvingExotic, milkCalvingLocal)

milkSaleCons$consumedYest <- psum(milkSaleCons$D2QtyConsumedMorning, milkSaleCons$D2QtyConsumedEvening)
milkSaleCons$soldYest <- psum(milkSaleCons$D2QtySold, milkSaleCons$D2QtyPerDay)
milkSaleCons$RespID <- milkSaleCons$MilkUsage_RespID
milkSaleCons <- group_by(milkSaleCons, RespID)
milkSaleConsSummary <- summarise(milkSaleCons, milkConsumedYest=sum(consumedYest, na.rm=T), milkSoldYest=sum(soldYest, na.rm=T))
milkSaleConsSummary$milkYestPercCons <- milkSaleConsSummary$milkConsumedYest /psum(milkSaleConsSummary$milkConsumedYest, milkSaleConsSummary$milkSoldYest, na.rm=T)
milkSaleConsSummary$milkYestPercSold <- milkSaleConsSummary$milkSoldYest /psum(milkSaleConsSummary$milkConsumedYest, milkSaleConsSummary$milkSoldYest, na.rm=T)


feedFodder <- select(feedFodder, RespID, E3GrowFodder, E3HaveFodder, E3NoFodNoCows, 
              E3NoFodNoLand, E3NoFodNoSeed, E3NoFodNoMilk, E3ReasonNoFodOther, E3WhoRespsible, E4FodLegumes, 
              E4ConstraintsLackTech, E4ConstraintsLackNoMat, E4ConstraintsCost, E4ConstraintsNoLand,
              E4ConstraintsLackLabor, E4ConstraintsNoBen, E4ConstraintsNoInterest, E4ConstraintsDontKnow, E4ConstraintsOtherName)
#Very few observations providing source of information.


recordsTagging <- select(recordsTagging, RespID, C8KeepRecords, C8_1, C8_1, C8_2, C8_3, C8_4, C8_5, C8_6, C8_7, C9_1, C9_2, C9_3, C9_4, C9_5)

increaseMilkLimitationsSales$RespID <- increaseMilkLimitationsSales$Identification_RespID
increaseMilkLimitationsSales$ProdConstrGeneticLimit <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==1|increaseMilkLimitationsSales$D1SecConstraint==1,1,0)
increaseMilkLimitationsSales$prodConstrCredit <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==2|increaseMilkLimitationsSales$D1MainConstraint==3|increaseMilkLimitationsSales$D1MainConstraintOther=="Lack of collateral" | increaseMilkLimitationsSales$D1SecConstraint==2|increaseMilkLimitationsSales$D1SecConstraint==3 | increaseMilkLimitationsSales$D1SecConstraintOther=="Lack of collateral", 1, 0)
increaseMilkLimitationsSales$prodConstrMilkMarket <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==4 | increaseMilkLimitationsSales$D1MainConstraint==5 | increaseMilkLimitationsSales$D1MainConstraintOther == "milk marketing" | increaseMilkLimitationsSales$D1SecConstraint ==4 | increaseMilkLimitationsSales$D1SecConstraint ==5 | increaseMilkLimitationsSales$D1SecConstraintOther == "Lack of consistent market", 1,0)
increaseMilkLimitationsSales$prodConstrLabor <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==6|increaseMilkLimitationsSales$D1SecConstraint==6,1,0)
increaseMilkLimitationsSales$prodConstrFeed <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==7| increaseMilkLimitationsSales$D1MainConstraintOther =="Limited land" |increaseMilkLimitationsSales$D1SecConstraint==7 | increaseMilkLimitationsSales$D1SecConstraintOther =="lack of enough land" | increaseMilkLimitationsSales$D1SecConstraintOther=="Small grazing land",1,0)
increaseMilkLimitationsSales$prodConstrFeedPurchCost <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==8|increaseMilkLimitationsSales$D1SecConstraint==8,1,0)
increaseMilkLimitationsSales$prodConstrCowHealth <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==9|increaseMilkLimitationsSales$D1SecConstraint==9,1,0)
increaseMilkLimitationsSales$prodConstrNoBuyer <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==10|increaseMilkLimitationsSales$D1SecConstraint==10,1,0)
increaseMilkLimitationsSales$prodConstrWater <- ifelse(increaseMilkLimitationsSales$D1MainConstraint==11|increaseMilkLimitationsSales$D1SecConstraint==11,1,0)
increaseMilkLimitationsSales$prodConstrTheft <- ifelse(increaseMilkLimitationsSales$D1MainConstraintOther=="Fear of Theft of livestock", 1,0)

increaseMilkLimitationsSales <- select(increaseMilkLimitationsSales, RespID, ProdConstrGeneticLimit, prodConstrCredit, prodConstrMilkMarket, prodConstrLabor, prodConstrFeed, prodConstrFeedPurchCost, prodConstrCowHealth, prodConstrNoBuyer, prodConstrWater, prodConstrTheft, D5Oct07Qty,D5Nov07Qty,D5Dec07Qty,D5Jan08Qty,D5Feb08Qty,D5Mar08Qty,D5Apr08Qty,D5May08Qty,D5Jun08Qty,D5July08Qty,D5Aug08Qty,D5Sept08Qty,D5Oct08Qty,D5Nov08Qty,D5Dec08Qty,D5Jan09Qty,D5Feb09Qty,D5Mar09Qty,D5Apr09Qty,D5May09Qty,D5Jun09Qty)
increaseMilkLimitationsSales[increaseMilkLimitationsSales>5000] <-NA

cattlePurch$RespID <- cattlePurch$Identification_RespID
cattlePurchSummary <- group_by(cattlePurch, RespID)
cattlePurchSummary <- summarise(cattlePurchSummary, cattlePuchDeci=max(C3DecPurchase, na.rm=T))

cattleSell$RespID <- cattleSell$Identification_RespID
cattleSellSummary <- group_by(cattleSell, RespID)
cattleSellSummary <- summarise(cattleSellSummary, cattleSellDeci=max(C4DecBought, na.rm=T))

feedQuality <- select(feedQuality, RespID:G4NotSure)
#Only 3 farmers with forage disease instances. Recent Napier grass study is more informative.

feedInformation$infofarmer <- ifelse(feedInformation$G6Access1==1 | feedInformation$G6Access1==3 | 
                                   feedInformation$G6Access2==1 | feedInformation$G6Access2==3 |
                                   feedInformation$G6Access3==1 | feedInformation$G6Access3==3 |
                                   feedInformation$G6Access4==1 | feedInformation$G6Access4==3, 1,0)
feedInformation$infoextensionBreif <- ifelse(feedInformation$G6Access1==2 |  
                                           feedInformation$G6Access2==2 | 
                                           feedInformation$G6Access3==2 | 
                                           feedInformation$G6Access4==2 , 1,0)
feedInformation$infoextension <- ifelse(feedInformation$G6Access1==4 |  
                                      feedInformation$G6Access2==4 | 
                                      feedInformation$G6Access3==4 | 
                                      feedInformation$G6Access4==4 , 1,0)
feedInformation$infoNGO <- ifelse(feedInformation$G6Access1==5 |  
                                feedInformation$G6Access2==5 | 
                                feedInformation$G6Access3==5 | 
                                feedInformation$G6Access4==5 , 1,0)
feedInformation$infomedia <- ifelse(feedInformation$G6Access1==6 |  
                                  feedInformation$G6Access2==6 | 
                                  feedInformation$G6Access3==6 | 
                                  feedInformation$G6Access4==6 |
                                  feedInformation$G6Access1Other=="press"|
                                  feedInformation$G6Access1Other=="media", 1,0)
feedInformation$infofieldDay <- ifelse(feedInformation$G6Access1==7 |  
                                     feedInformation$G6Access2==7 | 
                                     feedInformation$G6Access3==7 | 
                                     feedInformation$G6Access4==7 | feedInformation$G6Access1Other=="field  days" , 1,0)
feedInformation$infoworkshop <- ifelse(feedInformation$G6Access1==8 |  
                                     feedInformation$G6Access2==8 | 
                                     feedInformation$G6Access3==8 | 
                                     feedInformation$G6Access4==8 , 1,0)
feedInformation$infochurch_mosque <- ifelse(feedInformation$G6Access1==9 |  
                                          feedInformation$G6Access2==9 | 
                                          feedInformation$G6Access3==9 | 
                                          feedInformation$G6Access4==9 , 1,0)
feedInformation$infoposter <- ifelse(feedInformation$G6Access1==10 |  
                                          feedInformation$G6Access2==10 | 
                                          feedInformation$G6Access3==10 | 
                                          feedInformation$G6Access4==10 , 1,0)
feedInformation$infoAgShow <- ifelse(feedInformation$G6Access3Other=="Agricultural shows", 1,0)
feedInformation$tmp <- 1
feedInformation <- feedInformation[!(feedInformation$G6TypeInfo==0 | feedInformation$G6TypeInfo==13 | is.na(feedInformation$G6TypeInfo)==T),]
feedInformation <- group_by(feedInformation, G6TypeInfo)
feedInfoSummary <- summarise(feedInformation, farmersAccessing=sum(tmp, na.rm=T), infoworkshop=sum(infoworkshop, na.rm=T),
                             infofieldDay=sum(infofieldDay, na.rm=T), infomedia=sum(infomedia, na.rm=T), 
                             infoNGO=sum(infoNGO, na.rm=T), infoextension=sum(infoextension, na.rm=T), 
                             infofarmer=sum(infofarmer, na.rm=T), infoposter=sum(infoposter, na.rm=T), 
                             infochurch_mosque=sum(infochurch_mosque, na.rm=T), infoAgShow=sum(infoAgShow, na.rm=T))
feedInfoSummary$Other <- psum(feedInfoSummary$infoAgShow, feedInfoSummary$infoposter, feedInfoSummary$infochurch_mosque, na.rm=T)
feedInfoSummary <- select(feedInfoSummary, -infoAgShow, -infoposter, -infochurch_mosque)
feedInfoSummary$G6TypeInfo <- c("Concentrate  feeding", "Fodder and forage feeding", "Grazing management", "Fodder establishment", "Fodder harvesting & processing", "Fodder conservation", "Feeds ration formulation",
                                     "Calf nutrition", "Cattle management", "Cattle housing", "Cattle breeding", "Disease management") 

colnames(feedInfoSummary) <- c("Type.of.information",  "Farmers.accessing", "Workshop", "Field day", "Media", "NGO", "Extension", "Other farmer", "Other")

EADD2009Comb <- merge(breedingServices, animalHealthInputs, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, landUsageSummary, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, fodderConservation, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, feedFodder, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, concentratesFed, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, concentrates, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, HHhead, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, HHmembers, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, HHlaborTotalhrs, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, landAcres, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, livestockNumbers, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, cattleDeathSummary, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, water, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, extensionServ, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, incomeCat, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, credit, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, milkProdSummary, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, milkSaleConsSummary, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, recordsTagging, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, increaseMilkLimitationsSales, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, feedQuality, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, cattlePurchSummary, by="RespID", all=T)
EADD2009Comb <- merge(EADD2009Comb, cattleSellSummary, by="RespID", all=T)

EADD2009Comb <- subset(EADD2009Comb, is.na(RespID)==F & RespID != 377)
EADD2009Comb <- subset(EADD2009Comb, CountryCode==3 & VillageCode %in% c(1, 7)) #Subset Kenya, Kabiyet and Kaptumo. Could include others. 2= Metkei,  5= Soy
EADD2009Comb <- subset(EADD2009Comb, TLUruminant>0) #At least 1 ruminant animal

rm(list=setdiff(ls(), c("kaliCombined","EADD2009Comb", "feedInfoSummary")))

write.csv(EADD2009Comb, "EADD2009summarySet.csv")
write.csv(feedInfoSummary, "EADD2009dairyLearning.csv")