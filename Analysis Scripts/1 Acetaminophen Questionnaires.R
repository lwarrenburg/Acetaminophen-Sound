setwd('/Users/home/Desktop/Research/Tylenol/Analysis')
library(reshape2)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)
library(data.table)
library("RColorBrewer")

##############################################################################################
#### Questionnaires ####
##############################################################################################
questionnaires<-read.csv("Baseline Questionnaires.csv", header = T)
ip<-questionnaires[,c(4,18:25)]
questionnaires<-questionnaires[,-c(1:3, 5:17)]

#### Fix Column Names ####
colnames(ip)<-c("IP", "ID", "MedsEffectiveness", "LastTimeTookMeds", "PreferredMeds", "FrequencyTakeMeds",
                "Politics", "PoliticalParty", "FamilyMoney")

colnames(questionnaires)<-c(# Participant ID
                            "IP", "ID", 
                            
                            # Demographics/Tylenol Behaviors
                            "MedsEffectiveness", "LastTimeTookMeds", "PreferredMeds", "FrequencyTakeMeds",
                            "Politics", "PoliticalParty",
                            "FamilyMoney", "WealthyNeighborhood", "RelativeWealth",
                            "CurrentWealth", "CurrentMoneyWorries", "FutureWealth",
                            
                            # Recent Health Behaviors
                            "Height", "Weight", "SleepHours", "WhenLastAte", "HowMuchLastEat", "SleepQuality",
                            "Sneeze", "RunnyNose", "Congestion", "SoreThroat", "Cough", "Pain", "Headache", "Chilliness",
                            "ExerciseTodayYN", "MinExercise", "WhenExercise", "ExerciseFrequency", "TypicalExerciseLength",
                            "WhenCaffeine", "GeneralHealth", "FeelSickRecently1", "FeelSickRecently2", "FeelSickRecently3",
                            "WhenLastSick", "SeeDoctorRecentlyYN", "NumberTimesSeeDoctor", "TakeMedsRecentlyYN", "MedsInfo",
                            "BirthControlYN", "MarijuanaFrequency", "WhenMarijuana", "CigaretteFrequency", "WhenCigarette",
                            "WhenAlcohol", "AlcoholFrequency", "NumberAlcohol", "HighAlcoholFrequency",
                            "ArthritisYN", "ImmuneDisordersYN", "EndocrineDisordersYN", "DiabeticYN", 
                            "Gender", "YearBorn", "Age", "YearUniversity", "Race", 
                            
                            # Need to Belong
                            "IfOtherPeopleDoNotAcceptMe", "ITryNotToDoThingsAvoid", "ISeldomWorryAboutPeopleCare", 
                            "INeedToFeelPeopleTurnTo", "IWantPeopleToAcceptMe", "IDoNotLikeBeingAlone",
                            "BeingApartFromFriends", "IHaveAStrongNeedToBelong", "ItBothersMeWhenNotIncluded", 
                            "MyFeelingsAreEasilyHurt",
                            
                            # Nostalgia
                            "HowValuableIsNostalgia", "HowImportantNostalgia", "HowSignificantNostalgia",
                            "HowProneNostalgia", "HowOftenExperienceNostalgia", "GeneralBringToMindNostalgia",
                            "SpecificBringToMindNostalgia",
                            
                            # Early Family Environment Questionnaire
                            "HowOftenFeelLoved", "HowOftenParentSwear", "HowOftenPhysicalAffection", "HowOftenPush",
                            "HowOftenViolent", "HowOftenParentsArgue", "HowOftenYouParentsArgue", "HowOftenSiblingsParentsArgue",
                            "HowOftenYouSiblingsArgue", "HouseChaotic", "LiveWithDrinkerDrugs", "HouseOrganized", "WereYouNeglected",
                            
                            # PANAS
                            "Interested", "Distressed", "Excited", "Upset", "Strong", "Guilty", "Scared", "Hostile", 
                            "Enthusiastic", "Proud", "Irritable", "Alert", "Ashamed", "Inspired", "Nervous", 
                            "Determined", "Attentive", "Jittery", "Active", "Afraid",
                            
                            # Ollen
                            "AgeStartMusic", "YearsPrivateLessons", "YearsDailyPractice", "CurrentPractice", 
                            "MusicCoursesYN", "HowMuchCoursework", "ExperienceComposing", "NumberLiveConcerts", "Title",
                            
                            # STOMP-R
                            "Alternative", "Bluegrass", "Blues", "Classical", "Country", "Dance/Electronica", 
                            "Folk", "Funk", "Gospel", "HeavyMetal", "World", "Jazz", "NewAge", "Oldies",
                            "Opera", "Pop", "Punk", "Rap/HipHop", "Reggae", "Religious", "Rock", "Soul/RB", "Soundtracks",
                            
                            
                            # IRI
                            "DaydreamRegularly", "OftenHaveTenderFeelings", "DifficultToSeeOtherPOV", "SometimesDoNotFeelSorry",
                            "InvolvedWithCharacterFeelings", "EmergencyApprehensive", "ObjectiveWhenWatchMovie", 
                            "LookAtEverybodysSide", "WhenSeeAdvantageFeelProtective", "SometimesFeelHelpless",
                            "SometimesUnderstandFriends", "ExtremelyInvolvedBookRare", "SeeSomeoneHurtCalm",
                            "OtherPeopleMisfortunesDoNotDisturb", "IfIAmRightDoNotListen", "AfterSeePlayFeelCharacter",
                            "TenseEmotionalSituationScary", "SeeUnfairDoNotFeelPity", "EffectiveWithEmergencies",
                            "OftenTouchedByThings", "BelieveTwoSidesToQuestion", "DescribeMyselfSoftHearted",
                            "WhenWatchMoviePutMyselfCharacter", "LoseControlEmergencies", "WhenUpsetPutInHisShoes",
                            "WhenReadStoryImagineEvents", "WhenSeeSomeoneHelpIGoPieces", "BeforeCriticizingIImagine",
                            
                            # Big Five
                            "Talkative", "FindFaultOthers", "DoesThoroughJob", "Depressed", "OriginalNewIdeas",
                            "Reserved", "HelpfulUnselfish", "Careless", "Relaxed", "Curious", "FullOfEnergy",
                            "StartsQuarrels", "ReliableWorker", "Tense", "Ingenious", "GeneratesEnthusiasm",
                            "ForgivingNature", "Disorganized", "WorriesALot", "ActiveImagination", "Quiet",
                            "Trusting", "Lazy", "EmotionallyStable", "Inventive", "Assertive", "ColdAloof",
                            "Perseveres", "Moody", "ValuesArt", "ShyInhibited", "ConsiderateKind", 
                            "Efficient", "StayCalm", "PreferRoutine", "Outgoing", "SometimesRude", 
                            "FollowsThroughOnPlans", "NervousEasily", "LikesToReflect", "FewArtisticInterests",
                            "LikesToCooperate", "EasilyDistracted", "SophisticatedInArt",
                            
                            # AIMS
                            "MoveHandConducting", "ForgetWhereIAm", "IAmOneWithMusic", "CaughtUpDoNotNotice",
                            "WhenNobodyUnderstandsMusic", "WillStopEverything", "CanImagineSongVividly",
                            "LoseTrainOfThought", "MyMindCanUnderstandWorld", "UnderstandSongwriterIntentions",
                            "ChangeAnySoundToMusic", "StopWalkingToListen", "SoInvolvedIForgetMyself",
                            "FeelCreative", "CompletelyImmersed", "MindAlteringMusic", "FeelMoreConnected", 
                            "SoundsHaveColors", "AsMuchTimeAsPossible", "FeelExperienceChild", "SongWrittenForMe",
                            "MakeMovementsWithMusic", "FindPatterns", "LoseSenseOfTime", "ConsiderMusicBeforeActivity", 
                            "SpeakingVoiceFascinating", "StepOutsideUsualSelf", "ImagineMusiciansPlaying",
                            "LiftedIntoAir", "TuneOutEverythingElse", "VividImages", "CloseEyesToFocus",
                            "TimesNothingExceptMusic", "PartOfSomethingBigger",
                            
                            # End
                            "QuestionsForExperimenter"
                            )



#### Check Names ####
check<-t(questionnaires[1,])
questionnaires<-questionnaires[-c(1:2),]
ip<-ip[-c(1:2),]
colnames<-as.data.frame(colnames(questionnaires))

# delete accidental duplicates
  # 117: 18d: 140.254.98.239
  # 144: 144: 140.254.98.239
  # 193: 193: 140.254.98.97
  # 91: 24e: 140.254.98.143
  # 92: 25e: 140.254.98.46

remove<-questionnaires[! questionnaires$ID %in% c("18d", "  193", "24e", "25e"),]
subj144<-remove[remove$ID == "144" & remove$MedsEffectiveness == "",]

############################################################################################
#### Participant ID ####

# Clean Questionnaire ID
fixID<-function(dataframe){
  dataframe$ID<-as.character(dataframe$ID)
  dataframe$ID<-toupper(dataframe$ID)
  
  x<-substr(dataframe$ID, 1, 1)
  y<-substr(dataframe$ID, 2, 20)
  dataframe$ID<-ifelse(grepl('[A-z]', x), paste0(y,x), paste0(x,y))
  dataframe$ID<-gsub("[^[:alnum:]]", "", dataframe$ID)
  
  dataframe$ID
}
questionnaireCleanID<-setNames(data.frame(fixID(questionnaires)), "OldID")

# Fix ID Codes
fixSubjectLog<-function(dataframe){
  dataframe<-dataframe[,c("ID", "Drug.Code")]
  dataframe$ID<-as.character(dataframe$ID)
  dataframe$Drug.Code<-as.character(dataframe$Drug.Code)
  dataframe$Drug.Code<-gsub("[^[:alnum:]]", "", dataframe$Drug.Code)
  
  x<-substr(dataframe$Drug.Code, 1, 1)
  y<-substr(dataframe$Drug.Code, 2, 20)
  dataframe$FixedDrugCode<-ifelse(grepl('[A-z]', x), paste0(y,x), paste0(x,y))
  
  dataframe$FixedDrugCode[c(1:32,35)]<-paste0(dataframe$FixedDrugCode[c(1:32,35)], "J")
  
  for(i in 132:nrow(dataframe)){
    dataframe$FixedDrugCode[i]<-paste0(dataframe$FixedDrugCode[i], "2")
  }
  
  for(i in 1:nrow(dataframe)){
    if(grepl("NA", dataframe$Drug.Code[i])){dataframe$FixedDrugCode[i]<-NA}
    if(dataframe$Drug.Code[i] == ""){dataframe$FixedDrugCode[i]<-NA}
  }
  
  dataframe$FixedDrugCode[279]<-"4D3"
  dataframe<-dataframe[,c("ID", "FixedDrugCode")]
  dataframe
}
subjectlog<-read.csv("Subject Log.csv", header = T)
idCodes<-setNames(data.frame(fixSubjectLog(subjectlog)), c("ID", "FixedDrugCode"))

# Clean Drug Codes
fixDrugCodes<-function(dataframe){
  dataframe$Tray<-as.character(dataframe$Tray)
  dataframe$Tray<-gsub("[^[:alnum:]]", "", dataframe$Tray)
  dataframe$Number<-as.character(dataframe$Number)
  dataframe$Number<-gsub("[^[:alnum:]]", "", dataframe$Number)
  
  dataframe$Combined<-paste0(dataframe$Number,dataframe$Tray)
  dataframe<-dataframe[,c("Combined", "Drug")]
  dataframe
}
drugplacebo<-read.csv("Drug Codes.csv", header = T)
drugcodes<-setNames(data.frame(fixDrugCodes(drugplacebo)), c("Combined", "Drug"))

# Make Final Dataframe for Participant ID/Drug Code
participantIDandDrugCode<-function(originalDF, subjectlogDF, drugcodeDF){
  
  # Match Drug Codes and ID Codes
  subjectlogDF$Match<-match(subjectlogDF$FixedDrugCode, drugcodeDF$Combined)
  
  for(i in 1:nrow(subjectlogDF)){
    match = subjectlogDF$Match[i]
    subjectlogDF$DrugPlacebo[i]<-drugcodeDF$Drug[match]
  }
  
  subjectlogDF<-subjectlogDF[,c("ID", "FixedDrugCode", "DrugPlacebo")]
  
  # Match ID from Questionnaires and ID Codes
  originalDF$Match1<-match(originalDF$OldID, subjectlogDF$ID)
  originalDF$Match2<-match(originalDF$OldID, subjectlogDF$FixedDrugCode)
  
  for(i in 1:nrow(originalDF)){
    if(!is.na(originalDF$Match1[i])){
      match = originalDF$Match1[i]
      originalDF$DrugCode[i]<-subjectlogDF$FixedDrugCode[match]
      originalDF$FixedID[i]<-subjectlogDF$ID[match]
    }
    
    if(!is.na(originalDF$Match2[i])){
      match = originalDF$Match2[i]
      originalDF$DrugCode[i]<-subjectlogDF$FixedDrugCode[match]
      originalDF$FixedID[i]<-subjectlogDF$ID[match]
    }
    
    if(is.na(originalDF$Match1[i]) & is.na(originalDF$Match2[i])){
      originalDF$DrugCode[i]<-NA
      originalDF$FixedID[i]<-NA
    }
  }
  
  # Add Drug Codes
  originalDF$Match3<-match(originalDF$DrugCode, subjectlogDF$FixedDrugCode)
  
  for(i in 1:nrow(originalDF)){
    if(!is.na(originalDF$Match3[i])){
      match = originalDF$Match3[i]
      originalDF$DrugPlacebo[i]<-subjectlogDF$DrugPlacebo[match]
    }
    
    if(is.na(originalDF$FixedID[i])){
      originalDF$DrugPlacebo[i]<-NA
    }
  }
  
  originalDF<-originalDF[,c("FixedID", "DrugCode", "DrugPlacebo")]
  # distinct<-distinct(originalDF)
  # distinct
  originalDF
}

fixedQuestionnaires<-setNames(data.frame(participantIDandDrugCode(questionnaireCleanID, idCodes, drugcodes)), c("SubjectID", "DrugCode", "DrugPlacebo"))
fixedIP<-cbind(fixedQuestionnaires, ip)

############################################################################################
##### Demographics/Tylenol Behaviors ####
demographics<-questionnaires[,c(1,2:13)]
demographics[]<-lapply(demographics, tolower)

# Meds Effectiveness
fixedQuestionnaires$MedsEffectiveness<-factor(demographics$MedsEffectiveness,
                                       levels = c("i have never taken tylenol", "not effective at all",
                                                  "slightly effective", "moderately effective",
                                                  "very effective", "extremely effective"),
                                       labels = c("never taken", "not at all", "slightly", "moderately",
                                                  "very", "extremely"))
  
# Last Time Took Meds
replaceLTTM<-function(dataframe, colname){
  cn1 = which(colnames(dataframe) == colname)
  dataframe[,cn1]<-as.character(dataframe[,cn1])
  
  cn2 = ncol(dataframe)

  for(i in 1:nrow(dataframe)){
    if(grepl("min", dataframe[i, cn1])){
      dataframe[i, cn2]<-"minutes"
      dataframe[i, cn1]<-sub("min|minute|minutes", "", dataframe[i, cn1])
    } else if(grepl("hr|hour", dataframe[i, cn1])){
      dataframe[i, cn2]<-"hours"
      dataframe[i, cn1]<-sub("hour|hrs|hr|hours", "", dataframe[i, cn1])
    } else if(grepl("day", dataframe[i, cn1])){
      dataframe[i, cn2]<-"days"
      dataframe[i, cn1]<-sub("day|days", "", dataframe[i, cn1])
    } else if(grepl("week", dataframe[i, cn1])){
      dataframe[i, cn2]<-"weeks"
      dataframe[i, cn1]<-sub("week|weeks", "", dataframe[i, cn1])
    } else if(grepl("month", dataframe[i, cn1])){
      dataframe[i, cn2]<-"months"
      dataframe[i, cn1]<-sub("month|months", "", dataframe[i, cn1])
    } else if(grepl("year", dataframe[i, cn1])){
      dataframe[i, cn2]<-"years"
      dataframe[i, cn1]<-sub("year|years", "", dataframe[i, cn1])
    } else if(grepl("never|n/a|no|haven't", dataframe[i, cn1])){
      dataframe[i, cn2]<-"never"
      dataframe[i, cn1]<-""
    } else if(grepl("not sure|unknown|can't remember|dont know|do not remember|don't know|very long time|forever|don't remember|a long time|can't remeber", dataframe[i, cn1])){
      dataframe[i, cn2]<-"unknown"
      dataframe[i, cn1]<-""
    }
  }
  
  for(i in 1:nrow(dataframe)){
    if(grepl("-", dataframe[i, cn1])){
      dataframe[i, cn1]<-sub(".-*", "", dataframe[i, cn1])}
    if(grepl(">|about|at least|+|over|~|more than", dataframe[i, cn1])){
      dataframe[i, cn1]<-sub(">|about|at least|\\+|over|~|more than", "", dataframe[i, cn1])}
    if(grepl("few|several|couple", dataframe[i, cn1])){
      dataframe[i, cn1]<-">1"}
    if(grepl("one|a|last|yester|within", dataframe[i, cn1])){
      dataframe[i, cn1]<-"1"}
    if(grepl("two", dataframe[i, cn1])){
      dataframe[i, cn1]<-"2"}
    if(grepl("tee|three", dataframe[i, cn1])){
      dataframe[i, cn1]<-"3"}
    if(grepl("four", dataframe[i, cn1])){
      dataframe[i, cn1]<-"4"}
  }
  
  for(i in 1:nrow(dataframe)){
    dataframe[i, cn1]<-gsub("[^[:digit:].,>]", "", dataframe[i, cn1])
    dataframe[i, cn1]<-paste0(dataframe[i, cn1], " ", dataframe[i, cn2])
  }
  
  dataframe[,cn1]
}

demographics$Delete<-NA
fixedQuestionnaires$LastTimeTookMeds<-replaceLTTM(demographics, "LastTimeTookMeds")

# Preferred Meds
replacePrefMeds<-function(dataframe, colname){
  cn1 = which(colnames(dataframe) == colname)
  dataframe[,cn1]<-as.character(dataframe[,cn1])
  
  for(i in 1:nrow(dataframe)){
    if(grepl("and|or|also", dataframe[i, cn1])){
      dataframe[i, cn1]<-"More Than One Type"}
    if(grepl("tylenol|acetaminophen|midol|excedrin|acetaminophin", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Acetaminophen"}
    if(grepl("ibuprofen|fenbid|ibprofuen|ip profen|ibuprophen|ibprophen|ibueprofen|advil|motrin|ibiphrofen|ibeprofen|ibuproffen|ibuprofin|ibiprofen|ibruprofen|ibprofen", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Ibuprofen"}
    if(grepl("naproxen|aleve|naproxin|alieve|aliev", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Naproxen Sodium"}
    if(grepl("none|nope|barely take medicine|prefer not to work my liver|nothing|never used|hardly use", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Do Not Take Meds"}
    if(grepl("2 weeks|15|some can stop|don't know|don't really use|do not know|no preference|don't use|no preference|don't take|anything|dont know", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Unknown"}
    if(grepl("aspirin|alkaseltzer|alka-seltzer|acetylsalicyclic acid", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Acetylsalicylic Acid"}
    if(grepl("equate", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Calcium Carbonate"}
    if(grepl("buklofen|baclofen", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Baclofen"}
    if(grepl("sudafed|pseudoephedrine", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Pseudoephedrine Hydrochloride"}
    if(grepl("marijuana", dataframe[i, cn1])){
      dataframe[i, cn1]<-"Marijuana"}
    
    if(dataframe[i, cn1] == ""){
      dataframe[i, cn1]<-"Unknown"}
  }
  dataframe[,cn1]
}

fixedQuestionnaires$PreferredMeds<-replacePrefMeds(demographics, "PreferredMeds")

# Frequency Take Meds
fixedQuestionnaires$FrequencyTakeMeds<-factor(demographics$FrequencyTakeMeds,
                                              levels = c("i have never taken a pain-killer", "less than once a year",
                                                         "at least once a year", "several times a year",
                                                         "at least once a month", "several times a month",
                                                         "at least once a week", "several times a week", "almost everyday"),
                                              labels = c("never taken", "less than once a year",
                                                         "at least once a year", "several times a year",
                                                         "at least once a month", "several times a month",
                                                         "at least once a week", "several times a week", "almost everyday"))

# Politics
fixedQuestionnaires$Politics<-factor(demographics$Politics,
                                              levels = c("very liberal", "liberal", "somewhat liberal", "moderate", 
                                                         "somewhat conservative", "conservative", "very conservative"))

# Political Party
fixedQuestionnaires$PoliticalParty<-factor(demographics$PoliticalParty,
                                              levels = c("democrat", "republican", "libertarian", "other"))

# Childhood SES
  # S.Cohen, Janicki-Deverts, Chen, & Matthews, 2010; Duncan, Ziol-Guest, & Kalil, 2010
  # average of three items 
  # 1 = strongly disagree; 7 = strongly agree
  # aggregated to form one index, with higher scores reflecting higher SES

colnames(demographics) # columns 8-10 are what we are interested in

likertToOrdinal<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn<-unlist(x[-1])
  
  for(i in cn){
    dataframe[,i]<-factor(dataframe[,i],
                         levels = c("strongly disagree", "disagree",
                                    "somewhat disagree", "neither agree nor disagree",
                                    "somewhat agree", "agree", "strongly agree"),
                         labels = c("1", "2", "3", "4", "5", "6", "7"))
    dataframe[,i]<-as.numeric(dataframe[,i])
  }
  dataframe[,cn]
}

fixedChildhoodSES<-likertToOrdinal(demographics, 8, 9, 10)
fixedQuestionnaires$ChildSES<-rowMeans(fixedChildhoodSES, na.rm=TRUE)

# Current SES -- from Vlad Griskevicius
  # average of three items 
  # 1 = strongly disagree; 7 = strongly agree
  # aggregated to form one index, with higher scores reflecting higher SES

colnames(demographics) # columns 11-13 are what we are interested in

fixedAdultSES<-likertToOrdinal(demographics, 11, 12, 13)
fixedQuestionnaires$AdultSES<-rowMeans(fixedAdultSES, na.rm=TRUE)

# Check Fixed Questionnaires So Far
head(fixedQuestionnaires)

############################################################################################
#### Recent Health Behaviors ####
health<-questionnaires[,c(1,14:60)]
health[]<-lapply(health, tolower)

# Height
health$Feet<-as.numeric(gsub(" feet.*", "", health$Height))*12
health$Inches<-as.numeric(gsub("^.*feet\\s*|\\s*inch.*$", "", health$Height))
colnames(health) # we want 49 and 50

fixedQuestionnaires$Height<-apply(health[49:50], 1, sum)

# Weight
health$fixWeight<-as.numeric(gsub("([0-9]+).*$", "\\1", health$Weight))
fixedQuestionnaires$Weight<-ifelse(grepl("kg", health$Weight), 
                                   health$fixWeight[i]*2.20462, health$fixWeight)

# Sleep Hours
fixSleepHours<-function(dataframe, colname){
  cn1 = which(colnames(dataframe) == colname)
  cn2 = ncol(dataframe)
  
  dataframe[,cn2]<-gsub("[^[:digit:].]", "\\1", dataframe[,cn1])

  dataframe[,cn2]<-ifelse(grepl("-", dataframe[,cn1]), 
                          gsub(".*-(.+) [a-z].*", "\\1", dataframe[,cn1]), 
                          dataframe[,cn2])

  dataframe[,cn2]<-ifelse(grepl("half", dataframe[,cn1]), 
                          paste0(dataframe[,cn2], ".5"), 
                          dataframe[,cn2])

  dataframe[,cn2]<-ifelse(grepl("min", dataframe[,cn1]), 
                          as.numeric(gsub("hrs.*|hours.*", "\\1", dataframe[,cn1]))+ 
                            as.numeric(gsub(".* hr |.*hrs|.*hrs |.* hours|.*hours |.* hours |.* and |.*and (.+) min.*| min.*|min.*", "\\1", 
                                            dataframe[,cn1]))/60,
                          dataframe[,cn2])

  dataframe[,cn2]<-as.numeric(dataframe[,cn2])
  dataframe[,cn2]
}

health$fixSleepHours<-NA
fixedQuestionnaires$SleepHours<-fixSleepHours(health, "SleepHours")

# When Last Ate
fixWhenLastAte<-function(dataframe, colname){
  cn1 = which(colnames(dataframe) == colname)
  cn2 = ncol(dataframe)
  
  dataframe[,cn1]<-gsub("ten", "10", dataframe[,cn1])
  dataframe[,cn1]<-gsub("one", "1", dataframe[,cn1])
  
  dataframe[,cn2]<-gsub("[^[:digit:].]", "\\1", dataframe[,cn1])
  
  # dataframe[,cn2]<-ifelse(grepl("-", dataframe[,cn1]), 
  #                         gsub(".*-(.+).*min", "\\1", dataframe[,cn1]),
  #                         dataframe[,cn2])
  
  dataframe[,cn2]<-ifelse(grepl("half", dataframe[,cn1]), 
                          paste0(dataframe[,cn2], ".5"), 
                          dataframe[,cn2])

  for(i in 1:nrow(dataframe)){
    if(grepl("min", dataframe[i,cn1])){
      ifelse(is.na(as.numeric(gsub("hr.*|hrs.*|hours.*| hours.*| hour.*", "\\1", dataframe[i,cn1]))),
             dataframe[i,cn2]<-0, dataframe[i,cn2]<-as.numeric(gsub("hr.*|hrs.*|hours.*| hours.*| hour.*", "\\1", dataframe[i,cn1])))
      dataframe[i,cn2]<-as.numeric(dataframe[i,cn2]) +
        as.numeric(gsub(".*hr|.*-|.* hr |.*hrs|.*hrs |.* hours|.*hours |.* hour, |.* hour |.* hours |.* and |.*and (.+) min.*| min.*|min.*", "\\1", 
                        dataframe[i,cn1]))/60
    }
  }
  
  dataframe[,cn2]<-as.numeric(dataframe[,cn2])
  dataframe[,cn2]
}

health$FixWhenLastAte<-NA
fixedQuestionnaires$WhenLastAte<-fixWhenLastAte(health, "WhenLastAte")

# How Much Last Ate
fixedQuestionnaires$HowMuchLastEat<-factor(health$HowMuchLastEat,
                                     levels = c("snack", "light meal", "full meal"))

# Sleep Quality
fixedQuestionnaires$SleepQuality<-as.numeric(factor(health$SleepQuality,
                                         levels = c("extremely poorly1", "2", "3", "4", "5",
                                                    "6", "7", "8", "9", "extremely well10"),
                                         labels = c("1", "2", "3", "4", "5",
                                                    "6", "7", "8", "9", "10")))

# Illness Severity
  # average of 8 items (e.g., sneezing, runny nose, ... chilliness)
  # 1 = none; 4 = severe
  # aggregated to form one index, with higher scores reflecting higher reported illness

colnames(health) # columns 8-15 are what we are interested in

illnessSeverity<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn<-unlist(x[-1])
  
  for(i in cn){
    dataframe[,i]<-factor(dataframe[,i],
                          levels = c("none", "mild",
                                     "moderate", "severe"),
                          labels = c("1", "2", "3", "4"))
    dataframe[,i]<-as.numeric(dataframe[,i])
  }
  dataframe[,cn]
}

fixedIllnessSeverity<-illnessSeverity(health, 8, 9, 10, 11, 12, 13, 14, 15)
fixedQuestionnaires$IllnessSeverity<-rowMeans(fixedIllnessSeverity, na.rm=TRUE)

# Exercise Today
colnames(health) # we want columns 16-18
exerciseToday<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn1<-unlist(x[2])
  cn2<-unlist(x[3])
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,cn1] == "no"){
      dataframe[i,cn1]<-0
    } else if(dataframe[i,cn1] == "yes"){
      dataframe[i,cn1]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[i,cn2]))
    } else {
      dataframe[i,cn1]<-NA}
    }
  dataframe[,cn1]
}

fixedQuestionnaires$exerciseMinToday<-exerciseToday(health, 16, 17)

# When Last Exercise
cn = which(colnames(health) == "WhenExercise")
health[,cn]<-gsub("1/2", ".5", health[,cn])
fixedQuestionnaires$WhenLastExerciseHrs<-as.numeric(gsub("[^[:digit:].]", "\\1", health[,cn]))

# Exercise Regularly
  # multiply days by minutes
colnames(health) # we want columns 19-20

fixExerciseRegularly<-function(dataframe){
  cn1 = which(colnames(dataframe) == "ExerciseFrequency")
  cn2 = which(colnames(dataframe) == "TypicalExerciseLength")
  cn3 = which(colnames(dataframe) == "FixExerciseLength")

  dataframe[,cn1]<-as.numeric(dataframe[,cn1])
  
  dataframe[,cn3]<-gsub("[^[:digit:].]", "\\1", dataframe[,cn2])
  
  for(i in 1:nrow(dataframe)){
    if(grepl("-", dataframe[i,cn2])){
      dataframe[i,cn3]<-gsub(".*-|.*- (.+) min.*| min.*| mionutes*|min.*| hour.*|hour.*| hr.*|hr.*", "\\1", dataframe[i,cn2])
    }
  }
  
  dataframe[,cn3]<-as.numeric(dataframe[,cn3])
  
  for(i in 1:nrow(dataframe)){
    if(grepl("hour|hr|ours|h", dataframe[i,cn2])){
      dataframe[i,cn3]<-dataframe[i,cn3]*60
    }
    
    if(grepl("times a day", dataframe[i,cn2])){
      dataframe[i,cn3]<-as.numeric(gsub(".*for (.+) min.*", "\\1", dataframe[i,cn2]))
    }
  }
  dataframe[,cn2]<-dataframe[,cn1]*dataframe[,cn3]
  dataframe[,cn2]
}

health$FixExerciseLength<-NA
fixedQuestionnaires$ExerciseRegularMins<-fixExerciseRegularly(health)

# Last Consume Caffeine 

fixWhenCaffeine<-function(dataframe){
  cn1 = which(colnames(dataframe) == "WhenCaffeine")
  cn2 = which(colnames(dataframe) == "FixWhenCaffeine")

  dataframe[,cn1]<-gsub("one", "1", dataframe[,cn1])
  dataframe[,cn1]<-gsub("two", "2", dataframe[,cn1])
  dataframe[,cn1]<-gsub("six", "6", dataframe[,cn1])
  dataframe[,cn1]<-gsub("fifteen", "15", dataframe[,cn1])
  dataframe[,cn1]<-gsub("a week", "1 week", dataframe[,cn1])
  dataframe[,cn1]<-gsub("a month", "1 month", dataframe[,cn1])
  dataframe[,cn1]<-gsub("at 7 and 11am this morning", "1 hour", dataframe[,cn1])
  dataframe[,cn1]<-gsub("34 mg", "", dataframe[,cn1])
  dataframe[,cn1]<-gsub(" and", ",", dataframe[,cn1])
  dataframe[,cn1]<-gsub("hr ", "hr, ", dataframe[,cn1])
  dataframe[,cn1]<-gsub("19- ", "", dataframe[,cn1])
  
  dataframe[,cn2]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[,cn1]))
  
  for(i in 1:nrow(dataframe)){
   if(grepl("sec", dataframe[i,cn1])){ # there's only one instance of this
      dataframe[i,cn2]<-as.numeric(round((2/60/60), 3))
    }
    
    if(grepl("m|min", dataframe[i,cn1])){
      dataframe[i,cn2]<-round(as.numeric(gsub("m.*", "\\1", dataframe[i,cn1]))/60, 3)
    }
    
    if(grepl("day", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) day.*|day.*", "\\1",dataframe[i,cn1]))*24
    }

    if(grepl("week", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" week.*| weeks.*", "\\1", dataframe[i,cn1]))*24*7
    }
    
    if(grepl("month", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*over (.+) month.*", "\\1", dataframe[i,cn1]))*24*30
    }
    
    if(grepl("hr,|hour,|hours,", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" hour.*| hr.*", "\\1",dataframe[i,cn1]))+
        as.numeric(gsub(".*, (.+) min.*", "\\1",dataframe[i,cn1]))/60
    }
    
    if(grepl("day,|days,", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" day.*", "\\1",dataframe[i,cn1]))*24 +
        as.numeric(gsub(".*, (.+) hour.*", "\\1",dataframe[i,cn1]))
    }
  }
  dataframe[,cn2]
}

health$FixWhenCaffeine<-NA
fixedQuestionnaires$LastConsumeCaffeineHours<-fixWhenCaffeine(health)

# General Health
fixedQuestionnaires$GeneralHealth<-as.numeric(factor(health$GeneralHealth,
                                                    levels = c("poor", "fair", "good",
                                                               "very good", "excellent"),
                                                    labels = c("1", "2", "3", "4", "5")))

# Subjective Feelings of Illness
  # average of three separate questions: "over the last couple of days...", 
  # "Lately, I have been feeling...", "I have felt sick within the last week
  # each were on a scale of 1-5 (strongly disagree to strongly agree)

colnames(health) # 23-25

fixIllness<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn<-unlist(x[-1])
  
  for(i in cn){
    dataframe[,i]<-factor(dataframe[,i],
                          levels = c("strongly disagree\n1", "2",
                                     "3", "4", "strongly agree\n5"),
                          labels = c("1", "2", "3", "4", "5"))
    dataframe[,i]<-as.numeric(dataframe[,i])
  }
  dataframe[,cn]
}

fixedIllness<-fixIllness(health, 23, 24, 25)
fixedQuestionnaires$SubjectiveIllness<-rowMeans(fixedIllness, na.rm=TRUE)
fixedQuestionnaires$SubjectiveIllness<-ifelse(grepl("NaN", fixedQuestionnaires$SubjectiveIllness),
                                              NA, fixedQuestionnaires$SubjectiveIllness)

# When Last Sick
fixedQuestionnaires$WhenLastSick<-factor(health$WhenLastSick,
                                         levels = c("a year or more ago", "a few months ago", 
                                                    "a month ago", "a couple of weeks ago",
                                                    "a week ago", "a couple of days ago", "today"))

# Number of Doctor Visits in the Last 3 Months
colnames(health) # we want columns 27-28

numDoctorVisits<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn1<-unlist(x[2])
  cn2<-unlist(x[3])
  
  dataframe[,cn2]<-gsub("no|none|n/a", "0", dataframe[,cn2])
  dataframe[,cn2]<-gsub("once", "1", dataframe[,cn2])
  dataframe[,cn2]<-gsub("twice", "2", dataframe[,cn2])
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,cn1] == "no"){
      dataframe[i,cn1]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[i,cn2]))
    } else if(dataframe[i,cn1] == "yes"){
      dataframe[i,cn1]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[i,cn2]))
    } else {
      dataframe[i,cn1]<-NA
    }
  }

  dataframe[,cn1]
}

fixedQuestionnaires$NumDoctorVisits<-numDoctorVisits(health, 27, 28)

# Take Meds Recently
fixedQuestionnaires$TakeMedsRecentlyYN<-factor(health$TakeMedsRecentlyYN, levels = c("no", "yes"))

# Meds
  # note -- I didn't know what to do with the different doses/frequency
colnames(health) # we want columns 29-30

health$MedsInfo[health$MedsInfo == ""] <- NA
health$MedsInfo<-ifelse(grepl("none|n\a|n/a|no|na", health$MedsInfo), NA, health$MedsInfo)

fixedQuestionnaires$MedsInfo<-health$MedsInfo

# Birth Control
fixedQuestionnaires$BirthControlYN<-factor(health$BirthControlYN, levels = c("no", "yes"))

# Marijuana Usage -- Frequency
fixedQuestionnaires$MarijuanaFrequency<-factor(health$MarijuanaFrequency, 
                                               levels = c("never", "at least once a year",
                                                          "several times a year", "at least once a month",
                                                          "several times a month", "at least once a week",
                                                          "several times a week", "almost every day"))


# Marijuana Currently in System 
  # Within last 24 Hours
  # Between 1-45 days (still in urine)
  # Between 45-75 days (still in blood)
  # Between 76-90 days (still in hair)
  # Longer than 90 days (not in system)
  # Never

colnames(health) # 33

marijuanaInSystem<-function(dataframe){
  cn1 = which(colnames(dataframe) == "WhenMarijuana")
  cn2 = which(colnames(dataframe) == "MarijuanaInSystem")
  
  dataframe[,cn1]<-gsub("one", "1", dataframe[,cn1])
  dataframe[,cn1]<-gsub("two", "2", dataframe[,cn1])
  dataframe[,cn1]<-gsub("last night", "1 day", dataframe[,cn1])
  dataframe[,cn1]<-gsub("more than a", "1", dataframe[,cn1])
  dataframe[,cn1]<-gsub("11 pm", "", dataframe[,cn1])
  dataframe[,cn1]<-gsub("10h", "10 hr", dataframe[,cn1])
  dataframe[,cn1]<-gsub("a month", "1 month", dataframe[,cn1])

  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn1] == "days ago", dataframe[i,cn1]<-"1 day ago", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "1y", dataframe[i,cn1]<-"1 year", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "0", dataframe[i,cn1]<-"never", dataframe[i,cn1])
  }
  
  dataframe[,cn2]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[,cn1]))
  
  for(i in 1:nrow(dataframe)){
    if(grepl("hour|hr", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) hr.*|hr.*| hour.*|hour.*| hours.*", "\\1",
                                        dataframe[i,cn1]))/24
    }
    
    if(grepl("day", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) day.*|day.*", "\\1",
                                        dataframe[i,cn1]))
    }

    if(grepl("week", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" week.*| weeks.*", "\\1", dataframe[i,cn1]))*7
    }

    if(grepl("month", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)month.*| month.*", "\\1", 
                                        dataframe[i,cn1]))*30
    }

    if(grepl("year", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)year.*| year.*", "\\1", 
                                        dataframe[i,cn1]))*365
    }
    dataframe[i,cn2]<-round(dataframe[i,cn2], 3)
  }

  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn1] == "-", dataframe[i,cn1]<-"Never", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "", dataframe[i,cn1]<-NA, dataframe[i,cn1])
    ifelse(grepl("never|n/a|n\a|no", dataframe[i,cn1]), dataframe[i,cn1]<-"Never", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 90, dataframe[i,cn1]<-"Not in System", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 75 & dataframe[i,cn2] <= 90, dataframe[i,cn1]<-"In Hair", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 45 & dataframe[i,cn2] <= 75, dataframe[i,cn1]<-"In Blood", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 1 & dataframe[i,cn2] <= 45, dataframe[i,cn1]<-"In Urine", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] <= 1, dataframe[i,cn1]<-"Today", dataframe[i,cn1])
  }
  
  dataframe[,cn1]<-factor(dataframe[,cn1], 
                          levels = c("Never", "Not in System", "In Hair", "In Blood", "In Urine", "Today"))

  dataframe[,cn1]
}

health$MarijuanaInSystem<-NA
fixedQuestionnaires$MarijuanaInSystem<-marijuanaInSystem(health)

# Cigarettes Per Day
fixedQuestionnaires$CigarettesPerDay<-factor(health$CigaretteFrequency, 
                                             levels = c("0", "1-10", "11-20", "21-30", "31 or more"))

# Cigarettes in System
  # Within last 24 Hours
  # Between 1-4 days (still in urine/saliva)
  # Between 4-10 days (still in blood)
  # Between 11-90 days (still in hair)
  # Longer than 90 days (not in system)
  # Never

colnames(health) # 35

cigarettesInSystem<-function(dataframe){
  cn1 = which(colnames(dataframe) == "WhenCigarette")
  cn2 = which(colnames(dataframe) == "CigarettesInSystem")
  
  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn1] == "few days", dataframe[i,cn1]<-"2 days", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "a month", dataframe[i,cn1]<-"1 month", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "24h", dataframe[i,cn1]<-"24 hours", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "10h", dataframe[i,cn1]<-"10 hours", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "9h", dataframe[i,cn1]<-"9 hours", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "0", dataframe[i,cn1]<-"never", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "-", dataframe[i,cn1]<-"never", dataframe[i,cn1])
  }
  
  dataframe[,cn2]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[,cn1]))
  
  for(i in 1:nrow(dataframe)){
    if(grepl("hour|hr", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) hr.*|hr.*| hour.*|hour.*| hours.*", "\\1",
                                        dataframe[i,cn1]))/24
    }
    
    if(grepl("day", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) day.*|day.*", "\\1",
                                        dataframe[i,cn1]))
    }
    
    if(grepl("week", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" week.*| weeks.*", "\\1", dataframe[i,cn1]))*7
    }
    
    if(grepl("month", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)month.*| month.*", "\\1", 
                                        dataframe[i,cn1]))*30
    }
    
    if(grepl("year", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)year.*| year.*", "\\1", 
                                        dataframe[i,cn1]))*365
    }
    dataframe[i,cn2]<-round(dataframe[i,cn2], 3)
  }
  
  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn1] == "", dataframe[i,cn1]<-NA, dataframe[i,cn1])
    ifelse(grepl("never|n/a|n\a|no|none", dataframe[i,cn1]), dataframe[i,cn1]<-"Never", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 90, dataframe[i,cn1]<-"Not in System", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 10 & dataframe[i,cn2] <= 90, dataframe[i,cn1]<-"In Hair", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 4 & dataframe[i,cn2] <= 10, dataframe[i,cn1]<-"In Blood", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] > 1 & dataframe[i,cn2] <= 4, dataframe[i,cn1]<-"In Urine/Saliva", dataframe[i,cn1])
    ifelse(dataframe[i,cn2] <= 1, dataframe[i,cn1]<-"Today", dataframe[i,cn1])
  }
  
  dataframe[,cn1]<-factor(dataframe[,cn1], 
                          levels = c("Never", "Not in System", "In Hair", "In Blood", "In Urine/Saliva", "Today"))
  
  dataframe[,cn1]
}

health$CigarettesInSystem<-NA
fixedQuestionnaires$CigarettesInSystem<-cigarettesInSystem(health)

# Last Drink Alcohol

colnames(health) # 36

lastAlcohol<-function(dataframe){
  cn1 = which(colnames(dataframe) == "WhenAlcohol")
  cn2 = which(colnames(dataframe) == "FixedAlcohol")
  
  dataframe[,cn1]<-gsub("one", "1", dataframe[,cn1])
  dataframe[,cn1]<-gsub("two", "2", dataframe[,cn1])
  dataframe[,cn1]<-gsub("three", "3", dataframe[,cn1])
  dataframe[,cn1]<-gsub("half", ".5", dataframe[,cn1])
  dataframe[,cn1]<-gsub("a month", "1 month", dataframe[,cn1])
  dataframe[,cn1]<-gsub("last week", "1 week", dataframe[,cn1])
  
  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn1] == "week", dataframe[i,cn1]<-"1 week", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "10h", dataframe[i,cn1]<-"10 hours", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "1 day 10 hours", dataframe[i,cn1]<-"1 day, 10 hours", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "few days", dataframe[i,cn1]<-"2 days", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "-", dataframe[i,cn1]<-"never", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "month", dataframe[i,cn1]<-"1 month", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "last weekend", dataframe[i,cn1]<-"2 days", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "december 2018", dataframe[i,cn1]<-"1 month", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "sunday", dataframe[i,cn1]<-"2 days", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "20d", dataframe[i,cn1]<-"20 days", dataframe[i,cn1])
    ifelse(dataframe[i,cn1] == "1 weekend (5 days ago)", dataframe[i,cn1]<-"5 days", dataframe[i,cn1])
  }
  
  dataframe[,cn2]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[,cn1]))
  
  for(i in 1:nrow(dataframe)){
    if(grepl("hour|hr", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) hr.*|hr.*| hour.*|hour.*| hours.*", "\\1",
                                        dataframe[i,cn1]))/24
    }
    
    if(grepl("day", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about (.+) day.*|day.*", "\\1",
                                        dataframe[i,cn1]))
    }
    
    if(grepl("week", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" week.*| weeks.*", "\\1", dataframe[i,cn1]))*7
    }
    
    if(grepl("month", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)month.*|month.*| month.*", "\\1", 
                                        dataframe[i,cn1]))*30
    }
    
    if(grepl("year", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(".*-|.*- |.*at least |.*about |.*over (.+)year.*| year.*", "\\1", 
                                        dataframe[i,cn1]))*365
    }
    
    if(grepl("hr,|hour,|hours,", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" hour.*| hr.*", "\\1",dataframe[i,cn1]))+
        as.numeric(gsub(".*, (.+) min.*", "\\1",dataframe[i,cn1]))/60
    }
    
    if(grepl("day,|days,", dataframe[i,cn1])){
      dataframe[i,cn2]<-as.numeric(gsub(" day.*", "\\1",dataframe[i,cn1]))*24 +
        as.numeric(gsub(".*, (.+) hour.*", "\\1",dataframe[i,cn1]))
    }
    
    if(grepl("never|n/a", dataframe[i,cn1])){
      dataframe[i,cn2]<-"Never"
    }
  }
  dataframe[,cn2]
}

health$FixedAlcohol<-NA
fixedQuestionnaires$LastAlcoholDays<-lastAlcohol(health)

# Alcohol Tendencies
  # From the Alcohol Consumption Screening AUDIT Questionnaire
  # different number of points for each question
  # we only used some of the questions, so I averaged them instead of summing them

# columns 37-39
alcoholTendencies<-function(...){
  x<-list(...)
  dataframe<-as.data.frame(x[1])
  cn1<-unlist(x[2])
  cn2<-unlist(x[3])
  cn3<-unlist(x[4])
  cn4<-which(colnames(dataframe) == "AlcoholAvg")
  
  # How often drink alcohol?
  dataframe[,cn1]<-as.numeric(as.character(factor(dataframe[,cn1], 
                          levels = c("never", "monthly", "2-4 times a month", 
                                     "2-3 times a week", "4 or more times a week"),
                          labels = c("0", "1", "2", "3", "4"))))
  
  # How many drinks?
  dataframe[,cn2]<-as.numeric(as.character(factor(dataframe[,cn2], 
                          levels = c("i do not drink alcohol", "1-2.", "3-4.", 
                                     "5-6.", "7-9.", "10 or more"),
                          labels = c("NA", "0", "1", "2", "3", "4"))))
  
  # How often have 6+ drinks?
  dataframe[,cn3]<-as.numeric(as.character(factor(dataframe[,cn3], 
                          levels = c("never", "less than monthly", "monthly", 
                                     "weekly", "daily or almost daily"),
                          labels = c("0", "1", "2", "3", "4"))))
  
  # Average
  dataframe[,cn4]<-rowMeans(dataframe[,c(cn1, cn2, cn3)], na.rm=TRUE)
  dataframe[,cn4]<-ifelse(grepl("NaN", dataframe[,cn4]), NA, dataframe[,cn4])
  dataframe[,cn4]
}

health$AlcoholAvg<-NA
fixedQuestionnaires$AlcoholAvg<-alcoholTendencies(health, 37, 38, 39)

# Chronic Illnesses
fixedQuestionnaires$ArthritisYN<-factor(health$ArthritisYN, levels = c("no", "yes"))
fixedQuestionnaires$ImmuneDisordersYN<-factor(health$ImmuneDisordersYN, levels = c("no", "yes"))
fixedQuestionnaires$EndocrineDisordersYN<-factor(health$EndocrineDisordersYN, levels = c("no", "yes"))
fixedQuestionnaires$DiabeticYN<-factor(health$DiabeticYN, levels = c("no", "yes, type i", "yes, type ii"),
                                       labels = c("no", "yes, type 1", "yes, type 2"))

# Gender
fixedQuestionnaires$Gender<-factor(health$Gender, levels = c("prefer not to answer", "female", "male"))

# Age
colnames(health) #45-46

# Year Born is redundant with Current Age
# for(i in 1:nrow(health)){
#   ifelse(health[i,45] == "99", health[i,45]<-"1999", health[i,45])
#   ifelse(health[i,45] == "98", health[i,45]<-"1998", health[i,45])
#   ifelse(health[i,45] == "11998", health[i,45]<-"1998", health[i,45])
#   ifelse(health[i,45] == "1198", health[i,45]<-"1998", health[i,45])
# }
# health$YearBorn<-as.numeric(health$YearBorn)

fixedQuestionnaires$Age<-as.numeric(health$Age)

# Year in University
fixedQuestionnaires$YearUniversity<-factor(health$YearUniversity, 
                                           levels = c("none", "1st year", "2nd year",
                                                      "3rd year", "4th year", "5th year"),
                                           labels = c("None", "1", "2", "3", "4", "5"))

# Race 
colnames(health) #48

for(i in 1:nrow(health)){
  ifelse(health[i,48] == "", health[i,48]<-NA, health[i,48])
  ifelse(health[i,48] == "white", health[i,48]<-"White", health[i,48])
  ifelse(health[i,48] == "asian", health[i,48]<-"Asian", health[i,48])
  ifelse(health[i,48] == "black, african-american or negro", health[i,48]<-"Black", health[i,48])
  ifelse(health[i,48] == "hispanic, latino, or spanish origin", health[i,48]<-"Hispanic", health[i,48])
  ifelse(health[i,48] == "some other race or origin", health[i,48]<-"Other", health[i,48])
  
  ifelse(health[i,48] == "white,hispanic, latino, or spanish origin", health[i,48]<-"White and Hispanic", health[i,48])
  ifelse(health[i,48] == "white,asian", health[i,48]<-"White and Asian", health[i,48])
  ifelse(health[i,48] == "white,black, african-american or negro", health[i,48]<-"White and Black", health[i,48])
  ifelse(health[i,48] == "black, african-american or negro,hispanic, latino, or spanish origin", health[i,48]<-"Black and Hispanic", health[i,48])
  ifelse(health[i,48] == "hispanic, latino, or spanish origin,asian", health[i,48]<-"Hispanic and Asian", health[i,48])
  ifelse(health[i,48] == "black, african-american or negro,some other race or origin", health[i,48]<-"Black and Other", health[i,48])
}
fixedQuestionnaires$Race<-health$Race

############################################################################################
#### Need to Belong ####
  # Leary, M. R., Kelly, K. M., Cottrell, C. A., & Schreindorfer, L. S. (2013). 
  # Construct validity of the need to belong scale: Mapping the nomological network. 

needtobelong<-questionnaires[,c(1,61:70)]
needtobelong[]<-lapply(needtobelong, tolower)

needToBelong<-function(dataframe){
  cn = seq(2, ncol(dataframe)-1, 1) # all column names we care about
  
  # Reverse Scoring
    # col 2: If other people don't seem to accept me, I don't let it bother me.
    # col 4: I seldom worry about whether other people care about me.
    # col 8: Being apart from my friends for long periods of time does not bother me.
  
  rs = c(2,4,8) # reverse-scored questions
  nr = cn[!cn %in% rs] # normal-scored questions (not reverse-scored)

  sr<-which(colnames(dataframe) == "SumNTB")
  
  # Changing Factors
  for(i in nr){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                          levels = c("disagree strongly: 1", "disagree a little: 2", 
                                     "neither agree nor disagree: 3",
                                     "agree a little: 4", "agree strongly: 5"),
                          labels = c("1", "2", "3", "4", "5"))))
  }
  
  # Reverse Scoring
  for(i in rs){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                          levels = c("disagree strongly: 1", "disagree a little: 2", 
                                     "neither agree nor disagree: 3",
                                     "agree a little: 4", "agree strongly: 5"),
                          labels = c("5", "4", "3", "2", "1"))))
  }
  
  # Summing Responses
  dataframe[,sr]<-rowSums(dataframe[,cn])
  dataframe[,sr]
}

needtobelong$SumNTB<-NA
fixedQuestionnaires$NeedToBelong<-needToBelong(needtobelong)

############################################################################################
#### Nostalgia ####
  # Southampton Nostalgia Scale (Version 2)
  # Sedikides, C., Wildschut, T., Arndt, J., & Routledge, C. (2008). 
  # Nostalgia: Past, present, and future. Current Directions in Psychological Science, 17(5), 304-307.
  # They don't say how to score it ANYWHERE, so I averaged it
  # They say there's a reverse scored item but the scale doesn't seem like it and they don't say which
  # one it is, so I didn't reverse score any of them...

nostalgia<-questionnaires[,c(1,71:77)]
nostalgia[]<-lapply(nostalgia, tolower)

fixNostalgia<-function(dataframe){
  cn = seq(2, ncol(dataframe)-1, 1) # all column names we care about
  
  lq = tail(cn, n=1) # last question has different factors
  nr = cn[!cn %in% lq] # normal responses
  ar<-which(colnames(dataframe) == "AvgNostalgia")
  
  # Normal Responses
  for(i in nr){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                                                  levels = c("1: not at all", "2", "3",
                                                             "4", "5", "6", "7: very much"),
                                                  labels = c("1", "2", "3", "4", "5", "6", "7"))))
  }
  
  # Last Question
  dataframe[,lq]<-as.numeric(as.character(factor(dataframe[,lq], 
                                                 levels = c("once or twice a year", "once every couple of months", 
                                                            "once or twice a month", "approximately once a week",
                                                            "approximately twice a week", "three to four times a week",
                                                            "at least once a day"),
                                                 labels = c("1", "2", "3", "4", "5", "6", "7"))))

  
  # Averaging Responses
  dataframe[,ar]<-rowMeans(dataframe[,cn], na.rm = T)
  dataframe[,ar]<-ifelse(grepl("NaN", dataframe[,ar]), NA, dataframe[,ar])
  dataframe[,ar]
}

nostalgia$AvgNostalgia<-NA
fixedQuestionnaires$Nostalgia<-fixNostalgia(nostalgia)

############################################################################################
#### Early Family Environment Questionnaire/Risky Families Questionnaire ####
  # Taylor, S. E., Lerner, J. S., Sage, R. M., Lehman, B. J., & Seeman, T. E. (2004). 
  # Early environment, emotions, responses to stress, and health. Journal of personality, 72(6), 1365-1394.

earlyfam<-questionnaires[,c(1,78:90)]
earlyfam[]<-lapply(earlyfam, tolower)

earlyFamilyEnvironment<-function(dataframe){
  cn = seq(2, ncol(dataframe)-1, 1) # all column names we care about
  
  # Reverse Scoring
    # col 2: Feel Loved
    # col 4: Physical Affection
    # col 13: House Organized
  
  rs = c(2,4,13) # reverse-scored questions
  nr = cn[!cn %in% rs] # normal-scored questions (not reverse-scored)
  
  ar<-which(colnames(dataframe) == "AvgEFE")
  
  # Normal Responses
  for(i in nr){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                                                  levels = c("1: not at all", "2", "3",
                                                             "4", "5: very often"),
                                                  labels = c("1", "2", "3", "4", "5"))))
  }
  
  # Reverse Scoring
  for(i in rs){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                                                  levels = c("1: not at all", "2", "3",
                                                             "4", "5: very often"),
                                                  labels = c("5", "4", "3", "2", "1"))))
  }
  
  # Averaging Responses
  dataframe[,ar]<-rowMeans(dataframe[,cn], na.rm = T)
  dataframe[,ar]<-ifelse(grepl("NaN", dataframe[,ar]), NA, dataframe[,ar])
  dataframe[,ar]
}

earlyfam$AvgEFE<-NA
fixedQuestionnaires$EarlyFamilyEnvironment<-earlyFamilyEnvironment(earlyfam)

############################################################################################
#### PANAS ####
  # Watson, D., Clark, L. A., & Tellegen, A. (1988). 
  # Development and validation of brief measures of positive and negative affect: The PANAS scales. Journal of Personality and Social Psychology, 54(6), 1063-1070.

panas<-questionnaires[,c(1,91:110)]
panas[]<-lapply(panas, tolower)

fixPANAS<-function(dataframe){
  dataframe<-dataframe[,-1]
  
  # Positive Affect Score: 
    # Add the scores on items 1, 3, 5, 9, 10, 12, 14, 16, 17, and 19. 
    # Scores can range from 10 – 50, with higher scores representing higher levels of positive affect.
  pa = c(1, 3, 5, 9, 10, 12, 14, 16, 17, 19)
  sumPA<-which(colnames(dataframe) == "PosPANAS")
  
  # Negative Affect Score: 
    # Add the scores on items 2, 4, 6, 7, 8, 11, 13, 15, 18, and 20. 
    # Scores can range from 10 – 50, with lower scores representing lower levels of negative affect. 
  na = c(2, 4, 6, 7, 8, 11, 13, 15, 18, 20)
  sumNA<-which(colnames(dataframe) == "NegPANAS")
  
  # Positive Affect
  for(i in pa){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                                                  levels = c("slightly or not at all", 
                                                             "a little", "moderately", 
                                                             "quite a bit", "extremely"),
                                                  labels = c("1", "2", "3", "4", "5"))))
  }
  
  # Negative Affect
  for(i in na){
    dataframe[,i]<-as.numeric(as.character(factor(dataframe[,i], 
                                                  levels = c("slightly or not at all", 
                                                             "a little", "moderately", 
                                                             "quite a bit", "extremely"),
                                                  labels = c("1", "2", "3", "4", "5"))))
  }
  
  # Summing Responses
  dataframe[,sumPA]<-rowSums(dataframe[,pa])
  dataframe[,sumNA]<-rowSums(dataframe[,na])
  
  dataframe[,c(sumPA, sumNA)]
}

panas$PosPANAS<-NA
panas$NegPANAS<-NA
fixedQuestionnaires<-cbind(fixedQuestionnaires, fixPANAS(panas))

############################################################################################
##### Ollen ####
  # Ollen, J. E. (2006). 
  # A criterion-related validity test of selected indicators of musical sophistication using expert ratings (Doctoral dissertation, The Ohio State University).

ollen<-questionnaires[,c(58,111:119)]
ollen[]<-lapply(ollen, tolower)

# Cleaning User Inputs
cleanOllen<-function(dataframe, cn){
  # Cleaning columns 1-4 
    # (Age, Age Start Music, Years Private Lessons, Years Daily Practice)
    # Making it into years
  
  for(i in 1:nrow(dataframe)){
    ifelse(dataframe[i,cn] == "-", dataframe[i,cn]<-"0", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "one", dataframe[i,cn]<-"1", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "zero", dataframe[i,cn]<-"0", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "four years", dataframe[i,cn]<-"4", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "three years", dataframe[i,cn]<-"3", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "o", dataframe[i,cn]<-"0", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "2005", dataframe[i,cn]<-"4", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "6 months", dataframe[i,cn]<-".5", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "10-12 months", dataframe[i,cn]<-"1", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "a year", dataframe[i,cn]<-"1", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "-", dataframe[i,cn]<-"0", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "30-36 monthes", dataframe[i,cn]<-"3", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "2 days per week", dataframe[i,cn]<-NA, dataframe[i,cn])
    ifelse(dataframe[i,cn] == "24 months", dataframe[i,cn]<-"2", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "2-3", dataframe[i,cn]<-"3", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "3-4", dataframe[i,cn]<-"4", dataframe[i,cn])
    ifelse(dataframe[i,cn] == "3-4 years", dataframe[i,cn]<-"4", dataframe[i,cn])
  }
  
  dataframe[,cn]<-as.numeric(gsub("[^[:digit:].]", "\\1", dataframe[,cn]))
}

# Cleaned Columns 1-4
for(i in 1:4){ollen[i]<-cleanOllen(ollen, i)}
for(i in 1:nrow(ollen)){
  ifelse(ollen[i,2] == 0, ollen[i,2]<-ollen[i,1], ollen[i,2])}

fixOllen<-function(dataframe){

  # Current Age: Column 1
    # Enter number of years and multiply the answer by its regression coefficient
    # Keep any negative signs
    # Enter the product in the far right column
  
  dataframe[,1]<-dataframe[,1]*(0.027)
  
  # Age Start Musical Activity: Column 2
    # Enter number of years and multiply the answer by its regression coefficient
    # Keep any negative signs
    # Enter the product in the far right column
  
  dataframe[,2]<-dataframe[,2]*(-0.026)
  
  # Years Private Lessons: Column 3
    # Enter number of years and multiply the answer by its regression coefficient
    # Keep any negative signs
    # Enter the product in the far right column
  dataframe[,3]<-dataframe[,3]*(-0.076)
  
  # Years Daily Practice: Column 4
    # Enter number of years and multiply the answer by its regression coefficient
    # Keep any negative signs
    # Enter the product in the far right column 
  dataframe[,4]<-dataframe[,4]*(0.042)
  
  # Current Practice: Column 5
    # Enter ONE value in the far right column that corresponds with the participant's
    # answer category
    # Keep any negative signs
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,5] == "i rarely or never practice singing or playing an instrument") 
      {dataframe[i,5]<- 0}
    if(dataframe[i,5] == "about 1 hour per month") 
      {dataframe[i,5]<- -0.060}
    if(dataframe[i,5] == "about 1 hour per week") 
      {dataframe[i,5]<- -0.098}
    if(dataframe[i,5] == "about 14 minutes per day") 
      {dataframe[i,5]<- -0.301}
    if(dataframe[i,5] == "about 1 hour per day") 
      {dataframe[i,5]<- -1.211}
    if(dataframe[i,5] == "more than 2 hours per day") 
      {dataframe[i,5]<- -1.528}
    if(dataframe[i,5] == "") 
      {dataframe[i,5]<- NA}
  }
  
  # Music Courses Yes/No AND How Much Coursework: Columns 6 and 7
    # For these two questions COMBINED, enter ONE value in the
    # far right column that corresponds with the participant's answer category
    # Keep any negative signs
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,6] == "no") {
      dataframe[i,6]<- 0
    } else if(dataframe[i,6] == "yes") {
      if(dataframe[i,7] == "none") 
        {dataframe[i,6]<- -0.423}
      if(dataframe[i,7] == "1 or 2 non-major courses (e.g. music appreciation or singing in an ensemble)") 
        {dataframe[i,6]<- 0.274}
      if(dataframe[i,7] == "3 or more courses for non-majors") 
        {dataframe[i,6]<- -0.616}
      if(dataframe[i,7] == "an introductory or preparatory music program for bachelor's level work") 
        {dataframe[i,6]<- 0.443}
      if(dataframe[i,7] == "1 year of full-time coursework in a bachelor of music degree program (or equivalent)") 
        {dataframe[i,6]<- 0.055}
      if(dataframe[i,7] == "2 years of full-time coursework in a bachelor of music degree program (or equivalent)") 
        {dataframe[i,6]<- 2.801}
      if(dataframe[i,7] == "3 years of full-time coursework in a bachelor of music degree program (or equivalent)") 
        {dataframe[i,6]<- 0.387}
      if(dataframe[i,7] == "completion of a bachelor of music degree program (or equivalent)") 
        {dataframe[i,6]<- 1.390}
      if(dataframe[i,7] == "one or more graduate-level music courses or degrees") 
        {dataframe[i,6]<- 3.050}
    } else {
      dataframe[i,6]<-NA}
  }
  
  # Experience Composing: Column 8
    # Enter ONE value per question in the far right column that corresponds with the 
    # participant’s answer category
    # Keep any negative signs
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,8] == "have never composed any music") 
      {dataframe[i,8]<- 0}
    if(dataframe[i,8] == "have composed bits and pieces, but have never completed a piece of music") 
      {dataframe[i,8]<- 0.516}
    if(dataframe[i,8] == "have composed one or more completed pieces, but none have been performed") 
      {dataframe[i,8]<- 1.071}
    if(dataframe[i,8] == "have composed pieces as assignments or projects for one or more music classes; one or more of my pieces have been performed and/or recorded within the context of my educational environment") 
      {dataframe[i,8]<- 0.875}
    if(dataframe[i,8] == "have composed pieces that have been performed for a local audience") 
      {dataframe[i,8]<- 0.456}
    if(dataframe[i,8] == "have composed pieces that have been performed for a regional or national audiences (e.g. nationally known performer or ensemble, major concert venue, broadly distributed recording)") 
      {dataframe[i,8]<- -1.187}
    if(dataframe[i,8] == "") 
      {dataframe[i,8]<- NA}
  }
  
  # Number Live Concerts: Column 9
    # Enter ONE value per question in the far right column that corresponds with the 
    # participant’s answer category
    # Keep any negative signs
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,9] == "none") 
      {dataframe[i,9]<- 0}
    if(dataframe[i,9] == "1-4") 
      {dataframe[i,9]<- 1.839}
    if(dataframe[i,9] == "5-8") 
      {dataframe[i,9]<- 1.394}
    if(dataframe[i,9] == "9-12") 
      {dataframe[i,9]<- 1.713}
    if(dataframe[i,9] == "13 or more") 
      {dataframe[i,9]<- 1.610}
    if(dataframe[i,9] == "") 
      {dataframe[i,9]<- NA}
  }

  # Title: Column 10
    # Enter ONE value per question in the far right column that corresponds with the 
    # participant’s answer category
    # Keep any negative signs
  
  for(i in 1:nrow(dataframe)){
    if(dataframe[i,10] == "nonmusician") 
      {dataframe[i,10]<- 0}
    if(dataframe[i,10] == "music-loving nonmusician") 
      {dataframe[i,10]<- -0.553}
    if(dataframe[i,10] == "amateur musician") 
      {dataframe[i,10]<- 0.328}
    if(dataframe[i,10] == "serious amateur musican") 
      {dataframe[i,10]<- 1.589}
    if(dataframe[i,10] == "semiprofessional musician") 
      {dataframe[i,10]<- 1.460}
    if(dataframe[i,10] == "professional musician") 
      {dataframe[i,10]<- 2.940}
    if(dataframe[i,10] == "") 
      {dataframe[i,10]<- NA}
  }
  
  # Summing Responses
    # Add all selected values together in the far right column
    # Enter the total in the bottom right box
    # This value equals the Logit
  dataframe<-dataframe[,-7]
  dataframe[]<-lapply(dataframe, as.numeric)
  
    # sum columns (we deleted a column so now there's only 9)
    # minus a constant
  dataframe$Logit<-rowSums(dataframe[,1:9]) - 3.513 
     
  # Calculate Probability (P)
    # Use equation below or...
    # use the conversion chart to estimate P:
      # Logit: -2.20    P = 0.1
      # Logit: -1.39    P = 0.2  
      # Logit: -0.847   P = 0.3
      # Logit: -0.405   P = 0.4
      # Logit:  0       P = 0.5
      # Logit:  0.405   P = 0.6
      # Logit:  0.847   P = 0.7
      # Logit:  1.39    P = 0.8
      # Logit:  2.20    P = 0.9
    # The predicted probability value will be a number between 0 and 1. 
    # If the probability is greater than .50, then the participant has a 
    # greater than 50% likelihood of being classified as more musically sophisticated. 
    # If the probability is less than .50, then the participant has a 
    # less than 50% probability of being classified as more musically sophisticated, 
    # and would be classified as less musically sophisticated.
  
  dataframe$OMSI<-(2.781^(dataframe$Logit))/(1+2.781^(dataframe$Logit))
  
  dataframe$OMSI
}

ollen$Logit<-NA
ollen$OMSI<-NA
fixedQuestionnaires$OMSI<-fixOllen(ollen)

############################################################################################
#### STOMP-R ####
  # Rentfrow, P. J., Goldberg, L. R., & Levitin, D. J.(2011). 
  # The structure of musical preferences: A five-factor model. Journal of Personality and Social Psychology, 100, 1139-1157.
  
stomp<-questionnaires[,c(120:142)]
stomp[]<-lapply(stomp, tolower)

fixSTOMP<-function(dataframe){
  # Note: The soundtrack (column 23) and oldies (column 14) genres don’t load on a single factor. 
    # So you can remove those two genres from the STOMP-R or simply not score them
  
  cn = seq(1, ncol(dataframe)-5, 1) # all column names we care about
 
  # Convert to Numeric
  for(i in cn){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("dislike strongly", "dislike moderately", 
                                                  "dislike a little", 
                                                  "neither like nor dislike", 
                                                  "like a little", "like moderately", 
                                                  "like strongly"),
                                        labels = c("1", "2", "3", "4", "5", "6", "7")))
  }
  
  dataframe[]<-lapply(dataframe, as.numeric)
  
  # Mellow:
    # 6, 13, 11 (Dance/Electronica, New Age, World)
  m = c(6, 13, 11)
  mellow<-which(colnames(dataframe) == "Mellow")
  
  # Unpretentious: 
    # 16, 5, 20 (Pop, Country, Religious)
  u = c(16, 5, 20)
  unpretentious<-which(colnames(dataframe) == "Unpretentious")
  
  # Sophisticated:
    # 3, 12, 2, 7, 4, 9, 15 (Blues, Jazz, Bluegrass, Folk, Classical, Gospel, Opera)
  s = c(3, 12, 2, 7, 4, 9, 15)
  sophisticated<-which(colnames(dataframe) == "Sophisticated")
  
  # Intense: 
    # 21, 17, 1, 10 (Rock, Punk, Alternative, Heavy Metal)
  i = c(21, 17, 1, 10)
  intense<-which(colnames(dataframe) == "Intense")
  
  # Contemporary: 
   # 18, 22, 8, 19 (Rap/Hip Hop, Soul/R&B, Funk, Reggae)
  c = c(18, 22, 8, 19)
  contemporary<-which(colnames(dataframe) == "Contemporary")
  
  # Average Responses
  dataframe[,mellow]<-rowMeans(dataframe[,m], na.rm = T)
  dataframe[,unpretentious]<-rowMeans(dataframe[,u], na.rm = T)
  dataframe[,sophisticated]<-rowMeans(dataframe[,s], na.rm = T)
  dataframe[,intense]<-rowMeans(dataframe[,i], na.rm = T)
  dataframe[,contemporary]<-rowMeans(dataframe[,c], na.rm = T)
  
  for(i in c(mellow, unpretentious, sophisticated, intense, contemporary)){
    dataframe[,c(i)]<-ifelse(grepl("NaN", dataframe[,c(i)]),NA, dataframe[,c(i)])
  }
  
  dataframe[,c(mellow, unpretentious, sophisticated, intense, contemporary)]
}

new<-c("Mellow", "Unpretentious", "Sophisticated", "Intense", "Contemporary")
stomp[,new]<-NA

fixedQuestionnaires<-cbind(fixedQuestionnaires, fixSTOMP(stomp))

############################################################################################
##### IRI ####
  # Davis, M. H. (1983). 
  # Measuring individual differences in empathy: Evidence for a multidimensional approach. Journal of Personality and Social Psychology, 44(1), 113.

iri<-questionnaires[,c(143:170)]
iri[]<-lapply(iri, tolower)

fixIRI<-function(dataframe){

  rc = c(3, 4, 7, 12, 13, 14, 15, 18, 19) # reverse coded
  nc = which(! 1:28 %in% rc) # normal coded
  
  # Normal Coding
  for(i in nc){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("a\ndoes not\ndescribe \nme well", 
                                                  "b\n ", "c\n ", "d\n ",
                                                  "e\ndescribes\nme very\nwell"),
                                       labels = c("0", "1", "2", "3", "4")))
  }
  
  # Reverse Coding
  for(i in rc){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("a\ndoes not\ndescribe \nme well", 
                                                  "b\n ", "c\n ", "d\n ",
                                                  "e\ndescribes\nme very\nwell"),
                                       labels = c("4", "3", "2", "1", "0")))
  }
  
  dataframe[]<-lapply(dataframe, as.numeric)
  
  # PT:
  pt = c(3, 8, 11, 15, 21, 25, 28)
  PT<-which(colnames(dataframe) == "PT")
  
  # FS: 
  fs = c(1, 5, 7, 12, 16, 23, 26)
  FS<-which(colnames(dataframe) == "FS")
  
  # EC:
  ec = c(2, 4, 9, 14, 18, 20, 22)
  EC<-which(colnames(dataframe) == "EC")
  
  # PD: 
  pd = c(6, 10, 13, 17, 19, 24, 27)
  PD<-which(colnames(dataframe) == "PD")
  
  # Average Responses
  dataframe[,PT]<-rowMeans(dataframe[,pt], na.rm = T)
  dataframe[,FS]<-rowMeans(dataframe[,fs], na.rm = T)
  dataframe[,EC]<-rowMeans(dataframe[,ec], na.rm = T)
  dataframe[,PD]<-rowMeans(dataframe[,pd], na.rm = T)

  for(i in c(PT, FS, EC, PD)){
    dataframe[,c(i)]<-ifelse(grepl("NaN", dataframe[,c(i)]), NA, dataframe[,c(i)])
  }
  
  dataframe[,c(PT, FS, EC, PD)]
}

new<-c("PT", "FS", "EC", "PD")
iri[,new]<-NA

fixedQuestionnaires<-cbind(fixedQuestionnaires, fixIRI(iri))

############################################################################################
#### Big Five ####
  # John, O. P., & Srivastava, S. (1999). 
  # The Big Five trait taxonomy: History, measurement, and theoretical perspectives. Handbook of personality: Theory and research, 2(1999), 102-138.

bigfive<-questionnaires[,c(171:214)]
bigfive[]<-lapply(bigfive, tolower)

fixBigFive<-function(dataframe){
  
  rc = c(2, 6, 8, 9, 12, 18, 21, 23, 24, 27, 31, 34, 35, 37, 41, 43) # reverse coded
  nc = which(!1:44 %in% rc) # normal coded
  
  # Normal Coding
  for(i in nc){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("disagree strongly", "disagree a little",
                                                  "neither agree nor disagree",
                                                  "agree a little", "agree strongly"),
                                       labels = c("1", "2", "3", "4", "5")))
  }
  
  # Reverse Coding
  for(i in rc){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("disagree strongly", "disagree a little",
                                                  "neither agree nor disagree",
                                                  "agree a little", "agree strongly"),
                                       labels = c("5", "4", "3", "2", "1")))
  }
  
  dataframe[]<-lapply(dataframe, as.numeric)
  
  # Extraversion:
  e = c(1, 6, 11, 16, 21, 26, 31, 36)
  E<-which(colnames(dataframe) == "E")
  
  # Agreeableness: 
  a = c(2, 7, 12, 17, 22, 27, 32, 37, 42)
  A<-which(colnames(dataframe) == "A")
  
  # Conscientiousness:
  c = c(3, 8, 13, 18, 23, 28, 33, 38, 43)
  C<-which(colnames(dataframe) == "C")
  
  # Neuroticism: 
  n = c(4, 9, 14, 19, 24, 29, 34, 39)
  N<-which(colnames(dataframe) == "N")
  
  # Openness: 
  o = c(5, 10, 15, 20, 25, 30, 35, 40, 41, 44)
  O<-which(colnames(dataframe) == "O")
  
  # Average Responses
  dataframe[,E]<-rowMeans(dataframe[,e], na.rm = T)
  dataframe[,A]<-rowMeans(dataframe[,a], na.rm = T)
  dataframe[,C]<-rowMeans(dataframe[,c], na.rm = T)
  dataframe[,N]<-rowMeans(dataframe[,n], na.rm = T)
  dataframe[,O]<-rowMeans(dataframe[,o], na.rm = T)
  
  for(i in c(E, A, C, N, O)){
    dataframe[,c(i)]<-ifelse(grepl("NaN", dataframe[,c(i)]), NA, dataframe[,c(i)])
  }
  
  dataframe[,c(E, A, C, N, O)]
}

new<-c("E", "A", "C", "N", "O")
bigfive[,new]<-NA

fixedQuestionnaires<-cbind(fixedQuestionnaires, fixBigFive(bigfive))

############################################################################################
#### AIMS ####
  # Sandstrom, G. M., & Russo, F. A. (2013). 
  # Absorption in music: Development of a scale to identify individuals with strong emotional responses to music. Psychology of Music, 41(2), 216-228.

aims<-questionnaires[,c(215:248)]
aims[]<-lapply(aims, tolower)

fixAIMS<-function(dataframe){

  aimsSum<-which(colnames(dataframe) == "AIMS")
  
  for(i in 1:ncol(dataframe)){
    dataframe[,i]<-as.character(factor(dataframe[,i], 
                                       levels = c("strongly disagree", "somewhat disagree",
                                                  "neural (neither agree nor disagree)",
                                                  "somewhat agree", "strongly agree"),
                                       labels = c("1", "2", "3", "4", "5")))
  }
  
  dataframe[]<-lapply(dataframe, as.numeric)
  
  # Sum Responses
  dataframe[,aimsSum]<-rowSums(dataframe[,-aimsSum])
  
  dataframe[,aimsSum]
}

aims$AIMS<-NA
fixedQuestionnaires$AIMS<-fixAIMS(aims)

############################################################################################

# duplicates 
n_occur<-data.frame(table(fixedQuestionnaires$SubjectID))
duplicates<-fixedQuestionnaires[fixedQuestionnaires$SubjectID %in% n_occur$Var1[n_occur$Freq > 1],]

# ones to remove
# 117: 18d: 140.254.98.239
# 144: 144: 140.254.98.239
# 193: 140.254.98.97
# 91: 24e: 140.254.98.143
# 92: 25e: 140.254.98.46


#### Write out fixed questionnaires!! ####
write.csv(fixedQuestionnaires, file = "Fixed Questionnaires.csv", row.names = F)
