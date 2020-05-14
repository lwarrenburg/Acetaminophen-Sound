setwd('/Users/home/Desktop/Tylenol/Analysis')
library(reshape2)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)
library(data.table)
library("RColorBrewer")
source("Acetaminophen Questionnaires.R")

##############################################################################################
#### Induced Emotions ####
##############################################################################################
induced<-read.csv("Induced Emotion.csv", header = T)
induced<-induced[,-c(1:17)]

#### Fix Column Names ####
colnames(induced)<-c(
  # Participant ID
  "ID", "Instructions",
  
  # Fearful Music 1: F4
  "FearMusic1_TimingFirst", "FearMusic1_TimingLast", "FearMusic1_TimingSubmit", "FearMusic1_TimingClickCount",
  "FearMusic1_Positive", "FearMusic1_Negative", "FearMusic1_Emotion", "FearMusic1_OtherE",
  "FearMusic1_StrongEmotion", "FearMusic1_OtherSE", "FearMusic1_Familiarity",
  
  # Fearful Music 2: F2
  "FearMusic2_TimingFirst", "FearMusic2_TimingLast", "FearMusic2_TimingSubmit", "FearMusic2_TimingClickCount",
  "FearMusic2_Positive", "FearMusic2_Negative", "FearMusic2_Emotion", "FearMusic2_OtherE",
  "FearMusic2_StrongEmotion", "FearMusic2_OtherSE", "FearMusic2_Familiarity",
  
  # Sad Music 1: S3
  "SadMusic1_TimingFirst", "SadMusic1_TimingLast", "SadMusic1_TimingSubmit", "SadMusic1_TimingClickCount",
  "SadMusic1_Positive", "SadMusic1_Negative", "SadMusic1_Emotion", "SadMusic1_OtherE",
  "SadMusic1_StrongEmotion", "SadMusic1_OtherSE", "SadMusic1_Familiarity",
  
  # Sad Music 2: S4
  "SadMusic2_TimingFirst", "SadMusic2_TimingLast", "SadMusic2_TimingSubmit", "SadMusic2_TimingClickCount",
  "SadMusic2_Positive", "SadMusic2_Negative", "SadMusic2_Emotion", "SadMusic2_OtherE",
  "SadMusic2_StrongEmotion", "SadMusic2_OtherSE", "SadMusic2_Familiarity",
  
  # Happy Music 1: H2
  "HappyMusic1_TimingFirst", "HappyMusic1_TimingLast", "HappyMusic1_TimingSubmit", "HappyMusic1_TimingClickCount",
  "HappyMusic1_Positive", "HappyMusic1_Negative", "HappyMusic1_Emotion", "HappyMusic1_OtherE",
  "HappyMusic1_StrongEmotion", "HappyMusic1_OtherSE", "HappyMusic1_Familiarity",
  
  # Happy Music 2: H4
  "HappyMusic2_TimingFirst", "HappyMusic2_TimingLast", "HappyMusic2_TimingSubmit", "HappyMusic2_TimingClickCount",
  "HappyMusic2_Positive", "HappyMusic2_Negative", "HappyMusic2_Emotion", "HappyMusic2_OtherE",
  "HappyMusic2_StrongEmotion", "HappyMusic2_OtherSE", "HappyMusic2_Familiarity",
  
  # Tender Music 1: T2
  "TenderMusic1_TimingFirst", "TenderMusic1_TimingLast", "TenderMusic1_TimingSubmit", "TenderMusic1_TimingClickCount",
  "TenderMusic1_Positive", "TenderMusic1_Negative", "TenderMusic1_Emotion", "TenderMusic1_OtherE",
  "TenderMusic1_StrongEmotion", "TenderMusic1_OtherSE", "TenderMusic1_Familiarity",
  
  # Tender Music 2: T1
  "TenderMusic2_TimingFirst", "TenderMusic2_TimingLast", "TenderMusic2_TimingSubmit", "TenderMusic2_TimingClickCount",
  "TenderMusic2_Positive", "TenderMusic2_Negative", "TenderMusic2_Emotion", "TenderMusic2_OtherE",
  "TenderMusic2_StrongEmotion", "TenderMusic2_OtherSE", "TenderMusic2_Familiarity",
  
  # Negative Valence, High Arousal -- Human Sounds 1: 277
  "NegValHighArHuman_TimingFirst", "NegValHighArHuman_TimingLast", "NegValHighArHuman_TimingSubmit", "NegValHighArHuman_TimingClickCount",
  "NegValHighArHuman_Positive", "NegValHighArHuman_Negative", "NegValHighArHuman_Emotion", "NegValHighArHuman_OtherE",
  "NegValHighArHuman_StrongEmotion", "NegValHighArHuman_OtherSE",
  
  # Negative Valence, High Arousal -- Non-human Sounds 2: 712
  "NegValHighArNon_TimingFirst", "NegValHighArNon_TimingLast", "NegValHighArNon_TimingSubmit", "NegValHighArNon_TimingClickCount",
  "NegValHighArNon_Positive", "NegValHighArNon_Negative", "NegValHighArNon_Emotion", "NegValHighArNon_OtherE",
  "NegValHighArNon_StrongEmotion", "NegValHighArNon_OtherSE",
  
  # Negative Valence, Low Arousal -- Human Sounds 1: 293
  "NegValLowArHuman_TimingFirst", "NegValLowArHuman_TimingLast", "NegValLowArHuman_TimingSubmit", "NegValLowArHuman_TimingClickCount",
  "NegValLowArHuman_Positive", "NegValLowArHuman_Negative", "NegValLowArHuman_Emotion", "NegValLowArHuman_OtherE",
  "NegValLowArHuman_StrongEmotion", "NegValLowArHuman_OtherSE",
  
  # Negative Valence, Low Arousal -- Non-human Sounds 1: 502
  "NegValLowArNon_TimingFirst", "NegValLowArNon_TimingLast", "NegValLowArNon_TimingSubmit", "NegValLowArNon_TimingClickCount",
  "NegValLowArNon_Positive", "NegValLowArNon_Negative", "NegValLowArNon_Emotion", "NegValLowArNon_OtherE",
  "NegValLowArNon_StrongEmotion", "NegValLowArNon_OtherSE",
  
  # Neutral -- Human Sounds 1: 245
  "NeutralHuman_TimingFirst", "NeutralHuman_TimingLast", "NeutralHuman_TimingSubmit", "NeutralHuman_TimingClickCount",
  "NeutralHuman_Positive", "NeutralHuman_Negative", "NeutralHuman_Emotion", "NeutralHuman_OtherE",
  "NeutralHuman_StrongEmotion", "NeutralHuman_OtherSE",
  
  # Neutral -- Non-human Sounds 1: 500
  "NeutralNon_TimingFirst", "NeutralNon_TimingLast", "NeutralNon_TimingSubmit", "NeutralNon_TimingClickCount",
  "NeutralNon_Positive", "NeutralNon_Negative", "NeutralNon_Emotion", "NeutralNon_OtherE",
  "NeutralNon_StrongEmotion", "NeutralNon_OtherSE",
  
  # Positive Valence, High Arousal -- Human Sounds 2: 353
  "PosValHighArHuman_TimingFirst", "PosValHighArHuman_TimingLast", "PosValHighArHuman_TimingSubmit", "PosValHighArHuman_TimingClickCount",
  "PosValHighArHuman_Positive", "PosValHighArHuman_Negative", "PosValHighArHuman_Emotion", "PosValHighArHuman_OtherE",
  "PosValHighArHuman_StrongEmotion", "PosValHighArHuman_OtherSE",
  
  # Positive Valence, High Arousal -- Non-human Sounds 1: 363
  "PosValHighArNon_TimingFirst", "PosValHighArNon_TimingLast", "PosValHighArNon_TimingSubmit", "PosValHighArNon_TimingClickCount",
  "PosValHighArNon_Positive", "PosValHighArNon_Negative", "PosValHighArNon_Emotion", "PosValHighArNon_OtherE",
  "PosValHighArNon_StrongEmotion", "PosValHighArNon_OtherSE",
  
  # Positive Valence, Low Arousal -- Human Sounds 1: 230
  "PosValLowArHuman_TimingFirst", "PosValLowArHuman_TimingLast", "PosValLowArHuman_TimingSubmit", "PosValLowArHuman_TimingClickCount",
  "PosValLowArHuman_Positive", "PosValLowArHuman_Negative", "PosValLowArHuman_Emotion", "PosValLowArHuman_OtherE",
  "PosValLowArHuman_StrongEmotion", "PosValLowArHuman_OtherSE",
  
  # Positive Valence, Low Arousal -- Non-human Sounds 2: 150
  "PosValLowArNon_TimingFirst", "PosValLowArNon_TimingLast", "PosValLowArNon_TimingSubmit", "PosValLowArNon_TimingClickCount",
  "PosValLowArNon_Positive", "PosValLowArNon_Negative", "PosValLowArNon_Emotion", "PosValLowArNon_OtherE",
  "PosValLowArNon_StrongEmotion", "PosValLowArNon_OtherSE",
  
  # End
  "QuestionsForExperimenter"
)

#### Check Names ####
check<-t(induced[1,])
induced<-induced[-c(1:2),]

############################################################################################
#### Participant ID ####

# Clean Induced ID
fixID<-function(dataframe){
  dataframe$ID<-as.character(dataframe$ID)
  dataframe$ID<-toupper(dataframe$ID)
  
  x<-substr(dataframe$ID, 1, 1)
  y<-substr(dataframe$ID, 2, 20)
  dataframe$ID<-ifelse(grepl('[A-z]', x), paste0(y,x), paste0(x,y))
  dataframe$ID<-gsub("[^[:alnum:]]", "", dataframe$ID)
  
  dataframe$ID
}
inducedCleanID<-setNames(data.frame(fixID(induced)), "OldID")

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
  originalDF
}

induced<-cbind(participantIDandDrugCode(inducedCleanID, idCodes, drugcodes), induced[,-1])
#fixedInduced<-setNames(data.frame(participantIDandDrugCode(inducedCleanID, idCodes, drugcodes)), c("SubjectID", "DrugCode", "DrugPlacebo"))

############################################################################################
##### Get Rid of Timing Info ####
induced<-induced[,which(!grepl("Timing|Instructions", colnames(induced)))]
colnames<-as.data.frame(colnames(induced))

############################################################################################
##### Write out wide dataframe ####
write.csv(induced, file = "Fixed Induced Wide.csv", row.names = F)

############################################################################################
##### Make Long Dataframe ####

FearMusic1<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearMusic1')) %>% mutate(Stimulus = "Fear Music 1")
FearMusic2<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearMusic2')) %>% mutate(Stimulus = "Fear Music 2")

SadMusic1<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadMusic1')) %>% mutate(Stimulus = "Sad Music 1")
SadMusic2<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadMusic2')) %>% mutate(Stimulus = "Sad Music 2")

HappyMusic1<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappyMusic1')) %>% mutate(Stimulus = "Happy Music 1")
HappyMusic2<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappyMusic2')) %>% mutate(Stimulus = "Happy Music 2")

TenderMusic1<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic1')) %>% mutate(Stimulus = "Tender Music 1")
TenderMusic2<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic2')) %>% mutate(Stimulus = "Tender Music 2")

NegValHighArHuman<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValHighArHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence High-Arousal Human") 
NegValHighArNon<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValHighArNon')) %>% mutate(NegValHighArNon_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence High-Arousal Non-human")
NegValLowArHuman<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValLowArHuman')) %>% mutate(NegValLowArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence Low-Arousal Human")
NegValLowArNon<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValLowArNon')) %>% mutate(NegValLowArNon_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence Low-Arousal Non-human")

NeutralHuman<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralHuman')) %>% mutate(NeutralHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Human")
NeutralNon<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralNon')) %>% mutate(NeutralNon_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Non-human")

PosValHighArHuman<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValHighArHuman')) %>% mutate(PosValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence High-Arousal Human")
PosValHighArNon<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValHighArNon')) %>% mutate(PosValHighArNon_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence High-Arousal Non-human")
PosValLowArHuman<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValLowArHuman')) %>% mutate(PosValLowArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence Low-Arousal Human")
PosValLowArNon<-induced %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValLowArNon')) %>% mutate(PosValLowArNon_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence Low-Arousal Non-human")

fixedInduced<-rbindlist(list(FearMusic1, FearMusic2, SadMusic1, SadMusic2, 
                             HappyMusic1, HappyMusic2, TenderMusic1, TenderMusic2,
                             NegValHighArHuman, NegValHighArNon, NegValLowArHuman, NegValLowArNon, 
                             NeutralHuman, NeutralNon, 
                             PosValHighArHuman, PosValHighArNon, PosValLowArHuman, PosValLowArNon))
fixedInduced$Locus<-"Induced"
fixedInduced$Arousal<-"Not Measured"
colnames(fixedInduced)<-c("FixedID", "DrugCode", "DrugPlacebo", 
                          "Positive", "Negative", "Emotion", "OtherE", 
                          "StrongEmotion", "OtherSE", "Familiarity", "Stimulus", "Locus", "Arousal")
fixedInduced<-fixedInduced[,c(1:3,12,11,4:5,13,6:10)]
fixedInduced[]<-lapply(fixedInduced, as.character)

############################################################################################
##### Fix Positive and Negative Columns ####
for(i in 1:nrow(fixedInduced)){
  if(grepl("0: I feel little", fixedInduced$Positive[i])){fixedInduced$Positive[i]<-"0"}
  if(grepl("10: I feel an extreme", fixedInduced$Positive[i])){fixedInduced$Positive[i]<-"10"}
  
  if(grepl("0: I feel little", fixedInduced$Negative[i])){fixedInduced$Negative[i]<-"0"}
  if(grepl("10: I feel an extreme", fixedInduced$Negative[i])){fixedInduced$Negative[i]<-"10"}
}
fixedInduced$Positive<-as.numeric(fixedInduced$Positive)
fixedInduced$Negative<-as.numeric(fixedInduced$Negative)

############################################################################################
##### Fix Emotion Columns ####

list<-c("Anger", "Anxious", "Bored", "Disgusted", "Excited", "Fearful", 
        "Grieved", "Happy", "Invigorated", "Joyful", "Nostalgic", "Peaceful",
        "Power", "Relaxed", "Sad", "Softhearted", "Surprised", "Sympathetic",
        "Tender", "Transcendent", "Tension", "Wonder", "Neutral", "Other")

fixedInduced[,list]<-NA

for(i in 1:nrow(fixedInduced)){
  fixedInduced$Anger[i] = as.numeric(grepl("Anger", fixedInduced$Emotion[i])) + 
                          as.numeric(grepl("Anger", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Anxious[i] = as.numeric(grepl("Anxious", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Anxious", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Bored[i] = as.numeric(grepl("Bored", fixedInduced$Emotion[i])) + 
                          as.numeric(grepl("Bored", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Disgusted[i] = as.numeric(grepl("Disgusted", fixedInduced$Emotion[i])) + 
                              as.numeric(grepl("Disgusted", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Excited[i] = as.numeric(grepl("Excited", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Excited", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Fearful[i] = as.numeric(grepl("Fearful", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Fearful", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Grieved[i] = as.numeric(grepl("Grieved", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Grieved", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Happy[i] = as.numeric(grepl("Happy", fixedInduced$Emotion[i])) + 
                          as.numeric(grepl("Happy", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Invigorated[i] = as.numeric(grepl("Invigorated", fixedInduced$Emotion[i])) + 
                                as.numeric(grepl("Invigorated", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Joyful[i] = as.numeric(grepl("Joyful", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Joyful", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Nostalgic[i] = as.numeric(grepl("Nostalgic", fixedInduced$Emotion[i])) + 
                              as.numeric(grepl("Nostalgic", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Peaceful[i] = as.numeric(grepl("Peaceful", fixedInduced$Emotion[i])) + 
                             as.numeric(grepl("Peaceful", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Power[i] = as.numeric(grepl("Power", fixedInduced$Emotion[i])) + 
                          as.numeric(grepl("Power", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Relaxed[i] = as.numeric(grepl("Relaxed", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Relaxed", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Sad[i] = as.numeric(grepl("Sad", fixedInduced$Emotion[i])) + 
                        as.numeric(grepl("Sad", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Softhearted[i] = as.numeric(grepl("Softhearted", fixedInduced$Emotion[i])) + 
                                as.numeric(grepl("Softhearted", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Surprised[i] = as.numeric(grepl("Surprised", fixedInduced$Emotion[i])) + 
                              as.numeric(grepl("Surprised", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Sympathetic[i] = as.numeric(grepl("Sympathetic", fixedInduced$Emotion[i])) + 
                                as.numeric(grepl("Sympathetic", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Tender[i] = as.numeric(grepl("Tender", fixedInduced$Emotion[i])) + 
                           as.numeric(grepl("Tender", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Transcendent[i] = as.numeric(grepl("Transcendent", fixedInduced$Emotion[i])) + 
                                 as.numeric(grepl("Transcendent", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Tension[i] = as.numeric(grepl("Tension", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Tension", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Wonder[i] = as.numeric(grepl("Wonder", fixedInduced$Emotion[i])) + 
                           as.numeric(grepl("Wonder", fixedInduced$StrongEmotion[i]))
  
  fixedInduced$Neutral[i] = as.numeric(grepl("Neutral", fixedInduced$Emotion[i])) + 
                            as.numeric(grepl("Neutral", fixedInduced$StrongEmotion[i]))

  fixedInduced$Other[i] = tolower(as.character(fixedInduced$OtherE[i]))
}

for(i in 1:nrow(fixedInduced)){if(fixedInduced$Other[i] == ""){fixedInduced$Other[i]<-NA}}

############################################################################################
##### Familiarity ####
for(i in 1:nrow(fixedInduced)){
  if(grepl("Not", fixedInduced$Familiarity[i])){fixedInduced$Familiarity[i]<-"0"}
  if(grepl("Somewhat", fixedInduced$Familiarity[i])){fixedInduced$Familiarity[i]<-"1"}
  if(grepl("Very", fixedInduced$Familiarity[i])){fixedInduced$Familiarity[i]<-"2"}
}
fixedInduced$Familiarity<-as.numeric(fixedInduced$Familiarity)

############################################################################################
#### Write out fixed induced!! ####
fixedInduced<-fixedInduced[,-c(9:12)]
write.csv(fixedInduced, file = "Fixed Induced.csv", row.names = F)

############################################################################################
#### Combine Questionnaires and Induced ####
colnames(fixedQuestionnaires)[1]<-"FixedID"
combined<-merge(fixedQuestionnaires, fixedInduced, by=c("FixedID","DrugCode", "DrugPlacebo"))

write.csv(combined, file = "Induced With Questionnaires.csv", row.names = F)

