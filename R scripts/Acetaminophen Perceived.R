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
# source("Acetaminophen Questionnaires.R")
# source("Acetaminophen Induced.R")

##############################################################################################
#### Perceived Emotions ####
##############################################################################################
perceived<-read.csv("Perceived Emotion.csv", header = T)
perceived<-perceived[,-c(1:17)]
check<-t(perceived[1,])

#### Fix Column Names ####
colnames(perceived)<-c(
  # Participant ID
  "ID", "Instructions",
  
  # Fear Music 1: 012
  "FearMusic1_TimingFirst", "FearMusic1_TimingLast", "FearMusic1_TimingSubmit", "FearMusic1_TimingClickCount",
  "FearMusic1_Positive", "FearMusic1_Negative", "FearMusic1_Arousal",
  "FearMusic1_Emotion", "FearMusic1_OtherE", "FearMusic1_StrongEmotion", "FearMusic1_OtherSE", "FearMusic1_Familiarity",
  
  # Fear Music 2: 014
 "FearMusic2_TimingFirst", "FearMusic2_TimingLast", "FearMusic2_TimingSubmit", "FearMusic2_TimingClickCount",
 "FearMusic2_Positive", "FearMusic2_Negative", "FearMusic2_Arousal",
 "FearMusic2_Emotion", "FearMusic2_OtherE", "FearMusic2_StrongEmotion", "FearMusic2_OtherSE", "FearMusic2_Familiarity",
  
  # Fear Music 3: 015
 "FearMusic3_TimingFirst", "FearMusic3_TimingLast", "FearMusic3_TimingSubmit", "FearMusic3_TimingClickCount",
 "FearMusic3_Positive", "FearMusic3_Negative", "FearMusic3_Arousal",
 "FearMusic3_Emotion", "FearMusic3_OtherE", "FearMusic3_StrongEmotion", "FearMusic3_OtherSE", "FearMusic3_Familiarity",
  
  # Happy Music 1: 021
 "HappyMusic1_TimingFirst", "HappyMusic1_TimingLast", "HappyMusic1_TimingSubmit", "HappyMusic1_TimingClickCount",
 "HappyMusic1_Positive", "HappyMusic1_Negative", "HappyMusic1_Arousal",
 "HappyMusic1_Emotion", "HappyMusic1_OtherE", "HappyMusic1_StrongEmotion", "HappyMusic1_OtherSE", "HappyMusic1_Familiarity",
  
  # Happy Music 2: 023
 "HappyMusic2_TimingFirst", "HappyMusic2_TimingLast", "HappyMusic2_TimingSubmit", "HappyMusic2_TimingClickCount",
 "HappyMusic2_Positive", "HappyMusic2_Negative", "HappyMusic2_Arousal",
 "HappyMusic2_Emotion", "HappyMusic2_OtherE", "HappyMusic2_StrongEmotion", "HappyMusic2_OtherSE", "HappyMusic2_Familiarity",
  
  # Happy Music 3: 024
 "HappyMusic3_TimingFirst", "HappyMusic3_TimingLast", "HappyMusic3_TimingSubmit", "HappyMusic3_TimingClickCount",
 "HappyMusic3_Positive", "HappyMusic3_Negative", "HappyMusic3_Arousal",
 "HappyMusic3_Emotion", "HappyMusic3_OtherE", "HappyMusic3_StrongEmotion", "HappyMusic3_OtherSE", "HappyMusic3_Familiarity",
  
  # Sad Music 1: 033
 "SadMusic1_TimingFirst", "SadMusic1_TimingLast", "SadMusic1_TimingSubmit", "SadMusic1_TimingClickCount",
 "SadMusic1_Positive", "SadMusic1_Negative", "SadMusic1_Arousal",
 "SadMusic1_Emotion", "SadMusic1_OtherE", "SadMusic1_StrongEmotion", "SadMusic1_OtherSE", "SadMusic1_Familiarity",
  
  # Sad Music 2: 032
 "SadMusic2_TimingFirst", "SadMusic2_TimingLast", "SadMusic2_TimingSubmit", "SadMusic2_TimingClickCount",
 "SadMusic2_Positive", "SadMusic2_Negative", "SadMusic2_Arousal",
 "SadMusic2_Emotion", "SadMusic2_OtherE", "SadMusic2_StrongEmotion", "SadMusic2_OtherSE", "SadMusic2_Familiarity",
  
  # Sad Music 3: 035
 "SadMusic3_TimingFirst", "SadMusic3_TimingLast", "SadMusic3_TimingSubmit", "SadMusic3_TimingClickCount",
 "SadMusic3_Positive", "SadMusic3_Negative", "SadMusic3_Arousal",
 "SadMusic3_Emotion", "SadMusic3_OtherE", "SadMusic3_StrongEmotion", "SadMusic3_OtherSE", "SadMusic3_Familiarity",
  
  # Tender Music 1: 042
 "TenderMusic1_TimingFirst", "TenderMusic1_TimingLast", "TenderMusic1_TimingSubmit", "TenderMusic1_TimingClickCount",
 "TenderMusic1_Positive", "TenderMusic1_Negative", "TenderMusic1_Arousal",
 "TenderMusic1_Emotion", "TenderMusic1_OtherE", "TenderMusic1_StrongEmotion", "TenderMusic1_OtherSE", "TenderMusic1_Familiarity",
  
  # Tender Music 2: 043
 "TenderMusic2_TimingFirst", "TenderMusic2_TimingLast", "TenderMusic2_TimingSubmit", "TenderMusic2_TimingClickCount",
 "TenderMusic2_Positive", "TenderMusic2_Negative", "TenderMusic2_Arousal",
 "TenderMusic2_Emotion", "TenderMusic2_OtherE", "TenderMusic2_StrongEmotion", "TenderMusic2_OtherSE", "TenderMusic2_Familiarity",
  
  # Tender Music 3: 045
 "TenderMusic3_TimingFirst", "TenderMusic3_TimingLast", "TenderMusic3_TimingSubmit", "TenderMusic3_TimingClickCount",
 "TenderMusic3_Positive", "TenderMusic3_Negative", "TenderMusic3_Arousal",
 "TenderMusic3_Emotion", "TenderMusic3_OtherE", "TenderMusic3_StrongEmotion", "TenderMusic3_OtherSE", "TenderMusic3_Familiarity",
  
  # Tender Music 4: 044
 "TenderMusic4_TimingFirst", "TenderMusic4_TimingLast", "TenderMusic4_TimingSubmit", "TenderMusic4_TimingClickCount",
 "TenderMusic4_Positive", "TenderMusic4_Negative", "TenderMusic4_Arousal",
 "TenderMusic4_Emotion", "TenderMusic4_OtherE", "TenderMusic4_StrongEmotion", "TenderMusic4_OtherSE", "TenderMusic4_Familiarity",
  
  # Tender Music 5: 047
 "TenderMusic5_TimingFirst", "TenderMusic5_TimingLast", "TenderMusic5_TimingSubmit", "TenderMusic5_TimingClickCount",
 "TenderMusic5_Positive", "TenderMusic5_Negative", "TenderMusic5_Arousal",
 "TenderMusic5_Emotion", "TenderMusic5_OtherE", "TenderMusic5_StrongEmotion", "TenderMusic5_OtherSE", "TenderMusic5_Familiarity",
  
  # Tender Music 6: 050
 "TenderMusic6_TimingFirst", "TenderMusic6_TimingLast", "TenderMusic6_TimingSubmit", "TenderMusic6_TimingClickCount",
 "TenderMusic6_Positive", "TenderMusic6_Negative", "TenderMusic6_Arousal",
 "TenderMusic6_Emotion", "TenderMusic6_OtherE", "TenderMusic6_StrongEmotion", "TenderMusic6_OtherSE", "TenderMusic6_Familiarity",
  
  # Fear Speech 1: 1064_IEO_FEA_HI
 "FearSpeech1_TimingFirst", "FearSpeech1_TimingLast", "FearSpeech1_TimingSubmit", "FearSpeech1_TimingClickCount",
 "FearSpeech1_Positive", "FearSpeech1_Negative", "FearSpeech1_Arousal",
 "FearSpeech1_Emotion", "FearSpeech1_OtherE", "FearSpeech1_StrongEmotion", "FearSpeech1_OtherSE", 
  
  # Fear Speech 2: 1036_TSI_FEA_XX
 "FearSpeech2_TimingFirst", "FearSpeech2_TimingLast", "FearSpeech2_TimingSubmit", "FearSpeech2_TimingClickCount",
 "FearSpeech2_Positive", "FearSpeech2_Negative", "FearSpeech2_Arousal",
 "FearSpeech2_Emotion", "FearSpeech2_OtherE", "FearSpeech2_StrongEmotion", "FearSpeech2_OtherSE", 
  
  # Fear Speech 3: 1036_WSI_FEA_XX
 "FearSpeech3_TimingFirst", "FearSpeech3_TimingLast", "FearSpeech3_TimingSubmit", "FearSpeech3_TimingClickCount",
 "FearSpeech3_Positive", "FearSpeech3_Negative", "FearSpeech3_Arousal",
 "FearSpeech3_Emotion", "FearSpeech3_OtherE", "FearSpeech3_StrongEmotion", "FearSpeech3_OtherSE", 
  
  # Happy Speech 1: 1038_IEO_HAP_HI
 "HappySpeech1_TimingFirst", "HappySpeech1_TimingLast", "HappySpeech1_TimingSubmit", "HappySpeech1_TimingClickCount",
 "HappySpeech1_Positive", "HappySpeech1_Negative", "HappySpeech1_Arousal",
 "HappySpeech1_Emotion", "HappySpeech1_OtherE", "HappySpeech1_StrongEmotion", "HappySpeech1_OtherSE", 
  
  # Happy Speech 2: 1049_ITS_HAP_XX
 "HappySpeech2_TimingFirst", "HappySpeech2_TimingLast", "HappySpeech2_TimingSubmit", "HappySpeech2_TimingClickCount",
 "HappySpeech2_Positive", "HappySpeech2_Negative", "HappySpeech2_Arousal",
 "HappySpeech2_Emotion", "HappySpeech2_OtherE", "HappySpeech2_StrongEmotion", "HappySpeech2_OtherSE", 
  
  # Happy Speech 3: 1064_TIE_HAP_XX
 "HappySpeech3_TimingFirst", "HappySpeech3_TimingLast", "HappySpeech3_TimingSubmit", "HappySpeech3_TimingClickCount",
 "HappySpeech3_Positive", "HappySpeech3_Negative", "HappySpeech3_Arousal",
 "HappySpeech3_Emotion", "HappySpeech3_OtherE", "HappySpeech3_StrongEmotion", "HappySpeech3_OtherSE", 
  
  # Sad Speech 1: 1029_IEO_SAD_HI
 "SadSpeech1_TimingFirst", "SadSpeech1_TimingLast", "SadSpeech1_TimingSubmit", "SadSpeech1_TimingClickCount",
 "SadSpeech1_Positive", "SadSpeech1_Negative", "SadSpeech1_Arousal",
 "SadSpeech1_Emotion", "SadSpeech1_OtherE", "SadSpeech1_StrongEmotion", "SadSpeech1_OtherSE", 
  
  # Sad Speech 2: 1028_WSI_SAD_XX
 "SadSpeech2_TimingFirst", "SadSpeech2_TimingLast", "SadSpeech2_TimingSubmit", "SadSpeech2_TimingClickCount",
 "SadSpeech2_Positive", "SadSpeech2_Negative", "SadSpeech2_Arousal",
 "SadSpeech2_Emotion", "SadSpeech2_OtherE", "SadSpeech2_StrongEmotion", "SadSpeech2_OtherSE", 
  
  # Sad Speech 3: 1007_TAI_SAD_XX
 "SadSpeech3_TimingFirst", "SadSpeech3_TimingLast", "SadSpeech3_TimingSubmit", "SadSpeech3_TimingClickCount",
 "SadSpeech3_Positive", "SadSpeech3_Negative", "SadSpeech3_Arousal",
 "SadSpeech3_Emotion", "SadSpeech3_OtherE", "SadSpeech3_StrongEmotion", "SadSpeech3_OtherSE", 
 
  # Neutral Speech 1: 1074_IOM_NEU_XX
 "NeutralSpeech1_TimingFirst", "NeutralSpeech1_TimingLast", "NeutralSpeech1_TimingSubmit", "NeutralSpeech1_TimingClickCount",
 "NeutralSpeech1_Positive", "NeutralSpeech1_Negative", "NeutralSpeech1_Arousal",
 "NeutralSpeech1_Emotion", "NeutralSpeech1_OtherE", "NeutralSpeech1_StrongEmotion", "NeutralSpeech1_OtherSE", 
  
  # Neutral Speech 2: 1083_IWL_NEU_XX
 "NeutralSpeech2_TimingFirst", "NeutralSpeech2_TimingLast", "NeutralSpeech2_TimingSubmit", "NeutralSpeech2_TimingClickCount",
 "NeutralSpeech2_Positive", "NeutralSpeech2_Negative", "NeutralSpeech2_Arousal",
 "NeutralSpeech2_Emotion", "NeutralSpeech2_OtherE", "NeutralSpeech2_StrongEmotion", "NeutralSpeech2_OtherSE", 
  
  # Neutral Speech 3: 1029_TAI_NEU_XX
 "NeutralSpeech3_TimingFirst", "NeutralSpeech3_TimingLast", "NeutralSpeech3_TimingSubmit", "NeutralSpeech3_TimingClickCount",
 "NeutralSpeech3_Positive", "NeutralSpeech3_Negative", "NeutralSpeech3_Arousal",
 "NeutralSpeech3_Emotion", "NeutralSpeech3_OtherE", "NeutralSpeech3_StrongEmotion", "NeutralSpeech3_OtherSE", 
  
  # Negative Valence, High Arousal -- Human 2: 275
 "NegValHighArHuman_TimingFirst", "NegValHighArHuman_TimingLast", "NegValHighArHuman_TimingSubmit", "NegValHighArHuman_TimingClickCount",
 "NegValHighArHuman_Positive", "NegValHighArHuman_Negative", "NegValHighArHuman_Arousal",
 "NegValHighArHuman_Emotion", "NegValHighArHuman_OtherE", "NegValHighArHuman_StrongEmotion", "NegValHighArHuman_OtherSE", 
  
  # Negative Valence, High Arousal -- Non-human 1: 424
 "NegValHighArNon_TimingFirst", "NegValHighArNon_TimingLast", "NegValHighArNon_TimingSubmit", "NegValHighArNon_TimingClickCount",
 "NegValHighArNon_Positive", "NegValHighArNon_Negative", "NegValHighArNon_Arousal",
 "NegValHighArNon_Emotion", "NegValHighArNon_OtherE", "NegValHighArNon_StrongEmotion", "NegValHighArNon_OtherSE",
  
  # Negative Valence, Low Arousal -- Human 2: 295
 "NegValLowArHuman_TimingFirst", "NegValLowArHuman_TimingLast", "NegValLowArHuman_TimingSubmit", "NegValLowArHuman_TimingClickCount",
 "NegValLowArHuman_Positive", "NegValLowArHuman_Negative", "NegValLowArHuman_Arousal",
 "NegValLowArHuman_Emotion", "NegValLowArHuman_OtherE", "NegValLowArHuman_StrongEmotion", "NegValLowArHuman_OtherSE",
  
  # Negative Valence, Low Arousal -- Non-human 1: 703
 "NegValLowArNon_TimingFirst", "NegValLowArNon_TimingLast", "NegValLowArNon_TimingSubmit", "NegValLowArNon_TimingClickCount",
 "NegValLowArNon_Positive", "NegValLowArNon_Negative", "NegValLowArNon_Arousal",
 "NegValLowArNon_Emotion", "NegValLowArNon_OtherE", "NegValLowArNon_StrongEmotion", "NegValLowArNon_OtherSE",
  
  # Neutral -- Human 1: 722
 "NeutralHuman_TimingFirst", "NeutralHuman_TimingLast", "NeutralHuman_TimingSubmit", "NeutralHuman_TimingClickCount",
 "NeutralHuman_Positive", "NeutralHuman_Negative", "NeutralHuman_Arousal",
 "NeutralHuman_Emotion", "NeutralHuman_OtherE", "NeutralHuman_StrongEmotion", "NeutralHuman_OtherSE",
  
  # Neutral -- Non-human 2: 701
 "NeutralNon_TimingFirst", "NeutralNon_TimingLast", "NeutralNon_TimingSubmit", "NeutralNon_TimingClickCount",
 "NeutralNon_Positive", "NeutralNon_Negative", "NeutralNon_Arousal",
 "NeutralNon_Emotion", "NeutralNon_OtherE", "NeutralNon_StrongEmotion", "NeutralNon_OtherSE",
  
  # Positive Valence, High Arousal -- Human 2: 352
 "PosValHighArHuman_TimingFirst", "PosValHighArHuman_TimingLast", "PosValHighArHuman_TimingSubmit", "PosValHighArHuman_TimingClickCount",
 "PosValHighArHuman_Positive", "PosValHighArHuman_Negative", "PosValHighArHuman_Arousal",
 "PosValHighArHuman_Emotion", "PosValHighArHuman_OtherE", "PosValHighArHuman_StrongEmotion", "PosValHighArHuman_OtherSE",
  
  # Positive Valence, High Arousal -- Non-human 2: 717
 "PosValHighArNon_TimingFirst", "PosValHighArNon_TimingLast", "PosValHighArNon_TimingSubmit", "PosValHighArNon_TimingClickCount",
 "PosValHighArNon_Positive", "PosValHighArNon_Negative", "PosValHighArNon_Arousal",
 "PosValHighArNon_Emotion", "PosValHighArNon_OtherE", "PosValHighArNon_StrongEmotion", "PosValHighArNon_OtherSE",
  
  # Positive Valence, Low Arousal -- Human 1: 112
 "PosValLowArHuman_TimingFirst", "PosValLowArHuman_TimingLast", "PosValLowArHuman_TimingSubmit", "PosValLowArHuman_TimingClickCount",
 "PosValLowArHuman_Positive", "PosValLowArHuman_Negative", "PosValLowArHuman_Arousal",
 "PosValLowArHuman_Emotion", "PosValLowArHuman_OtherE", "PosValLowArHuman_StrongEmotion", "PosValLowArHuman_OtherSE",
  
  # Positive Valence, Low Arousal -- Non-human 2: 172
 "PosValLowArNon_TimingFirst", "PosValLowArNon_TimingLast", "PosValLowArNon_TimingSubmit", "PosValLowArNon_TimingClickCount",
 "PosValLowArNon_Positive", "PosValLowArNon_Negative", "PosValLowArNon_Arousal",
 "PosValLowArNon_Emotion", "PosValLowArNon_OtherE", "PosValLowArNon_StrongEmotion", "PosValLowArNon_OtherSE",
  
  # End
  "QuestionsForExperimenter"
)

#### Check Names ####
check<-t(perceived[1,])
perceived<-perceived[-c(1:2),]

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
perceivedCleanID<-setNames(data.frame(fixID(perceived)), "OldID")

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

perceived<-cbind(participantIDandDrugCode(perceivedCleanID, idCodes, drugcodes), perceived[-1])

############################################################################################
##### Get Rid of Timing Info ####
perceived<-perceived[,which(!grepl("Timing|Instructions", colnames(perceived)))]
colnames<-as.data.frame(colnames(perceived))

############################################################################################
##### Write out wide dataframe ####
write.csv(perceived, file = "Fixed Perceived Wide.csv", row.names = F)

############################################################################################
##### Make Long Dataframe ####

FearMusic1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearMusic1')) %>% mutate(Stimulus = "Fear Music 1")
FearMusic2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearMusic2')) %>% mutate(Stimulus = "Fear Music 2")
FearMusic3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearMusic3')) %>% mutate(Stimulus = "Fear Music 3")

HappyMusic1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappyMusic1')) %>% mutate(Stimulus = "Happy Music 1")
HappyMusic2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappyMusic2')) %>% mutate(Stimulus = "Happy Music 2")
HappyMusic3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappyMusic3')) %>% mutate(Stimulus = "Happy Music 3")

SadMusic1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadMusic1')) %>% mutate(Stimulus = "Sad Music 1")
SadMusic2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadMusic2')) %>% mutate(Stimulus = "Sad Music 2")
SadMusic3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadMusic3')) %>% mutate(Stimulus = "Sad Music 3")

TenderMusic1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic1')) %>% mutate(Stimulus = "Tender Music 1")
TenderMusic2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic2')) %>% mutate(Stimulus = "Tender Music 2")
TenderMusic3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic3')) %>% mutate(Stimulus = "Tender Music 3")
TenderMusic4<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic4')) %>% mutate(Stimulus = "Tender Music 4")
TenderMusic5<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic5')) %>% mutate(Stimulus = "Tender Music 5")
TenderMusic6<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('TenderMusic6')) %>% mutate(Stimulus = "Tender Music 6")

FearSpeech1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearSpeech1')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Fear Speech 1") 
FearSpeech2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearSpeech2')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Fear Speech 2") 
FearSpeech3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('FearSpeech3')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Fear Speech 3") 

HappySpeech1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappySpeech1')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Happy Speech 1") 
HappySpeech2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappySpeech2')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Happy Speech 2") 
HappySpeech3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('HappySpeech3')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Happy Speech 3") 

SadSpeech1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadSpeech1')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Sad Speech 1") 
SadSpeech2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadSpeech2')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Sad Speech 2") 
SadSpeech3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('SadSpeech3')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Sad Speech 3") 

NeutralSpeech1<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralSpeech1')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Speech 1") 
NeutralSpeech2<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralSpeech2')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Speech 2") 
NeutralSpeech3<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralSpeech3')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Speech 3") 

NegValHighArHuman<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValHighArHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence High-Arousal Human") 
NegValHighArNon<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValHighArNon')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence High-Arousal Non-human") 
NegValLowArHuman<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValLowArHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence Low-Arousal Human") 
NegValLowArNon<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NegValLowArNon')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Negative-Valence Low-Arousal Non-human") 

NeutralHuman<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Human") 
NeutralNon<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('NeutralNon')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Neutral Non-human") 

PosValHighArHuman<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValHighArHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence High-Arousal Human") 
PosValHighArNon<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValHighArNon')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence High-Arousal Non-human") 
PosValLowArHuman<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValLowArHuman')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence Low-Arousal Human") 
PosValLowArNon<-perceived %>% select("FixedID", "DrugCode", "DrugPlacebo", starts_with('PosValLowArNon')) %>% mutate(NegValHighArHuman_Familiarity = "Not Measured") %>% mutate(Stimulus = "Positive-Valence Low-Arousal Non-human") 

fixedPerceived<-rbindlist(list(FearMusic1, FearMusic2, FearMusic3, HappyMusic1, HappyMusic2, HappyMusic3, SadMusic1, SadMusic2, SadMusic3,
                               TenderMusic1, TenderMusic2, TenderMusic3, TenderMusic4, TenderMusic5, TenderMusic6, 
                               FearSpeech1, FearSpeech2, FearSpeech3, HappySpeech1, HappySpeech2, HappySpeech3, 
                               SadSpeech1, SadSpeech2, SadSpeech3, NeutralSpeech1, NeutralSpeech2, NeutralSpeech3, 
                               NegValHighArHuman, NegValHighArNon, NegValLowArHuman, NegValLowArNon, NeutralHuman, NeutralNon,
                               PosValHighArHuman, PosValHighArNon, PosValLowArHuman, PosValLowArNon))
fixedPerceived$Locus<-"Perceived"
colnames(fixedPerceived)<-c("FixedID", "DrugCode", "DrugPlacebo", 
                          "Positive", "Negative", "Arousal",
                          "Emotion", "OtherE", "StrongEmotion", "OtherSE", 
                          "Familiarity", "Stimulus", "Locus")
fixedPerceived<-fixedPerceived[,c(1:3,13,12,4:11)]
fixedPerceived[]<-lapply(fixedPerceived, as.character)

############################################################################################
##### Fix Positive and Negative Columns ####
for(i in 1:nrow(fixedPerceived)){
  if(grepl("0: Not at all positive", fixedPerceived$Positive[i])){fixedPerceived$Positive[i]<-"0"}
  if(grepl("10: Extremely positive", fixedPerceived$Positive[i])){fixedPerceived$Positive[i]<-"10"}
  
  if(grepl("0: Not at all negative", fixedPerceived$Negative[i])){fixedPerceived$Negative[i]<-"0"}
  if(grepl("10: Extremely negative", fixedPerceived$Negative[i])){fixedPerceived$Negative[i]<-"10"}
  
  if(grepl("0: This sound represents no energy/arousal", fixedPerceived$Arousal[i])){fixedPerceived$Arousal[i]<-"0"}
  if(grepl("10: This sound represents an extreme amount of energy/arousal", fixedPerceived$Arousal[i])){fixedPerceived$Arousal[i]<-"10"}
}
fixedPerceived$Positive<-as.numeric(fixedPerceived$Positive)
fixedPerceived$Negative<-as.numeric(fixedPerceived$Negative)
fixedPerceived$Arousal<-as.numeric(fixedPerceived$Arousal)

############################################################################################
##### Fix Emotion Columns ####

list<-c("Anger", "Anxious", "Bored", "Disgusted", "Excited", "Fearful", 
        "Grieved", "Happy", "Invigorated", "Joyful", "Nostalgic", "Peaceful",
        "Power", "Relaxed", "Sad", "Softhearted", "Surprised", "Sympathetic",
        "Tender", "Transcendent", "Tension", "Wonder", "Neutral", "Other")

fixedPerceived[,list]<-NA

for(i in 1:nrow(fixedPerceived)){
  fixedPerceived$Anger[i] = as.numeric(grepl("Anger", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Anger", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Anxious[i] = "Not Measured"
  
  fixedPerceived$Bored[i] = as.numeric(grepl("Bored", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Bored", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Disgusted[i] = as.numeric(grepl("Disgusted", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Disgusted", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Excited[i] = as.numeric(grepl("Excited", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Excited", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Fearful[i] = as.numeric(grepl("Fearful", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Fearful", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Grieved[i] = as.numeric(grepl("Grieved", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Grieved", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Happy[i] = as.numeric(grepl("Happy", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Happy", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Invigorated[i] = as.numeric(grepl("Invigorated", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Invigorated", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Joyful[i] = "Not Measured"
  
  fixedPerceived$Nostalgic[i] = "Not Measured"
  
  fixedPerceived$Peaceful[i] = "Not Measured"
  
  fixedPerceived$Power[i] = "Not Measured"
  
  fixedPerceived$Relaxed[i] = as.numeric(grepl("Relaxed", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Relaxed", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Sad[i] = as.numeric(grepl("Sad", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Sad", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Softhearted[i] = "Not Measured"
  
  fixedPerceived$Surprised[i] = as.numeric(grepl("Surprised", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Surprised", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Sympathetic[i] = "Not Measured"
  
  fixedPerceived$Tender[i] = as.numeric(grepl("Tender", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Tender", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Transcendent[i] = "Not Measured"
  
  fixedPerceived$Tension[i] = "Not Measured"
  
  fixedPerceived$Wonder[i] = "Not Measured"
  
  fixedPerceived$Neutral[i] = as.numeric(grepl("Neutral", fixedPerceived$Emotion[i])) + 
    as.numeric(grepl("Neutral", fixedPerceived$StrongEmotion[i]))
  
  fixedPerceived$Other[i] = tolower(as.character(fixedPerceived$OtherE[i]))
}

for(i in 1:nrow(fixedPerceived)){if(fixedPerceived$Other[i] == ""){fixedPerceived$Other[i]<-NA}}

############################################################################################
##### Familiarity ####
for(i in 1:nrow(fixedPerceived)){
  if(grepl("Not", fixedPerceived$Familiarity[i])){fixedPerceived$Familiarity[i]<-"0"}
  if(grepl("Somewhat", fixedPerceived$Familiarity[i])){fixedPerceived$Familiarity[i]<-"1"}
  if(grepl("Very", fixedPerceived$Familiarity[i])){fixedPerceived$Familiarity[i]<-"2"}
}
fixedPerceived$Familiarity<-as.numeric(fixedPerceived$Familiarity)

############################################################################################
#### Write out fixed perceived!! ####
fixedPerceived<-fixedPerceived[,-c(9:12)]
write.csv(fixedPerceived, file = "Fixed Perceived.csv", row.names = F)

############################################################################################
#### Combine Questionnaires, Induced, and Perceived ####
PandI<-rbind(fixedInduced, fixedPerceived)
write.csv(PandI, file = "Fixed Perceived and Induced.csv", row.names = F)

colnames(fixedQuestionnaires)[1]<-"FixedID"
combined<-merge(fixedQuestionnaires, PandI, by=c("FixedID","DrugCode", "DrugPlacebo"))

write.csv(combined, file = "Fixed Perceived, Induced, and Questionnaires.csv", row.names = F)

