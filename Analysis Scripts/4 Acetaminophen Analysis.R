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
library(corrplot)
library(nFactors)
library("nlme")

fixed<-read.csv("Fixed Perceived, Induced, and Questionnaires.csv", header = T)
colnames<-as.data.frame(colnames(fixed))
null<-which(is.na(fixed$FixedID) | is.na(fixed$DrugPlacebo))
fixed<-fixed[-null,]
fixed$Stimulus<-as.character(fixed$Stimulus)

# add sound type
fixed$SoundType<-NA

for(i in 1:nrow(fixed)){
  if(grepl("Music", fixed$Stimulus[i])){fixed$SoundType[i]<-"Music"}
  if(grepl("Speech", fixed$Stimulus[i])){fixed$SoundType[i]<-"Speech"}
  if(grepl("Human|Non-human", fixed$Stimulus[i])){fixed$SoundType[i]<-"Natural Sounds"}
}

# add emotion type
fixed$PosNeg<-NA

for(i in 1:nrow(fixed)){
  if(grepl("Negative|Sad|Fear", fixed$Stimulus[i])){fixed$PosNeg[i]<-"Negative"}
  if(grepl("Positive|Happy|Tender", fixed$Stimulus[i])){fixed$PosNeg[i]<-"Positive"}
  if(grepl("Neutral", fixed$Stimulus[i])){fixed$PosNeg[i]<-"Neutral"}
}

# add circumplex model info
fixed$Russell<-NA

for(i in 1:nrow(fixed)){
  if(grepl("Negative-Valence Low|Sad", fixed$Stimulus[i])){fixed$Russell[i]<-"Neg-Valence Low-Arousal"}
  if(grepl("Negative-Valence High|Fear", fixed$Stimulus[i])){fixed$Russell[i]<-"Neg-Valence High-Arousal"}
  if(grepl("Positive-Valence Low|Tender", fixed$Stimulus[i])){fixed$Russell[i]<-"Pos-Valence Low-Arousal"}
  if(grepl("Positive-Valence High|Happy", fixed$Stimulus[i])){fixed$Russell[i]<-"Pos-Valence High-Arousal"}
  if(grepl("Neutral", fixed$Stimulus[i])){fixed$Russell[i]<-"Neutral"}
}
fixed<-fixed[,c(1:65,95:97,66:94)]
fixed$Arousal<-as.numeric(fixed$Arousal)


########################################################################################
### Perceived ###
########################################################################################
perceived<-subset(fixed, Locus == "Perceived")

# make it so no controls for now 
perceived<-perceived[,c(1:3,65:97)]

#### Testing Differences in Pos, Neg, and Arousal: General ####

# Positive -- placebo higher, p < 0.05
  # placebo: 3.99
  # drug: 3.58

t.test(perceived$Positive[perceived$DrugPlacebo==0], 
       perceived$Positive[perceived$DrugPlacebo==1], 
       alternative = "two.sided", mu = 0, var.equal = T, conf.level = .95)

# Negative -- placebo higher, p < 0.05
  # placebo: 3.87
  # drug: 3.58

t.test(perceived$Negative[perceived$DrugPlacebo==0], 
       perceived$Negative[perceived$DrugPlacebo==1], 
       alternative = "two.sided", mu = 0, var.equal = T, conf.level = .95)

# Arousal -- no diff, p = 0.92
  # placebo: 5.46
  # drug: 5.47

perceived$Arousal<-as.numeric(perceived$Arousal)

t.test(perceived$Arousal[perceived$DrugPlacebo==0], 
       perceived$Arousal[perceived$DrugPlacebo==1], 
       alternative = "two.sided", mu = 0, var.equal = T, conf.level = .95)

#### Testing Differences in Pos, Neg, and Arousal: By Speech, Music, Natural Sounds ####

mean(perceived$Positive[perceived$DrugPlacebo==1 & perceived$SoundType == "Speech"], na.rm = T)

# Positive 
  # placebo, speech: 2.92       # placebo, music: 5.02        # placebo, natural sounds: 3.70
  # drug, speech: 2.44          # drug, music: 4.62           # drug, natural sounds: 3.38

# Negative 
  # placebo, speech: 4.44       # placebo, music: 3.36        # placebo, natural sounds: 3.96
  # drug, speech: 4.07          # drug, music: 3.12           # drug, natural sounds: 3.69

# Arousal 
  # placebo, speech: 4.81       # placebo, music: 6.01        # placebo, natural sounds: 5.41
  # drug, speech: 4.77          # drug, music: 6.08           # drug, natural sounds: 5.39

perceivedgraph1<-read.csv("perceivedgraph1.csv", header = T)
ggplot(perceivedgraph1, aes(x = SoundType, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,10) + facet_grid(. ~ DV) +
  geom_text(position = position_dodge(width = 1), aes(y=Rating+0.3, label=round(Rating,1), hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))

#### Testing Differences in Pos, Neg, and Arousal: By Pos/Neg Emotion Type ####

# Positive 
  # placebo, positive: 6.80       # placebo, negative: 1.65       # placebo, neutral: 2.42
  # drug, positive: 6.33          # drug, negative: 1.35          # drug, neutral: 1.85

# Negative
  # placebo, positive: 1.52       # placebo, negative: 6.47       # placebo, neutral: 3.08
  # drug, positive: 1.36          # drug, negative: 6.15          # drug, neutral: 2.59

# Arousal 
  # placebo, positive: 6.13       # placebo, negative: 5.47       # placebo, neutral: 3.24
  # drug, positive: 6.15          # drug, negative: 5.55          # drug, neutral: 3.04

#### Testing Differences in Pos, Neg, and Arousal: By Pos/Neg Emotion Type ####

# Positive 
  # placebo, Pos-Valence High-Arousal: 7.62       # placebo, Pos-Valence Low-Arousal: 5.99
  # drug, Pos-Valence High-Arousal: 7.12          # drug, Pos-Valence Low-Arousal: 5.54 

  # placebo, Neg-Valence High-Arousal: 1.34       # placebo, Neg-Valence Low-Arousal: 1.97
  # drug, Neg-Valence High-Arousal: 1.08          # drug, Neg-Valence Low-Arousal: 1.61 

  # placebo, Neutral: 2.42
  # drug, Neutral: 1.85

# Negative 
  # placebo, Pos-Valence High-Arousal: 1.18       # placebo, Pos-Valence Low-Arousal: 1.87
  # drug, Pos-Valence High-Arousal: 1.04          # drug, Pos-Valence Low-Arousal: 1.68

  # placebo, Neg-Valence High-Arousal: 6.92       # placebo, Neg-Valence Low-Arousal: 6.02
  # drug, Neg-Valence High-Arousal: 6.69          # drug, Neg-Valence Low-Arousal: 5.61

  # placebo, Neutral: 3.08
  # drug, Neutral: 2.59

# Arousal 
  # placebo, Pos-Valence High-Arousal: 6.94       # placebo, Pos-Valence Low-Arousal: 5.34
  # drug, Pos-Valence High-Arousal: 7.2           # drug, Pos-Valence Low-Arousal: 5.10

  # placebo, Neg-Valence High-Arousal: 6.22       # placebo, Neg-Valence Low-Arousal: 4.72
  # drug, Neg-Valence High-Arousal: 6.43          # drug, Neg-Valence Low-Arousal: 4.66 

  # placebo, Neutral: 3.24
  # drug, Neutral: 3.04

# mean(perceived$Arousal[perceived$DrugPlacebo==1 & perceived$Russell == "Neutral"], na.rm = T)

perceivedgraph2<-read.csv("perceivedgraph2.csv", header = T)
ggplot(perceivedgraph2, aes(x = SoundType, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,10) + facet_grid(. ~ DV) +
  geom_text(position = position_dodge(width = 1), aes(y=Rating+0.25, label=round(Rating,1), hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))
########################################################################################
### Induced ###
########################################################################################
induced<-subset(fixed, Locus == "Induced")

# make it so no controls for now 
induced<-induced[,c(1:3,65:97)]

#### Testing Differences in Pos, Neg, and Arousal: General ####

# Positive -- no diff, p = 0.33
  # placebo: 3.74
  # drug: 3.64

t.test(induced$Positive[induced$DrugPlacebo==0], 
       induced$Positive[induced$DrugPlacebo==1], 
       alternative = "two.sided", mu = 0, var.equal = T, conf.level = .95)

# Negative -- no diff, p = 0.50
  # placebo: 3.57
  # drug: 3.64

t.test(induced$Negative[induced$DrugPlacebo==0], 
       induced$Negative[induced$DrugPlacebo==1], 
       alternative = "two.sided", mu = 0, var.equal = T, conf.level = .95)

# Arousal -- not measured for induced emotions

#### Testing Differences in Pos, Neg, and Arousal: By Music, Natural Sounds ####

# no speech tested in induced emotions
# Positive 
  # placebo, music: 4.59        # placebo, natural sounds: 3.06
  # drug, music: 4.70           # drug, natural sounds: 2.79

# Negative 
  # placebo, music: 3.15        # placebo, natural sounds: 3.91
  # drug, music: 3.16           # drug, natural sounds: 4.03

inducedgraph1<-read.csv("inducedgraph1.csv", header = T)
ggplot(inducedgraph1, aes(x = SoundType, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,10) + facet_grid(. ~ DV) +
  geom_text(position = position_dodge(width = 1), aes(y=Rating+0.25, label=Rating, hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))

#### Testing Differences in Pos, Neg, and Arousal: By Pos/Neg Emotion Type ####

# Positive 
  # placebo, positive: 6.27       # placebo, negative: 1.77       # placebo, neutral: 1.50
  # drug, positive: 6.25          # drug, negative: 1.62          # drug, neutral: 1.28

# Negative
  # placebo, positive: 1.32       # placebo, negative: 5.80       # placebo, neutral: 3.67
  # drug, positive: 1.31          # drug, negative: 5.93          # drug, neutral: 3.79

#### Testing Differences in Pos, Neg, and Arousal: By Pos/Neg Emotion Type ####

# Positive 
  # placebo, Pos-Valence High-Arousal: 6.92       # placebo, Pos-Valence Low-Arousal: 5.63
  # drug, Pos-Valence High-Arousal: 6.86          # drug, Pos-Valence Low-Arousal: 5.63 

  # placebo, Neg-Valence High-Arousal: 1.55       # placebo, Neg-Valence Low-Arousal: 1.99
  # drug, Neg-Valence High-Arousal: 1.41          # drug, Neg-Valence Low-Arousal: 1.84 

  # placebo, Neutral: 1.50
  # drug, Neutral: 1.28

# Negative 
  # placebo, Pos-Valence High-Arousal: 0.98       # placebo, Pos-Valence Low-Arousal: 1.66
  # drug, Pos-Valence High-Arousal: 1.01          # drug, Pos-Valence Low-Arousal: 1.62

  # placebo, Neg-Valence High-Arousal: 6.27       # placebo, Neg-Valence Low-Arousal: 5.33
  # drug, Neg-Valence High-Arousal: 6.48          # drug, Neg-Valence Low-Arousal: 5.39

  # placebo, Neutral: 3.67
  # drug, Neutral: 3.79

# mean(induced$Positive[induced$DrugPlacebo==0 & induced$SoundType == "Speech"], na.rm = T)

inducedgraph2<-read.csv("inducedgraph2.csv", header = T)
ggplot(inducedgraph2, aes(x = SoundType, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,10) + facet_grid(. ~ DV) +
  geom_text(position = position_dodge(width = 1), aes(y=Rating+0.25, label=Rating, hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))

########################################################################################
### Linear Regression ###
########################################################################################
#### Positive Emotion ####
posAnalysis<-fixed[,-c(2,# Drug Code
                       33, 36, 37, 38, 39, # I don't think there's enough variance of these 
                                           # (CigarettesInSystem, ArthritisYN, ImmuneDisordersYN, 
                                           # EndocrineDisordersYN, DiabeticYN)
                       67, # PosNeg (redundant with Russell column)
                       68, # Russell
                       69, # Stimulus (redundant with other emotions)
                       71,72, # Negative and Arousal 
                       74:97)] # Specific Emotions

posLR<-lm(Positive ~ ., data = posAnalysis)
summary(posLR)

# R2 = 0.077
# sig. coefficients (p < 0.05)

  # Drug Placebo: 1.169 (drug results in more positive emotion ratings)
  # SoundType Natural Sounds: -1.856 (compared to music, natural sounds result in less positive perceptions or feelings)
  # SoundType Speech: -2.535 (compared to music, speech results in less positive perceptions or feelings)
  # Familiarity: -0.897 (more familiar, more positive)
  # (notice, no difference in perceived/induced!)

#### Negative Emotion ####
negAnalysis<-fixed[,-c(2,# Drug Code
                       33, 36, 37, 38, 39, # I don't think there's enough variance of these 
                                           # (CigarettesInSystem, ArthritisYN, ImmuneDisordersYN, 
                                           # EndocrineDisordersYN, DiabeticYN)
                       67, # PosNeg (redundant with Russell column)
                       68, # Russell 
                       69, # Stimulus (redundant with other emotions)
                       70,72, # Positive and Arousal 
                       74:97)] # Specific Emotions
negLR<-lm(Negative ~ ., data = negAnalysis)
summary(negLR)

# R2 = 0.009
# no sig. coefficients (p < 0.05)

#### Arousal ####
arAnalysis<-fixed[,-c(2,# Drug Code
                       33, 36, 37, 38, 39, # I don't think there's enough variance of these 
                                           # (CigarettesInSystem, ArthritisYN, ImmuneDisordersYN, 
                                           # EndocrineDisordersYN, DiabeticYN)
                       #65, # Locus (only perceived had arousal measured)
                       67, # PosNeg (redundant with Russell column)
                       68, # Russell
                       69, # Stimulus (redundant with other emotions)
                       70,71, # Positive and Negative 
                       74:97)] # Specific Emotions
arLR<-lm(Arousal ~ ., data = arAnalysis)
summary(arLR)

# R2 = 0.192
# sig. coefficients (p < 0.05)
  # DrugPlacebo: -1.498 (placebo results in higher arousal ratings)
  # SoundType Speech: -4.164 (compared to music, speech result in less high arousal)

#### All Combined ####
pos<-fixed[,-c(2, 33, 36, 37, 38, 39, 71, 72, 74:97)] 
pos$RatingType<-"Positive"
colnames(pos)[64]<-"Rating"

neg<-fixed[,-c(2, 33, 36, 37, 38, 39, 70, 72, 74:97)] 
neg$RatingType<-"Negative"
colnames(neg)[64]<-"Rating"

ar<-fixed[,-c(2, 33, 36, 37, 38, 39, 70, 71, 74:97)] 
ar$RatingType<-"Arousal"
colnames(ar)[64]<-"Rating"

combined<-rbindlist(list(pos, neg, ar))
colnames(combined)

# make RatingType reference Positive
combined$RatingType<-factor(combined$RatingType, levels = c("Positive", "Negative", "Arousal"))
combined$PosNeg<-factor(combined$PosNeg, levels = c("Neutral", "Negative", "Positive"))
combined$Russell<-factor(combined$Russell, levels = c("Neutral", "Neg-Valence High-Arousal", "Neg-Valence Low-Arousal", 
                                                      "Pos-Valence High-Arousal", "Pos-Valence Low-Arousal"))
combined$FixedID<-factor(combined$FixedID)

# regression
combinedLR<-lm(Rating ~ ., data = combined)
summary(combinedLR)
colnames(combined)

# R2 = 0.3135
# sig. coefficients (p < 0.05)
  # MedsEffectiveness Slightly: -1.280
  # LastTimeTookMeds 1 month: 1.166
  # Locus Perceived: -2.34
  # SoundType Natural Sounds: -0.738
  # SoundType Speech: -1.154
  # RussellNeg-Valence High-Arousal: 1.905
  # RussellNeg-Valence Low-Arousal: 1.355
  # RussellPos-Valence High-Arousal: 1.994
  # RussellPos-Valence Low-Arousal: 1.435
  # RatingType Negative: 0.742
  # RatingType Arousal: 4.069

# REML
combinedREML<-gls(Rating ~ Stimulus, 
                  data = combined, 
                  correlation = corSymm(form = ~ 1 |FixedID),
                  weights = varIdent(form = ~ 1 | Stimulus),
                  method = "REML")
summary(combinedREML)
getVarCov(combinedREML)

# ML
combinedML <- update(combinedREML, method = "ML")
summary(combinedML)
getVarCov(combinedML)

# THIS IS THE REGRESSION MODEL THAT WORKS--LOOK HERE #
colnames(combined)
meh<-combined[,-c(1,3:58,62,63)] # gets rid of questionnaire stuff that's clogging up the model
colnames(meh)

meh$RatingType<-factor(meh$RatingType, levels = c("Positive", "Negative", "Arousal"))
meh$PosNeg<-factor(meh$PosNeg, levels = c("Negative", "Neutral", "Positive"))

combinedLR<-lm(Rating ~ ., data = meh)
summary(combinedLR)

############
meh<-combined[,-c(1,3:58,62,63)] # gets rid of questionnaire stuff that's clogging up the model
mehI<-subset(meh, Locus == "Induced")
mehI<-mehI[,-2]

mehP<-subset(meh, Locus == "Perceived")
mehP<-mehP[,-2]

mehI$RatingType<-factor(mehI$RatingType, levels = c("Positive", "Negative", "Arousal"))
mehI$PosNeg<-factor(mehI$PosNeg, levels = c("Negative", "Neutral", "Positive"))
noNeutralI<-subset(mehI, PosNeg != "Neutral")
noNeutralI$PosNeg<-as.character(noNeutralI$PosNeg)

mehP$RatingType<-factor(mehP$RatingType, levels = c("Positive", "Negative", "Arousal"))
mehP$PosNeg<-factor(mehP$PosNeg, levels = c("Negative", "Neutral", "Positive"))

table(noNeutralI$DrugPlacebo, noNeutralI$PosNeg)

iLR<-lm(Rating ~ ., data = p) 
summary(iLR)

t.test(pos$Rating, neg$Rating)

iLR<-lm(Rating ~ . + DrugPlacebo*PosNeg, data = noNeutralI)
summary(iLR)

pLR<-lm(Rating ~ ., data = mehP)
summary(pLR)

#########################################################################################
#### Are Emotion Terms Used Differently? ####
#########################################################################################
#### Perceived ####
perceived<-subset(fixed, Locus == "Perceived")
perceived<-perceived[,c(1,3,66,68,69,74,76:82,87:88,90,92,96:97)]
colnames(perceived)
perceived[,-c(1:5,19)]<-lapply(perceived[,-c(1:5,19)], as.numeric)
perceived$Other<-as.character(perceived$Other)
perceived$Other<-trimws(perceived$Other, which = "both")
perceived$Other<-gsub("/|-", " ", perceived$Other)
perceived$Other<-gsub("[[:punct:]]", "\\1", perceived$Other)
table(perceived$Other)

#### Testing Differences Between Drug and Placebo ####
placebo<-subset(perceived, DrugPlacebo == 0)
drug<-subset(perceived, DrugPlacebo == 1)

test<-setNames(data.frame(matrix(ncol = 14, nrow = 2)), colnames(perceived[,c(2,6:18)]))
test[1,]<-append(0,colMeans(placebo[,-c(1:5,19)], na.rm = T), after = 1)
test[2,]<-append(1,colMeans(drug[,-c(1:5,19)], na.rm = T), after = 1)

#test[2,]<-append(1,colMeans(drug[,-c(1:5,19)]) %>% prop.table()*100, after = 1)
test$DrugPlacebo<-factor(test$DrugPlacebo, labels = c("Placebo", "Drug"))
library(data.table)
test<-as.data.frame(t(test))
colnames(test)<-c("Placebo", "Drug")
test<-test[-1,]
test<-setDT(test, keep.rownames = TRUE)[]

(perceived %>% group_by(DrugPlacebo) %>% summarise_at(vars(Anger:Neutral), .funs =  sum))
(perceived %>% group_by(DrugPlacebo) %>% summarise(Sum = sum(Anger)))

perceivedgraph6<-read.csv("perceivedgraph6.csv", header = T)
ggplot(perceivedgraph6, aes(x = Emotion, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,2) + facet_grid(. ~ DV) +
  #geom_text(position = position_dodge(width = 1), aes(y=Rating+0.05, label=Rating, hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    axis.text.x = element_text(size = 8, angle = 90),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))

table(test$DrugPlacebo, test[,-1])
chisq.test(test[1,-1], test[2,-1], correct = F) 

chisq.test(allPlacebo, correct = F) # terms not used equally across placebo, p = 0.001
chisq.test(allDrug, correct = F) # terms not used equally across drug, p = 0.001

# Examining "Other" Emotions 
table(placebo$Other)
table(drug$Other)

#### Testing Differences Between Drug and Placebo: By Speech, Music, Natural Sounds ####

# is melancholy diff from bored?
binom.test(191, n=191+25, p=0.5, alternative="two.sided", conf.level=.95)

#### Testing Differences Between Drug and Placebo: By Pos/Neg Emotion Type ####

#### Testing Differences Between Drug and Placebo: By Russell Emotion Type ####

#### Testing Differences Between Drug and Placebo: By Stimulus ####

#########################################################################################
#### Induced ####
induced<-subset(fixed, Locus == "Induced")
induced<-induced[,c(1,3,66,68,69,74:96)]
induced[,-c(1:5)]<-lapply(induced[,-c(1:5)], as.numeric)

placebo<-subset(induced, DrugPlacebo == 0)
drug<-subset(induced, DrugPlacebo == 1)

test<-setNames(data.frame(matrix(ncol = 24, nrow = 2)), colnames(induced[,c(2,6:28)]))
test[1,]<-append(0,colMeans(placebo[,-c(1:5)], na.rm = T), after = 1)
test[2,]<-append(1,colMeans(drug[,-c(1:5)], na.rm = T), after = 1)

# test[1,]<-append(0,colMeans(placebo[,-c(1:5)]) %>% prop.table()*100, after = 1)
# test[2,]<-append(1,colMeans(drug[,-c(1:5)]) %>% prop.table()*100, after = 1)
test$DrugPlacebo<-factor(test$DrugPlacebo, labels = c("Placebo", "Drug"))
library(data.table)
test<-as.data.frame(t(test))
colnames(test)<-c("Placebo", "Drug")
as.data.frame(test$Placebo)
as.data.frame(test$Drug)
test<-test[-1,]
test<-setDT(test, keep.rownames = TRUE)[]

meh<-(induced %>% group_by(DrugPlacebo, SoundType) %>% summarise_at(vars(Anger:Neutral), mean))
write.csv(meh, "inducedgraph6.csv")

inducedgraph6<-read.csv("inducedgraph6.csv", header = T)
ggplot(inducedgraph6, aes(x = Emotion, y = Rating, fill = Condition)) + 
  geom_bar(position = position_dodge(), stat = "identity") + xlab("") + ylab("") + 
  ylim(0,2) + facet_grid(. ~ DV) +
  #geom_text(position = position_dodge(width = 1), aes(y=Rating+0.05, label=Rating, hjust=.5)) +
  theme(plot.title = element_text(lineheight=.8, face="bold")) + theme(
    axis.text.y = element_text(size = 8, angle = 0),
    axis.text.x = element_text(size = 8, angle = 90),
    panel.grid.major = element_line(colour = "transparent"),
    panel.grid.minor = element_line(colour = "transparent"),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(colour="black", fill="#CCCCFF"),
    axis.title.y = element_text(size = rel(1.5), angle = 90),
    plot.title = element_text(size = rel(2.25)),
    panel.border = element_rect(colour = "black", fill = "transparent"))

#########################################################################################
#### Factor Analysis ####
ev<-eigen(cor(perceived[,c(2:14)]))
ap<-parallel(subject=nrow(mydata),var=ncol(mydata),
             rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)





# questions:
  # DV: combine neg, pos, arousal? more than one DV? 
  # eliminate bad subjects?
  # what model? ML? REML? 
  # missing data?
  # within subjects?
  # how test for differences in specific emotions? or neg/pos emotions? between speech/nat sounds/music?
  

# LEAPS; all-subsets regression
  # one for pos, one for neg, one for ar
  # REML
 
library(spotifyr)
library(ggplot2)
library(cluster)
library(fpc)
library(dendextend)
library(purrr)
library(lme4)
library(tidyverse)
library(caret)
library(leaps)

models <- regsubsets(Positive ~ ., data = posAnalysis, nvmax = 2)
summary(models)

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_model_formula(24, models, "Rating")
sort(coef(models, 24))

# to do:
  # make wide format
  # anovas testing drug x type (speech music); drug x arousal (NVLA)
  # mixed models of ^ 

#### make wide ####
# perceived<-read.csv("Fixed Perceived.csv", header = T)
# induced<-read.csv("Fixed Induced.csv", header = T)
# questionnaires<-read.csv("Fixed Questionnaires.csv", header = T)

# colnames(perceived)
# colnames(induced)
# colnames(questionnaires)

fixed$Locus<-as.character(fixed$Locus)
unique<-unique(fixed[c("Stimulus", "Locus")])
names<-""
for(i in 1:nrow(unique)){
  meh<-paste0(unique[i,1], "_", 
              unique[i,2], "_", 
              colnames(fixed)[-c(1:64)])
  names<-c(names,meh)
}
names<-names[-1]

ids<-unique(fixed[,1:64])
ids[,names]<-NA
colnames<-data.frame(colnames(ids))

dataframe1 = fixed
dataframe2 = ids
id = 1

for(id in unique(ids$FixedID)){
  location<-which(dataframe2$FixedID == id)
  oldRows<-which(dataframe1$FixedID == id)
  
  for(i in oldRows){
    stim = dataframe1$Stimulus[i]
    locus = dataframe1$Locus[i]
    cols<-which(grepl(stim, colnames(dataframe2)) & grepl(locus, colnames(dataframe2)))
    newRow<-subset(dataframe1, FixedID == id & Stimulus == stim & Locus == locus)
    dataframe2[location,cols]<-newRow[,65:97]
  }
}
write.csv(dataframe2, file = "test wide.csv", row.names = F)

meep<-data.frame(t(dataframe2))















  
  
  
  
  
  




