####                      Info:                                      ####
####   Authors: Phillips, J. Mott, C., De Freitas, J.                 ###
####                  Gruber, J. & Knobe, J.                          ###
####                                                                  ###
####   Title: True happiness;                                         ###
####          The role of morality in the concept of happiness        ###
####   Contact: phillips01@g.harvard.edu                              ###

#### Working directory, packages and data ####
rm(list=ls())

#setwd("####")
setwd("C:/Users/Jphil/Documents/currentProjects/Happiness2/trueHappiness/trueHappiness")

# packages
library(plyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(lme4)
library(lmerTest)
library(ez)
library(lsr)

# Simple palette for figures
blackGreyPalette <- c("#2C3539", "#999999") 

#data
d.1a <- read.csv("Data/Study_1/study1_mturk_data.csv") # mturk data
d.1b <-read.csv("Data/Study_1/study1_expert_data.csv") # listserv data
d.2 <- read.csv("Data/Study_2/study2_data.csv") 
d.3a <- read.csv("Data/Study_3/study3a_data.csv") # moral question
d.3b <- read.csv("Data/Study_3/study3b_data.csv") # happiness question 
d.4a <- read.csv("Data/Study_4/study4a_data.csv") # text dv
d.4b <- read.csv("Data/Study_4/study4b_data.csv") #face dv
d.5 <- read.csv("Data/Study_5/study5_data.csv")

#### Demographics ####
d.1a$study <- "Study 1.1"
d.1b$study <- "Study 1.2"
d.2$study <- "Study 2"
d.3a$study <- "Study 3.1"
d.3b$study <- "Study 3.2"
d.4a$study <- "Study 4.1"
d.4b$study <- "Study 4.2"
d.5$study <- "Study 5"

##demographic data
demog.data <- rbind(d.1a[!duplicated(d.1a[,1]),c(13:14,17:18,30)] # this had within-subject data
                    ,d.1b[!duplicated(d.1b[,1]),c(10:11,14:15,30)] # this had within-subject data
                    ,d.2[,c(12:13,20:21,25)]
                    ,d.3a[,c(25:26,29:30,35)]
                    ,d.3b[,c(25:26,29:30,35)]
                    ,d.4a[,c(101:102,105:106,119)]
                    ,d.4b[,c(8:9,12:13,15)]
                    ,d.5[,c(133:134,137:138,151)]
                    )

demog.data$gender <- factor(c("Male","Female")[demog.data$gender], exclude=NULL)
demog.age <- aggregate(age~study, demog.data, FUN=function(x) c(M =mean(x), SD =sd(x)))
demog.gender <- aggregate(gender~study, demog.data, FUN=table)
demog.n <- aggregate(gender~study, demog.data, FUN=length)
print(cbind(demog.age,demog.gender[,2],demog.n[,2]))

demog.data$education <- factor(c("No GED","Highschool","College","Masters","Doctorate","Other")[demog.data$education], exclude=NULL)
demog.education <- aggregate(education~study, demog.data, FUN=table)

demog.ses <- aggregate(ses~study, demog.data, FUN=table)

# Study 1 ---------------------------------------------------------------------------------

## preprocessing, expertise checks, control questions
d.1a$subj <- as.numeric(d.1a$subj)
d.1b$subj <- d.1b$subj+max(d.1a$subj)
d.1a$group <- "Non-Researchers"
d.1b$group <- "Non-Researchers"
d.1b$group[d.1b$researchQ==1] <- "Researchers"
d.1 <- rbind(d.1a,d.1b)
d.1$researchScale <- d.1$hapResearc + d.1$emoResearc + d.1$familiarity
hist(d.1$researchScale,breaks=21,col='Red')
expert.cutoff <- quantile(d.1$researchScale,.5,na.rm=T)
d.1$group[d.1$researchScale>expert.cutoff] <- "Experts"
aggregate(researchScale~group, FUN=function(x) c(m = mean(x), sd = sd(x), n= length(x)), data=d.1)

#factoring variables
d.1$coding <- factor(c("Morality is relevant to happiness","Different descriptive states","Dislike for agent","Other","No difference in happiness")[d.1$coding])
d.1$withIn_between <- factor(c("Within","Between")[d.1$withIn_between])
d.1$scenario <- factor(c("Garret","Sarah","Tom")[d.1$scenario])
d.1$morality <- factor(c("Moral","Immoral")[d.1$morality])
d.1$order <- factor(c("Moral First","Immoral First","Between Subjects")[d.1$order])
d.1$group <- factor(d.1$group, levels=c("Non-Researchers","Researchers","Experts"))
aggregate(subj~group, FUN=function(x)  length(unique(x)), data=d.1)

#control variable for psychological states
d.1$control <- rowSums(d.1[,7:9])

# Primary Analyses
##between-subjects
#d.1.btw <- d.1[d.1$withIn_between=="Between",] ## use this instead if you want to include participants
d.1.btw <- d.1[d.1$withIn_between=="Between" & d.1$control==3,]
d.1.btw <- d.1.btw[!is.na(d.1.btw$subj),]
length(d.1$subj[d.1$withIn_between=="Between"])-length(d.1.btw$subj) ##45 excluded in between-subjects analyses

lm1.0 <- lmer(happiness ~ morality + group + (1|scenario), data=d.1.btw)  #base model
lm1.1 <- lmer(happiness ~ morality * group + (1|scenario), data=d.1.btw) 
summary(lm1.1)
anova(lm1.0,lm1.1) # test for group*morality interaction

lm1.2 <- lmer(happiness ~ morality + (1|scenario), data=d.1.btw) 
anova(lm1.0,lm1.2) # test for main effect of group

lm1.3 <- lmer(happiness ~ group + (1|scenario), data=d.1.btw) 
anova(lm1.0,lm1.3) # test for main effect of morality

## analyzing with an ANOVA rather than mixed effects models
ezANOVA(data=d.1.btw,
        dv = happiness,
        wid=subj,
        between=.(morality,group,scenario))

## Qucik check for demographic variables 
summary(aov(lm(happiness ~ morality * group * scenario * age * gender * ses * education,data=d.1.btw)))

## using the research scale as a continuous predictor rather than grouping and treating as a factor
lm1.4 <- lmer(happiness ~ morality + researchScale + (1|scenario), data=d.1.btw)  #base model
lm1.5 <- lmer(happiness ~ morality * researchScale + (1|scenario), data=d.1.btw) 
anova(lm1.4,lm1.5) # test for group*morality interaction
summary(lm1.5)

## t-test for main effect of morality
aggregate(happiness ~ morality, d.1.btw, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
var.test(d.1.btw$happiness[d.1.btw$morality=="Immoral"],
         d.1.btw$happiness[d.1.btw$morality=="Moral"])
t.test(d.1.btw$happiness[d.1.btw$morality=="Immoral"],
       d.1.btw$happiness[d.1.btw$morality=="Moral"])
cohensD(d.1.btw$happiness[d.1.btw$morality=="Immoral"],
       d.1.btw$happiness[d.1.btw$morality=="Moral"])

#within
#d.1.wtn <- d.1[d.1$withIn_between=="Within",] ## use this instead if you want to include participants
d.1.wtn <- d.1[d.1$withIn_between=="Within" & d.1$control==3,]
keep <- d.1.wtn$subj[duplicated(d.1.wtn$subj)]
d.1.wtn <- d.1.wtn[!is.na(d.1.wtn$subj),]
d.1.wtn <- d.1.wtn[d.1.wtn$subj %in% keep,]
length(unique(d.1$subj[d.1$withIn_between=="Within"]))-length(unique(d.1.wtn$subj)) ##94 excluded between subjects

lm1.10 <- lmer(happiness ~ morality + group + (1|scenario) + (1|subj), data=d.1.wtn)  #base model
lm1.11 <- lmer(happiness ~ morality * group + (1|scenario) + (1|subj), data=d.1.wtn) 
anova(lm1.10,lm1.11) # test for group*morality interaction
summary(lm1.11)

lm1.12 <- lmer(happiness ~ morality + (1|scenario) + (1|subj), data=d.1.wtn) 
anova(lm1.10,lm1.12) # test for main effect of group

lm1.13 <- lmer(happiness ~ group + (1|scenario) + (1|subj), data=d.1.wtn) 
anova(lm1.10,lm1.13) # test for main effect of morality


## analyzing with an ANOVA rather than mixed effects models
ezANOVA(data=d.1.wtn,
        dv = happiness,
        wid=subj,
        between=.(group,scenario),
        within = morality
        )

## Qucik check for demographic variables 
summary(aov(lm(happiness ~ morality * group * scenario * age * gender * ses * education,data=d.1.wtn)))

aggregate(happiness ~ morality * group, d.1.wtn, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))

## this converts the within-subejcts data to the correct format for the t-test for morality
d.1.wtn <- d.1.wtn[with(d.1.wtn, order(morality,subj)),]

var.test(d.1.wtn$happiness[d.1.wtn$morality=="Moral"],
         d.1.wtn$happiness[d.1.wtn$morality=="Immoral"])
t.test(d.1.wtn$happiness[d.1.wtn$morality=="Moral"],
       d.1.wtn$happiness[d.1.wtn$morality=="Immoral"],paired=T)
cohensD(d.1.wtn$happiness[d.1.wtn$morality=="Moral"],
       d.1.wtn$happiness[d.1.wtn$morality=="Immoral"], method = "paired")

## Figure 1
d.1.all <- rbind(d.1.wtn,d.1.btw)
d1.sum <- ddply(d.1.all, c("group","withIn_between","morality"), summarise,
                 N    = length(happiness),
                 mean = mean(happiness, na.rm=TRUE),
                 sd   = sd(happiness,na.rm=TRUE),
                 se   = sd / sqrt(N) )

d1.plot <- ggplot(d1.sum, aes(x=group, y=mean, fill=morality)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  facet_wrap(~withIn_between) +
  ylab("Happiness Attribution Agreement Rating") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.7))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d1.plot,filename="Fig2.jpg", path="Figures", dpi=600,width=13,height=6)

## Justifactions
d.1.wtn <- d.1[d.1$withIn_between=="Within" & d.1$coding!="No difference in happiness",]
d.1.wtn$coding <- factor(d.1.wtn$coding)
#d.1.wtn <- d.1[d.1$withIn_between=="Within" & d.1$control==3,]

table(d.1.wtn$coding)/207 ## 207 is the total number of responses coded

d.1.just <- aggregate(coding ~ group, d.1.wtn, FUN=table)
print(d.1.just)

chisq.test(d.1.just[[2]])

# Study 2 -----------------------------------------------------------------

d.2$subj <- 1:length(d.2$ipAddress)
d.2$lifeValue <- factor(c("Immoral","Moral")[d.2$lifeValue+1])
d.2$learningCondition <- factor(c("Happiness is Bad","Happiness is Good")[d.2$learningCondition+1])

d.2 <- subset(d.2, d.2$factControl==1)

## Preliminary Analyses
aggregate(happinessIsGood ~ learningCondition, d.2, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
var.test(d.2$happinessIsGood[d.2$learningCondition=="Happiness is Bad"],
       d.2$happinessIsGood[d.2$learningCondition=="Happiness is Good"])
t.test(d.2$happinessIsGood[d.2$learningCondition=="Happiness is Bad"],
       d.2$happinessIsGood[d.2$learningCondition=="Happiness is Good"])
cohensD(d.2$happinessIsGood[d.2$learningCondition=="Happiness is Bad"],
        d.2$happinessIsGood[d.2$learningCondition=="Happiness is Good"])


aggregate(changeInHappiness ~ learningCondition, d.2, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
var.test(d.2$changeInHappiness[d.2$learningCondition=="Happiness is Bad"],
       d.2$changeInHappiness[d.2$learningCondition=="Happiness is Good"])
t.test(d.2$changeInHappiness[d.2$learningCondition=="Happiness is Bad"],
       d.2$changeInHappiness[d.2$learningCondition=="Happiness is Good"],var.equal = T)
cohensD(d.2$changeInHappiness[d.2$learningCondition=="Happiness is Bad"],
       d.2$changeInHappiness[d.2$learningCondition=="Happiness is Good"])

aggregate(happyMood ~ learningCondition, d.2, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
var.test(d.2$happyMood[d.2$learningCondition=="Happiness is Bad"],  
       d.2$happyMood[d.2$learningCondition=="Happiness is Good"])
t.test(d.2$happyMood[d.2$learningCondition=="Happiness is Bad"],  
       d.2$happyMood[d.2$learningCondition=="Happiness is Good"],var.equal = T)
cohensD(d.2$happyMood[d.2$learningCondition=="Happiness is Bad"],  
        d.2$happyMood[d.2$learningCondition=="Happiness is Good"])

## Primary Analyses
## analyzed without mixed effects models because there are no random factors to be modeled
ezANOVA(data=d.2,
        dv = happyAttribution,
        wid=subj,
        between=.(lifeValue,learningCondition))

## Qucik check for demographic variables 
summary(aov(lm(happyAttribution ~ lifeValue * learningCondition * age * gender * ses * education,data=d.2)))

aggregate(happyAttribution ~ lifeValue, d.2, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
var.test(d.2$happyAttribution[d.2$lifeValue=="Immoral"],
         d.2$happyAttribution[d.2$lifeValue=="Moral"])
t.test(d.2$happyAttribution[d.2$lifeValue=="Immoral"],
       d.2$happyAttribution[d.2$lifeValue=="Moral"])
cohensD(d.2$happyAttribution[d.2$lifeValue=="Immoral"],
        d.2$happyAttribution[d.2$lifeValue=="Moral"],method="unequal")

## Study 2 Graph
d2.sum <- ddply(d.2, c("learningCondition","lifeValue"), summarise,
                N    = length(happyAttribution),
                mean = mean(happyAttribution, na.rm=TRUE),
                sd   = sd(happyAttribution,na.rm=TRUE),
                se   = sd / sqrt(N) )

d2.plot <- ggplot(d2.sum, aes(x=lifeValue, y=mean, fill=lifeValue)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  facet_wrap(~learningCondition) +
  ylab("Happiness Attribution Agreement Rating") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="none"
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.6))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d2.plot,filename="Fig3.jpg", path="Figures", dpi=600, width = 10, height=6)

# Study 3 -----------------------------------------------------------------

## Moral question pre-test 

d.3a <- d.3a[,c(1,13,16,19,22,25:31)] 
d.3a$time <- rowSums(d.3a[,c(3,5)],na.rm = T)
#hist(d.3a$time[d.3a$time<100],col="Red",breaks=50)

d.3a$moral <- rowSums(d.3a[,c(2,4)],na.rm = T)
d.3a$moral <- 8 - d.3a$moral
d.3a$condition <- factor(d.3a$DO.BR.FL_14)

var.test(d.3a$moral[d.3a$condition=="Immoral"],d.3a$moral[d.3a$condition=="Evil"])
t.test(d.3a$moral[d.3a$condition=="Immoral"],d.3a$moral[d.3a$condition=="Evil"],var.equal = T)
cohensD(d.3a$moral[d.3a$condition=="Immoral"],d.3a$moral[d.3a$condition=="Evil"])

d.3a$moral <- 8-d.3a$moral

## Happiness question

d.3b <- d.3b[,c(1,13,16,19,22,25:31)]
d.3b$time <- rowSums(d.3b[,c(3,5)],na.rm = T)
#hist(d.3b$time[d.3b$time<100],col="Red",breaks=50)

d.3b$happy <- rowSums(d.3b[,c(2,4)],na.rm = T)
d.3b$condition <- factor(d.3b$DO.BR.FL_14)

var.test(d.3b$happy[d.3b$condition=="Immoral"],d.3b$happy[d.3b$condition=="Evil"])
t.test(d.3b$happy[d.3b$condition=="Immoral"],d.3b$happy[d.3b$condition=="Evil"],var.equal = T)
cohensD(d.3b$happy[d.3b$condition=="Immoral"],d.3b$happy[d.3b$condition=="Evil"])

## Figure 4
d.3a$question <- "Rating of Moral Goodness"
d.3b$question <- "Rating of Happiness"

names(d.3a)[c(2,14)] <- c("immoHappy","rating")
names(d.3b)[14] <- "rating"

d.3 <- rbind(d.3a,d.3b)

d3.sum <- ddply(d.3, c("condition","question"), summarise,
                 N    = length(rating),
                 mean = mean(rating, na.rm=TRUE),
                 sd   = sd(rating,na.rm=TRUE),
                 se   = sd / sqrt(N) )

d3.sum$condition <- factor(c("Evil Agent","Immoral Agent")[d3.sum$condition])
d3.sum$condition <- factor(d3.sum$condition,levels=c("Immoral Agent","Evil Agent"))

d3.sum$question <- factor(d3.sum$question, levels =c("Rating of Moral Goodness","Rating of Happiness"))

d3.plot <- ggplot(d3.sum, aes(x=condition, y=mean, fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  facet_wrap(~question) +
  ylab("Assessment of the Agent") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position = "none"
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.75))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.6))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d3.plot,filename="Fig4.jpg", path="Figures", dpi=600, width = 10, height=6)

# Study 4a -----------------------------------------------------------------

## Assign life value
d.4a$lifeValue[d.4a$DO.BR.FL_14=="(1) Immoral" 
               |d.4a$DO.BR.FL_14=="(2) Immoral"
               |d.4a$DO.BR.FL_14=="(3) Immoral" 
               |d.4a$DO.BR.FL_14=="(4) Immoral"] <- "Immoral"
d.4a$lifeValue[d.4a$DO.BR.FL_14=="(1) Moral" 
               |d.4a$DO.BR.FL_14=="(2) Moral"
               |d.4a$DO.BR.FL_14=="(3) Moral" 
               |d.4a$DO.BR.FL_14=="(4) Moral"] <- "Moral"

## assign scenario
d.4a$scenario[d.4a$DO.BR.FL_14=="(1) Moral" |d.4a$DO.BR.FL_14=="(1) Immoral"] <- "Nurse"  
d.4a$scenario[d.4a$DO.BR.FL_14=="(2) Moral" |d.4a$DO.BR.FL_14=="(2) Immoral"] <- "Mother"  
d.4a$scenario[d.4a$DO.BR.FL_14=="(3) Moral" |d.4a$DO.BR.FL_14=="(3) Immoral"] <- "Janitor"  
d.4a$scenario[d.4a$DO.BR.FL_14=="(4) Moral" |d.4a$DO.BR.FL_14=="(4) Immoral"] <- "Uncle"  

d.4a <- melt(d.4a, measure.vars = c(17,22,28,33,39,44,50,55,61,66,72,77,83,88,94,99), id.vars=c("ResponseID","lifeValue","scenario"),na.rm = T)

d.4a$qCode <- substring(d.4a$variable,5,10)
d.4a$qCode <- factor(d.4a$qCode)

d.4a$attributionType <- "XX"

d.4a$attributionType[d.4a$qCode=="Feel"] <- "Emotion Assessment" 
d.4a$attributionType[d.4a$qCode=="Happy"] <- "Happiness Assessment" 
d.4a$attributionType[d.4a$qCode=="Happy."] <- "Happiness Assessment" 

d4a.sum <- ddply(d.4a, c("attributionType","lifeValue"), summarise,
                 N    = length(value),
                 mean = mean(value, na.rm=TRUE),
                 sd   = sd(value,na.rm=TRUE),
                 se   = sd / sqrt(N) )

d4a.sum$attributionType <- factor(d4a.sum$attributionType, levels=c("Happiness Assessment","Emotion Assessment"))

d4a.plot <- ggplot(d4a.sum, aes(x=lifeValue, y=mean, fill=lifeValue)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~attributionType) +
  scale_fill_manual(values=blackGreyPalette) + 
  ylab("Attribution Agreement Rating") +
  xlab("") +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_blank()
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.6))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d4a.plot,filename="fig5.jpg", path="Figures", dpi=600, width = 10, height=6)

# Primary Analyses

## interaction effect 
lm4a.0 <- lmer(value ~ lifeValue * attributionType + (lifeValue + attributionType | scenario), data=d.4a)
lm4a.1 <- lmer(value ~ lifeValue + attributionType + (lifeValue + attributionType | scenario), data=d.4a)
anova(lm4a.0,lm4a.1)

## effect of life value 
lm4a.2 <- lmer(value ~ attributionType + (lifeValue + attributionType | scenario), data=d.4a)
anova(lm4a.1,lm4a.2)

## effect of attribution type 
lm4a.3 <- lmer(value ~ lifeValue + (lifeValue + attributionType | scenario), data=d.4a)
anova(lm4a.1,lm4a.3)

# decomposing lifeValue * attributionType interaction
aggregate(value ~ lifeValue*attributionType, d.4a, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))

## happiness assessments
var.test(d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Immoral"],
         d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Moral"])
t.test(d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Immoral"], 
       d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Moral"])
cohensD(d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Immoral"], 
        d.4a$value[d.4a$attributionType=="Happiness Assessment" & d.4a$lifeValue=="Moral"])

## emotion assessments
var.test(d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Immoral"],
         d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Moral"])
t.test(d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Immoral"], 
       d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Moral"],var.equal = T)
cohensD(d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Immoral"],
        d.4a$value[d.4a$attributionType=="Emotion Assessment" & d.4a$lifeValue=="Moral"])

# analysis without lmers
anova(lm(value ~ lifeValue * attributionType * scenario, data=d.4a))
etaSquared(lm(value ~ lifeValue * attributionType * scenario, data=d.4a))

# Study 4b -----------------------------------------------------------------

d.4b$scenario <- factor(c("Janitor","Uncle","Nurse","Mother")[d.4b$scenario])
d.4b$lifeValue <- factor(c("Immoral","Moral")[d.4b$lifeValue+1])
d.4b$psychState <- factor(c("Negative States","Positive States")[d.4b$psychState+1])
d.4b$attributionType <- factor(c("Emotion Assessment","Happiness Assessment")[d.4b$attributionType])
d.4b$attributionType <- factor(d.4b$attributionType, levels=c("Happiness Assessment","Emotion Assessment"))

# Primary Analyses
#overall model
lm4b.1 <- lmer(happyAttribution ~ lifeValue * psychState * attributionType + (1|scenario), data=d.4b) 

# test for 3-way interaction: lifeValue*psychState*attributionType
lm4b.2 <- lmer(happyAttribution ~ (lifeValue*psychState) + (lifeValue*attributionType) + (psychState*attributionType) + (1|scenario), data=d.4b) 
anova(lm4b.1,lm4b.2) 

# test for 2-way lifeValue*attributionType interaction
lm4b.4 <- lmer(happyAttribution ~ (lifeValue*psychState) + (psychState*attributionType) + (1|scenario), data=d.4b)
anova(lm4b.2,lm4b.4)

# decomposing lifeValue * attributionType interaction
aggregate(happyAttribution ~ lifeValue*attributionType, d.4b, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
## happiness assessments
var.test(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Immoral"],
         d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Moral"])
t.test(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Immoral"], 
       d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Moral"],var.equal = T)
cohensD(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Immoral"],
        d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$lifeValue=="Moral"])

## emotion assessments
var.test(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Immoral"],
         d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Moral"])
t.test(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Immoral"], 
       d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Moral"],var.equal = T)
cohensD(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Immoral"],
        d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$lifeValue=="Moral"])

# test for 2-way attributionType*psychState interaction
lm4b.5 <- lmer(happyAttribution ~ (lifeValue*attributionType) + (lifeValue*psychState) + (1|scenario), data=d.4b)
anova(lm4b.2,lm4b.5)

# decomposing attributionType * psychState interaction
aggregate(happyAttribution ~ attributionType * psychState, d.4b, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
## happiness assessments
var.test(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Positive States"],
         d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Negative States"])
t.test(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Positive States"],
       d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Negative States"])
cohensD(d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Positive States"],
        d.4b$happyAttribution[d.4b$attributionType=="Happiness Assessment" & d.4b$psychState=="Negative States"])

## emotion assessments
var.test(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Positive States"],
         d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Negative States"])
t.test(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Positive States"],
       d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Negative States"])
cohensD(d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Positive States"],
        d.4b$happyAttribution[d.4b$attributionType=="Emotion Assessment" & d.4b$psychState=="Negative States"])

# test for 2-way psychState*lifeValue interaction
lm4b.6 <- lmer(happyAttribution ~ (lifeValue*attributionType) + (psychState*attributionType) + (1|scenario), data=d.4b)
anova(lm4b.2,lm4b.6)

# decomposing marginal psychState * lifeValue interaction
aggregate(happyAttribution ~ psychState*lifeValue, d.4b, FUN=function(x) c(M=mean(x),SD=sd(x),n=length(x)))
## positive states
var.test(d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Immoral"],
         d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Moral"])
t.test(d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Immoral"], 
       d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Moral"])
cohensD(d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Immoral"],
        d.4b$happyAttribution[d.4b$psychState=="Positive States" & d.4b$lifeValue=="Moral"])

## negative states
var.test(d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Immoral"],
         d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Moral"])
t.test(d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Immoral"], 
       d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Moral"],var.equal = T)
cohensD(d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Immoral"],
        d.4b$happyAttribution[d.4b$psychState=="Negative States" & d.4b$lifeValue=="Moral"])

#main effects
# Life Value
lm4b.7 <- lmer(happyAttribution ~ lifeValue + attributionType*psychState + (1|scenario), data=d.4b)
lm4b.8 <- lmer(happyAttribution ~ attributionType*psychState + (1|scenario), data=d.4b)
anova(lm4b.7,lm4b.8)

# Psychological States
lm4b.9 <- lmer(happyAttribution ~ psychState + attributionType*lifeValue + (1|scenario), data=d.4b)
lm4b.10 <- lmer(happyAttribution ~ attributionType*lifeValue + (1|scenario), data=d.4b)
anova(lm4b.9,lm4b.10)

# Assessment Type
lm4b.11 <- lmer(happyAttribution ~ attributionType + psychState*lifeValue + (1|scenario), data=d.4b)
lm4b.12 <- lmer(happyAttribution ~ psychState*lifeValue + (1|scenario), data=d.4b)
anova(lm4b.11,lm4b.12)

## simple ANOVA analysis 
ezANOVA(data=d.4b,
        dv = happyAttribution,
        wid=subj,
        between=.(lifeValue,psychState,attributionType,scenario))

## Quick check for demographic variables 
summary(aov(lm(happyAttribution ~ lifeValue * psychState * attributionType * scenario * age * gender * ses * education,data=d.4b)))

## Study 4b Graph
d4b.sum <- ddply(d.4b, c("attributionType","lifeValue","psychState"), summarise,
                 N    = length(happyAttribution),
                 mean = mean(happyAttribution, na.rm=TRUE),
                 sd   = sd(happyAttribution,na.rm=TRUE),
                 se   = sd / sqrt(N) )

d4b.plot <- ggplot(d4b.sum, aes(x=psychState, y=mean, fill=lifeValue)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  ylab("Happiness Attribution Agreement Rating") +
  xlab("") +
  facet_wrap(~attributionType) +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.6))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d4b.plot,filename="Fig6.jpg", path="Figures", dpi=600, width = 10, height=6)
## The face scale that appears in the figure in the paper was subsequently added in Adobe Illustrator

# Study 5  -----------------------------------------------------------------

d.5 <- d.5[,c(1,12,15,17,20,22,25,27,30,32,35,37,40,42,45,47,50,52,55,57,60,62,65,67,70,72,75,77,80,82,85,87,90,92,95,97,100,102,105,107,110,112,115,117,120,122,125,127,130,133:147)]

## assign good vs. bad life
d.5$lifeValue[d.5$DO.BR.FL_14=="Moral - Good - No mental states"|d.5$DO.BR.FL_14=="Non-moral - Good - No mental states"
              |d.5$DO.BR.FL_14=="Moral - Good - Mental states"|d.5$DO.BR.FL_14=="Non-moral - Good - Mental states"] <- "Good life"
d.5$lifeValue[d.5$DO.BR.FL_14=="Moral - Bad - No mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - No mental states"
              |d.5$DO.BR.FL_14=="Moral - Bad - Mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - Mental states"] <- "Bad life"
## assign moral vs non-moral
d.5$morality[d.5$DO.BR.FL_14=="Moral - Good - No mental states"|d.5$DO.BR.FL_14=="Moral - Bad - No mental states"
              |d.5$DO.BR.FL_14=="Moral - Good - Mental states"|d.5$DO.BR.FL_14=="Moral - Bad - Mental states"] <- "Moral"
d.5$morality[d.5$DO.BR.FL_14=="Non-moral - Good - No mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - No mental states"
              |d.5$DO.BR.FL_14=="Non-moral - Good - Mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - Mental states"] <- "Non-moral"
## assign mental state vs. none
d.5$psychStates[d.5$DO.BR.FL_14=="Moral - Good - No mental states"|d.5$DO.BR.FL_14=="Moral - Bad - No mental states"
              |d.5$DO.BR.FL_14=="Non-moral - Good - No mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - No mental states"] <- "No psych info"
d.5$psychStates[d.5$DO.BR.FL_14=="Moral - Good - Mental states"|d.5$DO.BR.FL_14=="Moral - Bad - Mental states"
              |d.5$DO.BR.FL_14=="Non-moral - Good - Mental states"|d.5$DO.BR.FL_14=="Non-moral - Bad - Mental states"] <- "Psych info"

d.5$happy <- rowMeans(d.5[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48)],na.rm=T)

d.5$time <- rowSums(d.5[,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49)],na.rm=T)
hist(d.5$time[d.5$time<200],col="Red",breaks=100)

a <- rep(1:24)*2 # this is to pick out the columns with happiness judgments
d.5l <- melt(d.5[,c(1,a,65:67)],id.vars=c("ResponseID","morality","lifeValue","psychStates"),na.rm = T)

d.5l$agent <- substr(d.5l$variable,1,1)
d.5l$agent <- factor(d.5l$agent)
d.5l$agent <- factor(c("Garrett","Sarah","Tom")[d.5l$agent])
  
## Study 5 Graph

d5.sum1 <- ddply(d.5, c("lifeValue","morality","psychStates","ResponseID"), summarise,
                 subjMean = mean(happy, na.rm=TRUE) )

d5.sum <- ddply(d5.sum1, c("lifeValue","morality","psychStates"), summarise,
                N    = length(subjMean),
                mean = mean(subjMean, na.rm=TRUE),
                sd   = sd(subjMean,na.rm=TRUE),
                se   = sd / sqrt(N) )

d5.sum$morality <- factor(d5.sum$morality)
d5.sum$morality <- factor(c("Moral Value","Non-moral Value")[d5.sum$morality])

d5.sum$psychStates <- factor(d5.sum$psychStates)
d5.sum$psychStates <- factor(c("No Information","Positive Information")[d5.sum$psychStates])

d5.plot <- ggplot(d5.sum, aes(x=psychStates, y=mean, fill=lifeValue)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=blackGreyPalette) + 
  ylab("Happiness Assessment") +
  xlab("") +
  facet_wrap(~morality) +
  coord_cartesian(ylim=c(1,7)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, position=position_dodge(.9)) +
  theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.title=element_blank()
    ,legend.text=element_text(size=rel(1.5))
    ,axis.text.x=element_text(size=rel(1.5))
    ,axis.text.y=element_text(size=rel(1.25))
    ,axis.title=element_text(size=rel(1.5))
    ,strip.text = element_text(size = rel(1.6))
    ,axis.title.y = element_text(vjust = 0.75)
  )

ggsave(plot=d5.plot,filename="Fig7.jpg", path="Figures", dpi=600, width = 10, height=6)

### analysis

lm5.0 <- lmer(value ~ lifeValue * morality * psychStates * agent + (1|ResponseID),data=d.5l)
lmerTest::anova(lm5.0)
## three way interaction
lm5.1 <- lmer(value ~ (lifeValue * morality) + (psychStates * morality) + (lifeValue * psychStates) * agent + (1|ResponseID),data=d.5l)
anova(lm5.0,lm5.1)
## two-way interaction life value * morality
lm5.2 <- lmer(value ~ (psychStates * morality) + (lifeValue * psychStates) * agent + (1|ResponseID),data=d.5l)
anova(lm5.1,lm5.2)
## two-way interaction psychStates *morality
lm5.3 <- lmer(value ~ (lifeValue * morality) + (lifeValue * psychStates) * agent + (1|ResponseID),data=d.5l)
anova(lm5.1,lm5.3)
## two-way interaction psychStates * lifeValue
lm5.4 <- lmer(value ~ (lifeValue * morality) + (morality * psychStates) * agent + (1|ResponseID),data=d.5l)
anova(lm5.1,lm5.4)

## Moral value conditions
lm5.5 <- lmer(value ~ lifeValue * psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Moral",])
lm5.6 <- lmer(value ~ lifeValue + psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Moral",])
anova(lm5.5,lm5.6)

## Non-Moral value conditions
lm5.7 <- lmer(value ~ lifeValue * psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Non-moral",])
lm5.8 <- lmer(value ~ lifeValue + psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Non-moral",])
anova(lm5.7,lm5.8)

## Moral value - No Psych Info
aggregate(happy ~ lifeValue, FUN = function(x) c(mean = mean(x), sd = sd(x, na.rm=T)), data=d.5[d.5$psychStates=="No psych info" & d.5$morality=="Moral",])
var.test(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])
t.test(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
       d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])
cohensD(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
        d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])


## Moral value - Psych Info
aggregate(happy ~ lifeValue, FUN = function(x) c(mean = mean(x), sd = sd(x, na.rm=T)), data=d.5[d.5$psychStates=="Psych info" & d.5$morality=="Moral",])
var.test(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])
t.test(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
       d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])
cohensD(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Bad life"],
       d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Moral" & d.5$lifeValue=="Good life"])


## Non-moral value conditions
lm5.7 <- lmer(value ~ lifeValue * psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Non-moral",])
lm5.8 <- lmer(value ~ lifeValue + psychStates * agent + (1|ResponseID),data=d.5l[d.5l$morality=="Non-moral",])
anova(lm5.7,lm5.8)

## Non-moral value - No psych States
aggregate(happy ~ lifeValue, FUN = function(x) c(mean = mean(x), sd = sd(x, na.rm=T)), data=d.5[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral",])
var.test(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"])
t.test(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
       d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"])
cohensD(d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="No psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"])

## Non-moral value - Psych States
aggregate(happy ~ lifeValue, FUN = function(x) c(mean = mean(x), sd = sd(x, na.rm=T)), data=d.5[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral",])
var.test(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"])
t.test(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
       d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"],var.equal = T)
cohensD(d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Bad life"],
         d.5$happy[d.5$psychStates=="Psych info" & d.5$morality=="Non-moral" & d.5$lifeValue=="Good life"])

# analysis relying on a simpel anova
anova(lm(happy~psychStates*lifeValue*morality,data=d.5))
## 2x2 for moral
anova(lm(happy~psychStates*lifeValue,data=d.5[d.5$morality=="Moral",]))


