#### Lab 4 document ####
library(tidyverse)

# Read in the dataset
df <- read_csv("MLM_Lab4/guatemala.csv")
head(df)

## Identify the number of clusters(communities), moms and kids
df %>%
summarise(kids = length(unique(kid)),
          moms = length(unique(mom)),
          cluster = length(unique(cluster)))

## Compute the number of mothers evaluated in each community
df %>% 
group_by(cluster) %>%
summarise(n.moms = length(unique(mom))) %>%
group_by(n.moms) %>%
summarise(n())

## Compute the number of kids per cluster
df %>% 
group_by(cluster) %>%
summarise(n.kids = length(unique(kid))) %>%
group_by(n.kids) %>%
summarise(n())

## Compute the number of kids per mom
df %>% 
group_by(mom) %>%
summarise(n.kids = length(unique(kid))) %>%
group_by(n.kids) %>%
summarise(n())

## What is the prevalence of completing the full course of 
## immunizations
table(df$immun)

## How does the prevalence of completing the full course of
## immunizations differ with and without the campaign
table(df$immun, df$kid2p)

########################################################
########################################################
## Fit the random intercept only model
########################################################
########################################################
library(lme4)
model1 <- glmer(data = df, immun ~ (1|mom) + (1|cluster), 
	    family = binomial(link = "logit"))
summary(model1)

########################################################
########################################################
## Fit the random intercept + kid2p model
########################################################
########################################################
model2 <- glmer(data = df, immun ~ kid2p + (1|mom) + (1|cluster), 
	    family = binomial(link = "logit"))
summary(model2)

########################################################
########################################################
## Fit the random intercept + random slope kid2p model
########################################################
########################################################
model3 <- glmer(data = df, immun ~ kid2p + (1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"))
summary(model3)


########################################################
########################################################
## Fit the random intercept + random slope kid2p model
########################################################
########################################################
model4 <- glmer(data = df, immun ~ kid2p + pcInd + rural+ kid2p:rural + kid2p:pcInd + 
	    	(1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"))
# warning--this model does not converge!!!

model4 <- glmer(data = df, immun ~ kid2p + pcInd + rural+ kid2p:rural + kid2p:pcInd + 
	    	(1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"), nAGQ = 20)

model4 <- glmer(data = df, immun ~ kid2p + pcInd + rural+ kid2p:rural + kid2p:pcInd + 
	    	(1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"), etastart = predict(model3))

model4 <- glmer(data = df, immun ~ kid2p + pcInd + rural+ kid2p:rural + kid2p:pcInd + 
	    	(1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"),
	    control = glmerControl(optimizer = "Nelder_Mead"))

model4 <- glmer(data = df, immun ~ kid2p + pcInd + rural+ kid2p:rural + kid2p:pcInd + 
	    	(1|mom) + (kid2p|cluster), 
	    family = binomial(link = "logit"),
	    control = glmerControl(optimizer = "bobyqa"))
summary(model4)
