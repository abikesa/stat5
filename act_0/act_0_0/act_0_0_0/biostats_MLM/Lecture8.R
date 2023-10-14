rm(list=ls())

data_path <- "~/Desktop/JohnsHopkins/TA/MLM/Data"   ## change me!


###################
#  load packages  #
###################
pckgs <- c("tidyverse","ggplot2", "geepack", "lme4")

sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
        install.packages(x)
        require(x, character.only=TRUE)
})
rm(list=c("pckgs"))

data <- read_csv(file.path(data_path, "pisaUSA2000.csv"))

# Calculate number of schools 
data %>% select(id) %>% unique()

# Calculate number of students by school
data <- 
        data %>% 
        group_by(id) %>% 
        mutate(n_student = n(),
               prop_prof = mean(pass_read),
               neither = as.numeric(high_school==0 & college == 0),
               zero = as.numeric(one_for == 0 & both_for == 0)) %>% 
        ungroup() 
        
# Summarize student characteristics
table(data$female)
table(data$high_school, data$college)
table(data$one_for, data$both_for)
table(data$test_lang)

# Caclulate the proportion of reading proficient students in each school
data <-
        data %>% 
        # calculate the average of level-1 covariates and summarize at the school level
        group_by(id) %>% 
        mutate_at(.vars=vars(one_of(c("isei","female","high_school","college","neither","one_for","both_for","zero","test_lang"))), 
                  .funs=list(mean = ~mean(., na.rm=TRUE))) %>% 
        rename_at(vars(contains("_mean")),list( ~paste("mean", gsub("_mean", "", .), sep = "_") ) ) %>% 
        # explore the within and between effects of gender
        # within effects
        mutate(school1 = mean(pass_read*female)/mean(female),
               school0 = mean(pass_read*(1-female))/mean(1-female)) %>% 
        ungroup() %>% 
        mutate(gender_LOR = log( (school1/(1-school1)) / (school0/(1-school0)) ),
               female_center = female - mean_female)



data %>% group_by(id) %>% slice(1)  %>% 
        ggplot() + geom_histogram(aes(x=gender_LOR),binwidth=0.5) + theme_bw()

data %>% group_by(id) %>% slice(1) %>% ungroup() %>% select(gender_LOR) %>% filter(is.finite(gender_LOR)) %>% summary()


## between effects
## fit the random ntercept only model
fit_glmer <- glmer(pass_read ~ 1 + (1|id), data=data, family = binomial())
bi        <- ranef(fit_glmer)

data <- 
        data %>% 
        mutate(lo = log(prop_prof/(1-prop_prof)),
               lo = replace(lo, !is.finite(lo), NA))

data$lo_b <- NA
uid       <- unique(data$id)
for(i in seq_along(uid)){
        inx_i  <- which(data$id == uid[i])
        inx_bi <- which(rownames(bi$id) == i)
        data$lo_b[inx_i] <- bi$id$`(Intercept)`[inx_bi] + fit_glmer@beta[1]
        rm(inx_i, inx_bi)
}
rm(uid, i, bi, fit_glmer)


data %>% 
        group_by(id) %>% 
        slice(1) %>% 
        ggplot(aes(x=mean_female, y=lo)) + geom_point() + 
        geom_smooth(method="lm",color="blue",se=FALSE) + 
        geom_smooth(method="loess",color="red",se=FALSE) +
        theme_bw()




fit_glmer_f1 <- glmer(pass_read ~ female + (1|id), data=data, family = binomial())
fit_glmer_f2 <- glmer(pass_read ~ female + mean_female + (1|id), data=data, family = binomial())
fit_glmer_f3 <- glmer(pass_read ~ female_center + mean_female + (1|id), data=data, family = binomial())

## Wald test for between = within
vB   <- vcov.merMod(fit_glmer_f3)
a    <- c(0,1,-1)
chi2 <- ((a %*% fit_glmer_f3@beta)/sqrt(t(a) %*% vB %*% a))^2
pchisq(as.vector(chi2), df=1, lower.tail=FALSE)
rm(vB, a, chi2)

fit_glmer_f4 <- glmer(pass_read ~ female_center  + (1|id), data=data, family = binomial())
fit_glmer_f5 <- glmer(pass_read ~ mean_female + (1|id), data=data, family = binomial())





#####################
##  Consider ISEI  ##
#####################

## scale ISEI by 10 units
data <- 
        data %>% 
        mutate(isei10 = isei/10,
               mean_isei10 = mean_isei/10,
               isei10_center = isei10 - mean_isei10)
data$slope <- NA

### Exploratory analysis for within cluster ISEI effect
# estimate school-specific log odds ratio
for(i in setdiff(1:148, c(5:7, 14, 15, 20, 23, 26, 31, 33, 51, 52, 59, 62, 68, 72, 80,
                          82, 84, 87, 95, 97, 99, 104, 105, 114, 117, 120, 122, 125, 127, 128, 
                          135, 139, 143)
                 )){
        inx_i <- which(data$id == i)
        fit_i <- glm(pass_read ~ isei10, family=binomial(), data=data[inx_i,,drop=FALSE])       
        data$slope[inx_i] <- coef(fit_i)["isei10"]
}
data %>% group_by(id) %>% slice(1) %>% ggplot() + geom_histogram(aes(x=slope), binwidth=0.5) + theme_bw()


### Exploratory analysis for the between cluster ISEI effect
data %>% 
        ggplot(aes(x=mean_isei10, y=lo)) + 
        geom_point() + 
        geom_smooth(method="loess", color="red", se=FALSE) + 
        geom_smooth(method="lm", color="blue", se=FALSE) + 
        theme_bw()



## fit the models
## note we change the optimizer for the second/fifth fit -- doesn't affect results
## but gets rid of a warning message
fit_glmer_i1 <- glmer(pass_read ~ isei10 + (1|id), data=data, family = binomial())
fit_glmer_i2 <- glmer(pass_read ~ isei10 + mean_isei10 + (1|id), data=data, family =binomial(),
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                      )
fit_glmer_i3 <- glmer(pass_read ~ isei10_center + mean_isei10 + (1|id), data=data, family = binomial())

## Wald test for between = within
vB   <- vcov.merMod(fit_glmer_i3)
a    <- c(0,1,-1)
chi2 <- ((a %*% fit_glmer_i3@beta)/sqrt(t(a) %*% vB %*% a))^2
pchisq(as.vector(chi2), df=1, lower.tail=FALSE)
rm(vB, a, chi2)

fit_glmer_i4 <- glmer(pass_read ~ isei10_center  + (1|id), data=data, family = binomial())
fit_glmer_i5 <- glmer(pass_read ~ mean_isei10 + (1|id), data=data, family = binomial(),
                      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
                      )


fit_gee_i <- geeglm(pass_read ~ isei10 + mean_isei10, family=binomial, id=id, 
                    na.action=na.omit,data=data, corstr = "exchangeable")
summary(fit_gee_i)


