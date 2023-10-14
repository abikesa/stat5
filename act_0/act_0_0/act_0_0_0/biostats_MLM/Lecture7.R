rm(list=ls())

data_path <- "~/Desktop/JohnsHopkins/TA/MLM/Data"   ## change me!


###################
#  load packages  #
###################
pckgs <- c("tidyverse","ggplot2", "lme4")

sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
  install.packages(x)
  require(x, character.only=TRUE)
})
rm(list=c("pckgs"))


###############
#  load data  #
###############
  
data <- read_csv(file.path(data_path, "gcse.csv"))
data <- 
        data %>% 
        group_by(school) %>% 
        mutate(totalstudents = n(),                ## number of students within each school
               withinschoolcount = 1:n(),          ## counter for # of students
               school_mean_lrt = mean(lrt),        ## mean lrt by school
               lrt_groupc = lrt - school_mean_lrt, ## group mean center lrt
               school_fac = factor(school)         ## create factor school variable for use in regressions
                ) %>% 
        ungroup() 

## What is the distribution of number of students in each school
data %>% 
        group_by(school) %>% 
        slice(1) %>% 
        ungroup() %>% 
        select(totalstudents) %>% 
        summary()
        
## What is the distribution of the gcse and lrt scores
data %>% 
        select(gcse, lrt) %>% 
        summary()


## What is the relationship between gcse and lrt for few schools
data %>% 
        filter(school %in% c(1,2)) %>% 
        ggplot() + 
                geom_point(aes(x=lrt,y=gcse)) +
                geom_smooth(aes(x=lrt,y=gcse), se=FALSE, method="lm") +
                facet_grid(school~.) + 
                theme_bw()


## Create SLR of gcse on lrt for each school and plot the fitted lines
fit_grpc <- lm(gcse ~ school_fac*lrt_groupc, data=data, subset=c(totalstudents >5))
df_pred <- 
        data %>% 
        arrange(school,lrt_groupc) %>% 
        filter(totalstudents > 5) %>% 
        ungroup() %>% 
        data.frame(., "y"=predict(fit_grpc, newdata=.))


df_pred %>% 
        ggplot() + geom_line(aes(x=lrt_groupc,y=y,group=school)) + theme_bw() + 
        xlab("LRT Score (group mean centered)") + 
        ylab("Predicted GCSE Score")
rm(df_pred)


data$intercept <- data$slope  <- NA
uid <- unique(data$school)
for(i in seq_along(uid)){
        inx_i <- which(data$school == uid[i])
        if(uid[i] != 48 & uid[i] <= 65){
                fit_i <- lm(gcse ~ lrt_groupc, data=data[inx_i, , drop=FALSE])
                data$intercept[inx_i] <- coef(fit_i)["(Intercept)"]
                data$slope[inx_i] <- coef(fit_i)["lrt_groupc"]
                rm(fit_i)
        }
        rm(inx_i)
}
rm(i)

## plot slope vs intercept for (most) schools
data %>% 
        ggplot(aes(x=intercept, y=slope)) + geom_point() + 
        geom_smooth(method="lm",se=FALSE)+
        theme_bw() + xlab("Estimated average gcse at school-average lrt") + 
        ylab("Estimated gcse vs. lrt relationship")
        
        

## fit the mixed model
## changed the optimizer to avoid a warning -- not overly relevant in this example
fit_lme <- lmer(gcse ~ lrt_groupc + (lrt_groupc|school_fac), data=data, REML=FALSE,
                control = lmerControl(optimizer ="Nelder_Mead"))

## NOTE: random effects are generally not returned in numeric order! character sorting
##       need to be careful to get the appropriate random effects for each cluster
eb     <- ranef(fit_lme)$school_fac       
bi_var <- t(apply(attributes(eb)$postVar,3,diag))

data$eb_intercept <- data$eb_slope <- data$bi0 <- data$bi1 <- data$bi0_se <- data$bi1_se <- NA
for(i in seq_along(uid)){
        inx_i <- which(data$school == uid[i])
        inx_eb_i <- which(rownames(eb) == uid[i])
        
        ## EB estimates of intercept + slope
        data$eb_intercept[inx_i] <- eb$`(Intercept)`[inx_eb_i] + fit_lme@beta[1]
        data$eb_slope[inx_i]     <- eb$lrt_groupc[inx_eb_i] + fit_lme@beta[2]
        
        data$bi0[inx_i] <- eb$`(Intercept)`[inx_eb_i]
        data$bi1[inx_i] <- eb$lrt_groupc[inx_eb_i] 
        
        
        ## SE of random intercept + slope
        data$bi0_se[inx_i] <- sqrt(bi_var[inx_eb_i,1])
        data$bi1_se[inx_i] <- sqrt(bi_var[inx_eb_i,2])
        rm(inx_i, inx_eb_i)
}
rm(i,uid,eb,bi_var)



data %>% 
        filter(totalstudents >= 5) %>%
        group_by(school) %>% 
        slice(1) %>% 
        ggplot(aes(x=intercept, y=eb_intercept)) + geom_point() + xlim(50,90) + ylim(50,90) + 
        theme_bw()

data %>% 
        filter(totalstudents >= 5) %>%
        group_by(school) %>% 
        slice(1) %>% 
        ggplot(aes(x=slope, y=eb_slope)) + geom_point() + xlim(0,1) + ylim(0,1) + 
        theme_bw()



data %>% 
        group_by(school) %>% 
        slice(1) %>% 
        ungroup() %>% 
        arrange(bi0) %>% 
        mutate(lower = bi0 - 2*bi0_se,
               upper = bi0 + 2*bi0_se,
               rank = 1:n()) %>% 
        ggplot() + 
        geom_point(aes(x=rank, y=bi0)) + 
        geom_errorbar(aes(x=rank,ymin= lower, ymax=upper)) + 
        theme_bw() + geom_hline(aes(yintercept=0), col="red", lty=2,size=2)
