library(survey)
gss_df <- readRDS("GSS2016_use.rds") # survey data
acs_df <- readRDS("ACS2016_use.rds") # auxiliary information

###Make variables to post-stratify on
gss_df$full_crossed <- as.character(factor(gss_df$edu5):factor(gss_df$sex):factor(gss_df$race3))
acs_df$full_crossed <- as.character(factor(acs_df$edu5):factor(acs_df$sex):factor(acs_df$race3))


###Framework for specifying a sampling frame -- basically set up so that samples
###are ddrawn randomly within two sets of nested strata 
###First sample a cluster (specified in id) then draw from within that cluster
##and then from a stratum. Prob var specifies probability of sampling each cluster 
###or alternately weights specifies the weights to be used for eah cluster
###can also specify id and strata as formulas compoose of different covariates
###This code loads the gss data
gss_dwt <- svydesign(ids = ~vpsu, weights = ~design_wt, strata = ~vstrat,
                     data = gss_df, nest = TRUE)



###This does the same for the census (acs specifically) raw survey data
###note id=~1 implies there are no clusters
acs_dwt <- svydesign(ids = ~1, weight = ~perwt, data = acs_df)


###helper function to extract marginal mean in format that Survey uses
make_margin_df <- function(var, dwt){
  tab <- svymean(as.formula(paste0('~', var)), dwt)
  to_return <- data.frame(names(tab),c(tab)) 
  colnames(to_return) <- c(var, 'Freq')
  to_return[,1] <- gsub(var, '' ,to_return[,1])
  to_return
}

pop_margins <- lapply(c('edu5', 'sex', 'race3'), make_margin_df, dwt=acs_dwt)

###Use rake function
raked <- rake(design=gss_dwt, sample.margins=list(~edu5,~sex, ~race3), population.margins=pop_margins)
svymean(~gunlaw, raked, na.rm=T)

###Now get the fully crossed means
interacted_means <- svymean(~full_crossed, acs_dwt)

###get pop frequences in the right format
interacted_means <- data.frame(full_crossed=gsub('full_crossed', '', names(interacted_means)), Freq=as.matrix(interacted_means)[,1])

pst <- postStratify(design=gss_dwt, strata=~full_crossed, population=interacted_means)
svymean(~gunlaw, pst, na.rm=T)


####Let's compare with MRP
library(dplyr)
library(lme4)

##Get pop frequencies
pop_targets <- group_by(acs_df, edu5, sex, race3) %>%
  summarize(freq=sum(perwt)/sum(acs_df$perwt))
  
###fit a mixed effects model for qoi
mod <- lmer(I(gss_df$gunlaw=='favor')~edu5 + sex + race3 + (1|edu5:sex:race3), gss_df)
sum(predict(mod, pop_targets) * pop_targets$freq)
