### Title: R Script replicating paper materials
### Paper: Stabilizing through randomizing: A bootstrap approach to estimating item validity and latent concepts
### All data required can be found in the GitHub repository
### NOTE: Requires installation of bootstrap.dyads package, and connecting of appropriate filepaths


call.dr.code(filepath="INPUT-FILEPATH")


#### SECTION 1 - THE IMPACT OF CHANGING DATA ON GB OPINIONS ####

data <- read.csv("GB-Imms-Data.csv")


# Figure 1
# Note: this data is originally a measurement of anti-immigrant sentiment , the '1-' is to flip to that most-liberal is at 1

g <- ggplot2::ggplot(data=data) 

figure1 <- g + ggplot2::geom_line(ggplot2::aes(x=Year, y=1-InclusiveMood)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),) 
figure1


# Figure 2
figure2 <- g + ggplot2::geom_line(ggplot2::aes(x=Year, y=1-NoRaceMood)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),) # 1- to flip to that most-liberal is at 1
figure2


# Figure 3
gb_small <- data[,c(1,4,6)]

gb_long <- reshape2::melt(gb_small, id.vars="Year")

g <- ggplot2::ggplot(data=gb_long[gb_long$Year>2000,])

figure3 <- g + ggplot2::geom_line(ggplot2::aes(x=Year, y=1-value, group=variable, linetype=variable)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(2000,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),) # 1- to flip to that most-liberal is at 1

figure3





#### SECTION 2 - DEMONSTRATING THE BOOTSTRAPPED MODELS ####

### 2.1 - Immigration in Britain

gb_data <- read.csv("GBImm.csv", header=FALSE)

gb_data$V2 <- as.Date(gb_data$V2, format="%m/%d/%Y")

head(gb_data)

## Run single-estimation
GBResults <- extract(data=gb_data, varname="V1", date="V2", index="V3",
                     ncases = "V4", smoothing=TRUE, print=TRUE, log=TRUE,
                     filename = "gb-log-test.txt")


## Re-run with intensive paramters off
GBResults <- extract(data=gb_data, varname="V1", date="V2", index="V3",
                     ncases = "V4", smoothing=TRUE, print=FALSE, log=FALSE, increase = FALSE)


### Run boostrap model
gb_bootstraps <- bootstrapped.extraction(data=gb_data, varname="V1", reps=500, draw=0.30, output=GBResults, print=TRUE)


gb_Analysis <- analyse.model(model=gb_bootstraps, print=TRUE)


### Check the distribution and trend of questions over time
d <- gb_Analysis$`Suggested Input`
head(d)
g <- ggplot2::ggplot(data=d, ggplot2::aes(x=V2, y=V3, group=V1, colour=V1))

g + ggplot2::geom_line()

gb_Analysis$`Bootstrap Suggested Model Result`$frequencies



### Plot the bootstrap and original together
gb_estimates <- as.data.frame(gb_Analysis$`Bootstrap Suggested Model Result`$period)
names(gb_estimates) <- "period"
gb_estimates$latent <- gb_Analysis$`Bootstrap Suggested Model Result`$latent1
gb_estimates$count <- gb_Analysis$`Bootstrap Suggested Model Result`$frequencies$count
gb_estimates$Model <- "Bootstrap Model"

gb_original <- as.data.frame(GBResults$period)
names(gb_original) <- "period"
gb_original$latent <- GBResults$latent1
gb_original$count <- GBResults$frequencies$count
gb_original$Model <- "Single Run"

gb_together <- rbind(gb_estimates, gb_original)


g <- ggplot2::ggplot(data=gb_together, ggplot2::aes(x=period, y=1-latent, group=Model)) # 1- to flip to that most-liberal is at 1

combined_graph_gb <- g + ggplot2::geom_line(ggplot2::aes(linetype=Model)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),)
combined_graph_gb



#### 2.2 - Foreigners in the Netherlands ####

nl_data <- read.csv("DutchOpinions.csv", header=TRUE)

head(nl_data)

nl_data$Date <- as.Date(nl_data$Date, format="%m/%d/%Y")

head(nl_data)

table(nl_data$Variable)

## Run single-estimation
NLResults <- extract(data=nl_data, varname="Variable", date="Date", index="PctLeft",
                     ncases = "N", smoothing=TRUE, print=TRUE, log=TRUE,
                     filename = "nl-log-test.txt")


## Rr-run single-estimation with extra stuff turned off
NLResults <- extract(data=nl_data, varname="Variable", date="Date", index="PctLeft",
                     ncases = "N", smoothing=TRUE, print=FALSE, log=FALSE)


## Run and analyse bootstrapper
nl_bootstraps <- bootstrapped.extraction(data=nl_data, varname="Variable", reps=500, draw=0.30, output=NLResults, print=TRUE)

nl_Analysis <- analyse.model(model=nl_bootstraps, print=TRUE)


### Check the distribution and trend of questions over time
d <- nl_Analysis$`Suggested Input`

g <- ggplot2::ggplot(data=d, ggplot2::aes(x=Date, y=PctLeft, group=Variable, colour=Variable))

g + ggplot2::geom_line()

nl_Analysis$`Bootstrap Suggested Model Result`$frequencies

### Plot the bootstrap and original together
nl_estimates <- as.data.frame(nl_Analysis$`Bootstrap Suggested Model Result`$period)
names(nl_estimates) <- "period"
nl_estimates$latent <- nl_Analysis$`Bootstrap Suggested Model Result`$latent1
nl_estimates$count <- nl_Analysis$`Bootstrap Suggested Model Result`$frequencies$count
nl_estimates$Model <- "Bootstrap Model"

nl_original <- as.data.frame(NLResults$period)
names(nl_original) <- "period"
nl_original$latent <- NLResults$latent1
nl_original$count <- NLResults$frequencies$count
nl_original$Model <- "Single Run"

nl_together <- rbind(nl_estimates, nl_original)


g <- ggplot2::ggplot(data=nl_together, ggplot2::aes(x=period, y=latent, group=Model))

combined_graph_nl <- g + ggplot2::geom_line(ggplot2::aes(linetype=Model)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),)
combined_graph_nl



#### 2.3 - Macro-approval in spain #####

spain <- read.csv("SpainExec.csv")

spain$DATE <- as.Date(spain$DATE, format="%Y-%m-%d")

head(spain)

range(spain$DATE)

table(spain$VARIABLE)

## Run once for single run visualised
ESResults <- extract(data=spain, varname="VARIABLE", date="DATE", index="POSITIVE",
                     ncases = "N", smoothing=TRUE, print=TRUE, log=TRUE, unit="Q", filename = "es-log-test.txt")

## Run with intensive parameters off
ESResults <- extract(data=spain, varname="VARIABLE", date="DATE", index="POSITIVE",
                     ncases = "N", smoothing=TRUE, print=FALSE, log=FALSE, unit="Q")

## Run bootstrap model
bootstraps_es <- bootstrapped.extraction(data=spain, varname="VARIABLE", reps=500, draw=0.30, output=ESResults, print=TRUE)

es_Analysis <- analyse.model(model=bootstraps_es, print=TRUE, sd.cut=0.1)


### Plot the bootstrap and original together
es_estimates <- as.data.frame(es_Analysis$`Bootstrap Suggested Model Result`$period)
names(es_estimates) <- "period"
es_estimates$latent <- es_Analysis$`Bootstrap Suggested Model Result`$latent1
es_estimates$count <- es_Analysis$`Bootstrap Suggested Model Result`$frequencies$count
es_estimates$Model <- "Bootstrap Model"

es_original <- as.data.frame(ESResults$period)
names(es_original) <- "period"
es_original$latent <- ESResults$latent1
es_original$count <- ESResults$frequencies$count
es_original$Model <- "Single Run"

es_together <- rbind(es_estimates, es_original)


g <- ggplot2::ggplot(data=es_together, ggplot2::aes(x=period, y=latent, group=Model))

combined_graph_es <- g + ggplot2::geom_line(ggplot2::aes(linetype=Model)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,5)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),)
combined_graph_es




#### 2.4 - Macro-Approval in the US ####
us_data <- read.csv("USMacroApproval.csv", header=TRUE)

us_data$DATE <- as.Date(us_data$DATE, format="%d/%m/%Y")

head(us_data)

us_data <- subset(us_data, us_data$DATE > "2001-01-20")

range(us_data$DATE)

### Sort out randomly uncapitalised variables
us_data$VARIABLE[us_data$VARIABLE=="Newsweek"] <- "NEWSWEEK"

us_data$VARIABLE[us_data$VARIABLE=="Time"] <- "TIME"

table(us_data$VARIABLE)

## Run single-estimation
USResults <- extract(data=us_data, varname="VARIABLE", date="DATE", index="POSITIVE",
                     ncases = "N", smoothing=TRUE, print=TRUE, log=TRUE,
                     filename = "us-log-test.txt", unit="Q")

## Re-run with intensive paramters off
USResults <- extract(data=us_data, varname="VARIABLE", date="DATE", index="POSITIVE",
                     ncases = "N", smoothing=TRUE, print=FALSE, log=FALSE, unit="Q")


## Run and analyse bootstrapper
bootstraps_us <- bootstrapped.extraction(data=us_data, varname="VARIABLE", reps=500, draw=0.30, output=USResults, print=TRUE)

Analysis_us <- analyse.model(model=bootstraps_us, print=TRUE, sd.cut=0.1)

## Plot the combination graph
US_dat <- as.data.frame(USResults$period)
names(US_dat) <- "period"
US_dat$latent <- USResults$latent1
US_dat$count <- USResults$frequencies$count
US_dat$Model <- "Single Run"

US_ests <- as.data.frame(Analysis_us$`Bootstrap Suggested Model Result`$period)
names(US_ests) <- "period"
US_ests$latent <- Analysis_us$`Bootstrap Suggested Model Result`$latent1
US_ests$count <- Analysis_us$`Bootstrap Suggested Model Result`$frequencies$count
US_ests$Model <- "Bootstrap Model"

US_together <- rbind(US_ests, US_dat)


g <- ggplot2::ggplot(data=US_together, ggplot2::aes(x=period, y=latent, group=Model))

combined_graph_us <- g + ggplot2::geom_line(ggplot2::aes(linetype=Model)) + ggplot2::theme_bw() +
  ggplot2::labs(
    x="Time period",
    y="Estimated latent value") + 
  ggplot2::scale_x_continuous(breaks=seq(1985,2020,2)) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                        panel.grid.minor = ggplot2::element_blank(),)
combined_graph_us

