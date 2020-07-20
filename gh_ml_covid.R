############# LOAD PACKAGES AND DATA
covid <- read.csv("C:/Users/lukep/Box/School Files/Spring 2020/STAT542/Final Project/county_data_apr22.csv")
set.seed(1)

############# DATA CLEANING
### Recode age variables (children, young adults, middle age, early seniors, late seniors)

sum(covid$X.Deaths_04.22.2020 != covid$tot_deaths)
covid$Pop_19below <- rowSums(covid[, c("PopMale.52010","PopFmle.52010", 
                                       "PopMale5.92010", "PopFmle5.92010",
                                       "PopMale10.142010", "PopFmle10.142010", 
                                       "PopMale15.192010", "PopFmle15.192010")]) / covid$CensusPopulation2010
covid$Pop_20_44 <- rowSums(covid[, c("PopMale20.242010", "PopFmle20.242010", 
                                     "PopMale25.292010", "PopFmle25.292010", 
                                     "PopMale30.342010", "PopFmle30.342010", 
                                     "PopMale35.442010",  "PopFmle35.442010")]) / covid$CensusPopulation2010
covid$Pop_45_59 <- rowSums(covid[, c("PopMale45.542010", "PopFmle45.542010", 
                                     "PopMale55.592010", "PopFmle55.592010")]) / covid$CensusPopulation2010
covid$Pop_60_74 <- rowSums(covid[, c("PopMale60.642010", "PopFmle60.642010", 
                                     "PopMale65.742010", "PopFmle65.742010")]) / covid$CensusPopulation2010
covid$Pop_75plus <- rowSums(covid[, c("PopMale75.842010", "PopFmle75.842010", 
                                      "PopMale.842010", "PopFmle.842010")]) / covid$CensusPopulation2010

### Remove variables 

# removed for one of the following reasons:
# - redundant: e.g. STATEFP (StateName), State (StateName), X.EligibleforMedicare (age)
# - too many NA missing at random (MAR): e.g. X3.YrDiabetes2015.17, mortality variables
# - recoded: population variables
# - no variation: e.g. foreign.travel.ban
remove_vars <- c("X", "STATEFP", "COUNTYFP", "State", "lon", "lat", "CensusRegionName",
                 "PopTotalMale2017", "PopTotalFemale2017", "PopulationEstimate65.2017", 
                 "CensusPopulation2010", "X.EligibleforMedicare2018", "X3.YrDiabetes2015.17", 
                 "PopMale.52010", "PopFmle.52010", "PopMale5.92010", "PopFmle5.92010", 
                 "PopMale10.142010", "PopFmle10.142010", "PopMale15.192010", "PopFmle15.192010", 
                 "PopMale20.242010", "PopFmle20.242010", "PopMale25.292010", "PopFmle25.292010", 
                 "PopMale30.342010", "PopFmle30.342010", "PopMale35.442010","PopFmle35.442010", 
                 "PopMale45.542010", "PopFmle45.542010", "PopMale55.592010", "PopFmle55.592010", 
                 "PopMale60.642010", "PopFmle60.642010", "PopMale65.742010", "PopFmle65.742010", 
                 "PopMale75.842010","PopFmle75.842010", "PopMale.842010", "PopFmle.842010", 
                 "X3.YrMortalityAge.1Year2015.17", "X3.YrMortalityAge1.4Years2015.17", 
                 "X3.YrMortalityAge5.14Years2015.17", "X3.YrMortalityAge15.24Years2015.17", 
                 "X3.YrMortalityAge25.34Years2015.17", "X3.YrMortalityAge35.44Years2015.17",
                 "X3.YrMortalityAge45.54Years2015.17", "X3.YrMortalityAge55.64Years2015.17", 
                 "X3.YrMortalityAge65.74Years2015.17", "X3.YrMortalityAge75.84Years2015.17", 
                 "X3.YrMortalityAge85.Years2015.17", "mortality2015.17Estimated",
                 "federal.guidelines", "foreign.travel.ban", "HPSAServedPop", "HPSAUnderservedPop",
                 "deaths", "cases", "X.Deaths_04.22.2020", "X.Cases_04.22.2020")
covid <- covid[, !colnames(covid) %in% remove_vars]

### Fix features
# make percentage
covid$MedicareEnrollment.AgedTot2017 <- covid$MedicareEnrollment.AgedTot2017 / covid$PopulationEstimate2018 * 100

# scale to population (per 100,000 residents)
covid$X.FTEHospitalTotal2017 <- covid$X.FTEHospitalTotal2017 / (covid$PopulationEstimate2018 / 100000)
covid$TotalM.D..s.TotNon.FedandFed2017 <- covid$TotalM.D..s.TotNon.FedandFed2017 / (covid$PopulationEstimate2018 / 100000)
covid$X.HospParticipatinginNetwork2017 <- covid$X.HospParticipatinginNetwork2017 / (covid$PopulationEstimate2018 / 100000)
covid$X.Hospitals <- covid$X.Hospitals / (covid$PopulationEstimate2018 / 100000)
covid$X.ICU_beds <- covid$X.ICU_beds / (covid$PopulationEstimate2018 / 100000)


# impute 0 for NA (no shortages)
covid$HPSAShortage[is.na(covid$HPSAShortage)] <- 0

# convert social distancing dates to "days since april 22"
# april 22, 2020 = 737537
apr22 <- 737537
covid$stay.at.home <- apr22 - covid$stay.at.home
covid$X.50.gatherings <- apr22 - covid$X.50.gatherings
covid$X.500.gatherings <- apr22 - covid$X.500.gatherings
covid$public.schools <- apr22 - covid$public.schools
covid$restaurant.dine.in <- apr22 - covid$restaurant.dine.in
covid$entertainment.gym <- apr22 - covid$entertainment.gym
# impute zero for rest 
covid$stay.at.home[is.na(covid$stay.at.home)] <- 0
covid$X.50.gatherings[is.na(covid$X.50.gatherings)] <- 0
covid$X.500.gatherings[is.na(covid$X.500.gatherings)] <- 0
covid$entertainment.gym[is.na(covid$entertainment.gym)] <- 0



# for remaining NAs, impute using mean of county's state
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
for (i in which(sapply(covid, is.numeric))) {
  for (j in which(is.na(covid[, i]))) {
    covid[j, i] <- mean(covid[covid[, "StateName"] == covid[j, "StateName"], i],  na.rm = TRUE)
  }
}

# colSums(is.na(covid)) # no more NA

# scale 
covid$FracMale2017 <- covid$FracMale2017 * 100
covid$SVIPercentile <- covid$SVIPercentile * 100
covid$Pop_19below <- covid$Pop_19below * 100
covid$Pop_20_44 <- covid$Pop_20_44 * 100
covid$Pop_45_59 <- covid$Pop_45_59 * 100
covid$Pop_60_74 <- covid$Pop_60_74 * 100
covid$Pop_75plus <- covid$Pop_75plus * 100

############# UNSUPERVISED LEARNING
#### PCA for demographic/health

# split features based on category: health, political, demographics 
feats_health <- c("MedicareEnrollment.AgedTot2017", "DiabetesPercentage", "HeartDiseaseMortality", 
                  "StrokeMortality", "Smokers_Percentage", "RespMortalityRate2014", 
                  "X.FTEHospitalTotal2017", "TotalM.D..s.TotNon.FedandFed2017", 
                  "X.HospParticipatinginNetwork2017", "X.Hospitals", "X.ICU_beds", 
                  "HPSAShortage", "SVIPercentile")
feats_pol <- c("dem_to_rep_ratio", "stay.at.home", "X.50.gatherings", "X.500.gatherings", 
               "public.schools", "restaurant.dine.in", "entertainment.gym")
feats_demo <- c("Rural.UrbanContinuumCode2013", "PopulationEstimate2018", "FracMale2017", 
                "PopulationDensityperSqMile2010", "MedianAge2010", 
                "Pop_19below", "Pop_20_44", "Pop_45_59", "Pop_60_74", "Pop_75plus") # excluded census region


# PCA for demographic/health
health_pc <- princomp(covid[ , c(feats_health, feats_pol, feats_demo)], cor = TRUE, scores = TRUE)

plot(1:30, health_pc$sdev ^ 2, pch = 19, xlab = "Index", ylab = "Eigenvalues", 
     main = "Scree Plot for PCA on County Demographic Variables")

pca_eigen_var <- summary(health_pc)
pca_loadings <- health_pc$loadings

plot(1:30, health_pc$sdev ^ 2, pch = 19, xlab = "Index", ylab = "Eigenvalues", 
     main = "Scree Plot for PCA on County Demographic Variables")


### K-means for covid counts 
covid_month <- covid[, c(which(names(covid) == "X.Cases_03.21.2020"):which(names(covid) == "X.Cases_04.21.2020"),
                         which(names(covid) == "X.Deaths_03.21.2020"):which(names(covid) == "X.Deaths_04.21.2020"))]
covid_month <- apply(covid_month, 2, scale)

set.seed(1)
WSS = NULL
for(K in 2:10){
  kms=kmeans(covid_month,
             centers = K, nstart = 25)
  wssk=sum(kms$withinss)
  WSS=c(WSS,wssk)
}
plot(c(2:10), WSS, xlab = "K", ylab = "WSS(K)", pch = 19)

set.seed(2)
kmeans_6 <- kmeans(covid_month, centers = 6, nstart = 25)
table(kmeans_6$cluster)

#inspect clusters
head(covid[kmeans_6$cluster == 2, c("CountyName", "StateName", "tot_deaths", "tot_cases")])
tail(covid[kmeans_6$cluster == 6, c("CountyName", "StateName", "tot_deaths", "tot_cases")])

covid[kmeans_6$cluster == 4, c("CountyName", "StateName", "tot_deaths", "tot_cases")]


km_results_tab <- matrix(0, 6, 4)
for (i in 1:6) { 
  the_cluster = i
  clust_min = min(covid$tot_deaths[kmeans_6$cluster == i])
  clust_max = max(covid$tot_deaths[kmeans_6$cluster == i])
  clust_mean = mean(covid$tot_deaths[kmeans_6$cluster == i])
  clust_meancase = mean(covid$tot_cases[kmeans_6$cluster == i])
  km_results_tab[i, ] = c(clust_min, clust_max, clust_mean, clust_meancase)
}

km_pca_tab <- matrix(0, 6, 5)
death_pop <- covid$tot_deaths / (covid$PopulationEstimate2018 / 100000)
death_pc_avg <- rep(0, 6)
for (i in 1:6) {
  if (i == 3){
    km_pca_tab[3, ] = health_pc$scores[kmeans_6$cluster == 3, 1:5]
  } else{
    km_pca_tab[i, ] = colMeans(health_pc$scores[kmeans_6$cluster == i, 1:5])
  }
  death_pc_avg[i] = mean(death_pop[kmeans_6$cluster == i])
}

km_pca_tab <- cbind(km_pca_tab, km_results_tab[, 3], death_pc_avg)

set.seed(5)
### hierarchical clustering
dist_mtx <- dist(health_pc$scores[, 1:5])
hcfit <- hclust(dist_mtx, method = "complete")
plot(hcfit, labels = rep(" ", length(covid$CountyName)))

plot(21:1,hcfit$height[(length(hcfit$height)-20):length(hcfit$height)])

hc_5 <- cutree(hcfit, k = 5)
table(hc_5)
covid[hc_5 == 1, c("StateName", "CountyName", "PopulationEstimate2018", "tot_cases", "tot_deaths")]
covid[hc_5 == 2, c("StateName", "CountyName", "PopulationEstimate2018", "tot_cases", "tot_deaths")]
covid[hc_5 == 3, c("StateName", "CountyName", "PopulationEstimate2018", "tot_cases", "tot_deaths")]
covid[hc_5 == 4, c("StateName", "CountyName", "PopulationEstimate2018", "tot_cases", "tot_deaths")]
covid[hc_5 == 5, c("StateName", "CountyName", "PopulationEstimate2018", "tot_cases", "tot_deaths")]

covid_health <- apply(covid[, c(feats_health, feats_pol, feats_demo)], 2, scale)

hc_pca_tab <- matrix(0, 5, 7)
for (i in 1:5){
  hc_pca_tab[i, 1] = sum(hc_5 == i)
  hc_pca_tab[i, 2:6] = colMeans(health_pc$scores[hc_5 == i, 1:5])
  hc_pca_tab[i, 7] = mean(covid$tot_deaths[hc_5 == i])
}
hc_pca_tab
