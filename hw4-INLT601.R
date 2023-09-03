#required packages 
library(tidyverse)
library(lubridate)

# read the dataset 
df = read_csv("C:/Users/Topcu/Desktop/cses_imd.csv")

#rename columns from the codebook and select only the required ones
df <- df %>%
  rename(
    country = "IMD1006_NAM",
    year_of_election = "IMD1008_YEAR",
    election_type = "IMD1009",
    age = "IMD2001_1",
    gender = "IMD2002",
    education = "IMD2003",
    religiosity = "IMD2005_2",
    household_income = "IMD2006",
    rural_urban = "IMD2007",
    race = "IMD2010",
    ethnicity = "IMD2011",
    n_household = "IMD2012_1",
    children_household = "IMD2012_2",
    employment = "IMD2014",
    socio_economic = "IMD2016",
    union_membership = "IMD2019_1",
    union_membership_household = "IMD2019_2",
    spouse_employment = "IMD2020",
    spouse_socio_economic = "IMD2022",
    business_association = "IMD2025",
    farmers_association = "IMD2026",
    professional_association = "IMD2027",
    ideology = "IMD3006",
    efficacy_incumbent = "IMD3011",
    efficacy_people = "IMD3012",
    turnout_main_election = "IMD3001",
    economy_state_year = "IMD3013_1",
    political_information = "IMD3015_A",
    voting_procedure = "IMD5017_1",
    election_schedule_month = "IMD5024_1",
    election_violence = "IMD5032_1",
    election_day_irregularities = "IMD5034_3",
    age_of_current_regime = "IMD5049",
    polity = "IMD5051_2",
    gdp_growth = "IMD5052_2",
    gdp_per_cap = "IMD5053_2",
    unemployment = "IMD5054_2",
    human_development_index = "IMD5055_2",
    inflation = "IMD5056_2"
  ) %>%
  select(
    country,
    year_of_election,
    election_type,
    age,
    gender,
    education,
    religiosity,
    household_income,
    rural_urban,
    race,
    ethnicity,
    n_household,
    children_household,
    employment,
    socio_economic,
    union_membership,
    union_membership_household,
    spouse_employment,
    spouse_socio_economic,
    business_association,
    farmers_association,
    professional_association,
    ideology,
    efficacy_incumbent,
    efficacy_people,
    turnout_main_election,
    economy_state_year,
    political_information,
    voting_procedure,
    election_schedule_month,
    election_violence,
    election_day_irregularities,
    age_of_current_regime,
    polity,
    gdp_growth,
    gdp_per_cap,
    unemployment,
    human_development_index,
    inflation
  )


#changing codebook nan values to nan 
df$year[df$ideology>=95]<-NA
df$ideology[df$ideology>=95]<-NA
df$education[df$education>=6]<-NA
df$turnout_main_election[df$turnout_main_election>=9999993]<-NA
polityreplace=c(-66,-77,-88,99)
df$polity[df$polity %in% polityreplace]<-NA
df$age[df$age>=9997]<-NA
df$`socio_economic`[df$`socio_economic` >= 5] <- NA
df$gender[df$gender>=3]<-NA
df$religiosity[df$religiosity>=7]<-NA
df$rural_urban[df$rural_urban>=7]<-NA 
df$voting_procedure[df$voting_procedure>=7]<-NA
df$inflation[df$inflation>= 99999.0]<-NA
df$political_information[df$political_information>= 9]<-NA
df$economy_state_year[df$economy_state_year>= 9]<-NA
df$gdp_per_cap[df$gdp_per_cap>= 999999.00]<-NA
df$age_of_current_regime[df$age_of_current_regime>= 999]<-NA

#this can go on and on and on.... to accommodate all the chosen variables but given our time constraints, we chose the following 
----------------------------------------------------------------
#Logistic Regression (we have a binary outcome)

mod=glm(turnout_main_election ~ inflation*ideology + 
          education + 
          polity+
          age + 
          voting_procedure + 
          gdp_per_cap + 
          religiosity + 
          unemployment*ideology + 
          political_information + 
          age_of_current_regime,
        family = binomial, data=df,na.action = na.omit)
summary(mod)
#the intercept is significant given that even without the effects of independent variables, 
#its highly possible there are other factors which explain vote turnout

#inflation * ideology: this represents both the main effects of inflation and ideology, as well as their interaction term. 
#It tries to capture how the effect of inflation on voter turnout changes with 
#varying levels of ideological self-placement.
#in this case, deviance of residuals are the minimum and maximum values are -2.9627 and 1.7144 respectively,
#which could be indicative of model fit.
#our model model explains a considerable amount of variability in voter turnout.
#several predictors are highly significant, although unemployment and ideology do not seem to interact in affecting voter turnout.
#we may want to look into the missing data and run additional diagnostics to validate the model's assumptions and fit.

deviance_resid <- resid(mod, type = "deviance")
summary(deviance_resid)
hist(deviance_resid, breaks=30, main="Histogram of Deviance Residuals")
plot(predict(mod, type="response"), deviance_resid, xlab="Fitted values", ylab="Deviance residuals")

---------------------------------------------------
#Fixed Effects 

install.packages("plm")
library(plm)
fixed <- plm(turnout_main_election ~ unemployment + ideology + education + 
               polity + age + voting_procedure + gdp_per_cap + inflation + 
               political_information + age_of_current_regime + religiosity,
             data = df, 
             model = "within")  # "within" specifies fixed effects
summary(fixed)

#One Way Fixed Effect (time)

fixed <- glm(turnout_main_election ~ unemployment * ideology + education + 
               polity + age + voting_procedure + gdp_per_cap + inflation + 
               political_information + age_of_current_regime + religiosity + as.factor(year_of_election),
             family = binomial, 
             data = df, 
             na.action = na.omit)
summary(fixed)

#in order to concentrate on the estimates of the other variables (such as unemployment, education, etc.), 
#we would need to adjust for time-invariant unobserved features for each unit of analysis (individuals, states, or years, depending on our data). 
#this will provide us unbiased estimates of the important variables, 
#which is especially helpful if you think that there are unobserved factors that influence voter turnout 
#and are connected with our independent variables.
#Numerous factors, including age, education, politics, and unemployment, have statistical significance.
---------------------------------------------------------------------------------

#Two way fixed effect ( unit + time) 

fixed2=glm(turnout_main_election ~ unemployment* ideology + education + polity+
            age + voting_procedure + gdp_per_cap + inflation + political_information + age_of_current_regime + 
            religiosity+as.factor(country)+as.factor(year_of_election),
          family = binomial, data=df,na.action = na.omit)
summary(fixed2)


#most predictors are significant and suggest practical implications—for example, 
#a rise in unemployment seems to deter voter turnout. 
--------------------------------------------------------------------------------

#Random Effects Model 


library(lme4)
random <- glmer(turnout_main_election ~ unemployment*ideology + education + polity +
                  age + voting_procedure + gdp_per_cap + inflation + 
                  political_information + age_of_current_regime + religiosity + (1|country) + (1|year_of_election), 
                family = binomial, data=df, na.action = na.omit)
summary(random)

---------------------------------------------------------------------------------
#Mixed Effects Model 

mixed <- glmer(turnout_main_election ~ unemployment*ideology + education + polity +
                 age + voting_procedure + gdp_per_cap + inflation + 
                 political_information + age_of_current_regime + religiosity + (1|country) + (1|year_of_election), 
               family = binomial, data=df, na.action = na.omit)
summary(mixed)
-------------------------------------------------------------------------------

#Standart errors  
library(sandwich)
install.packages("multiwayvcov")
library(multiwayvcov)
library(lmtest)
vcovCL1 <- cluster.vcov(mod, ~as.factor(df$country) + as.factor(df$year_of_election))
coeftest(mod, vcovCL1)

---------------------------------------------------------------------------------
#what's next? 

#null model benchmarking: Assess the added value of predictors over a null model.
#categorical variables: Include dummies for categories like gender and socio-economic status.
#model selection: Employ a random effects model, justified via a Hausman test.
#alternative economic indicators: Test models using inflation and GDP growth as additional explanatory variables.
