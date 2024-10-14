# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(MASS)



### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MNL",
  modelDescr ="Simple MNL model on mode choice SP data",
  indivID    ="ID"
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
### if data is to be loaded from a file (e.g. called data.csv), 
### the code would be: database = read.csv("data.csv",header=TRUE)
database = read.csv("D:/Thesis/RP/New/Final Data/Main_Don'tchangeanything_BustoBusMRTT.csv")
### for data dictionary, use ?apollo_modeChoiceData

summary(database)

# Standardize selected variables
variables_to_standardize <- c("personal_income", "family_income", "trans_expn_3months")

# Standardize the variables
database[variables_to_standardize] <- scale(database[variables_to_standardize])

#database$sex <- factor(database$sex)

# Install and load the fastDummies package if not already installed
#install.packages("fastDummies")
library(fastDummies)

# Create dummy variables, removing the first dummy column
# Create dummy variables, removing the first dummy column
database <- dummy_cols(database, select_columns = c("age_3", "education_3", "main_distance_3","personal_income_3", "family_income_3"), remove_first_dummy = TRUE)




summary(database)



sum(is.na(database$column))


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc_MRT                 = 0,
  asc_bus                 = 0,
  #b_tt                     = 0,
  b_tt_MRT                = 0,
  b_tt_bus                = 0,
  b_Fare              = 0,
  #b_Fare_MRT              = 0,
  #b_Fare_bus              = 0,
  b_FlexibilityandConv    = 0,
  b_Family_inclination    = 0,
  #b_Pedestrian_Infrastruc = 0,
  b_Reliability           = 0,
  b_Safety                = 0,
  b_Inflation             = 0,
  b_WalkingBenefits       = 0,
  #b_SocialMedia           = 0,
  #b_Society               = 0,
  b_main_distance         = 0,
  #b_main_distance_2         =0,
  #b_main_distance_3         =0,
  b_Percofactivity        = 0,
  b_age_30to45            = 0,
  b_age_45above            = 0,
  b_male                   = 0,
  b_Rapidpass             = 0,
  b_education_BSC         = 0,
  b_education_AboveBsc    = 0,
  #b_Accesibility_MRT     =0,
  #b_Transit_Frequency     =0,
  b_H_size                = 0,
  #b_EarningFM             = 0,
  b_AvailablePV_yes_No    = 0,
  #b_Personal_Income       = 0,
  b_Personal_Income_2         = 0,
  b_Personal_Income_3         = 0,
  b_Family_Income_2         = 0,
  b_Family_Income_3         = 0,
  #b_ChangeinTransExpend   = 0,
  b_TRANS_expn_3months    = 0,
  b_Distance_500m         = 0,
  b_Distance_1500m        = 0
  ,b_Scheduled_Flexibility =0
  ,b_access_distance       =0
   ,b_Access_egress_Fare    =0
  #b_Time_save             =0
)
  
#b_WalkingInfrastructure = 0,
#b_Walking_benefits      = 0,

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_bus")

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["bus"]] = asc_bus  + 
    b_tt_bus * total_time_bus +
    b_Fare * (total_fare_bus/log(total_distance))
    
  
  V[["MRT"]] = asc_MRT  + 
    b_tt_MRT * total_time_MRT+ 
    b_Fare* (total_fare_MRT/log(total_distance)) +
    b_age_30to45* age_3_2 +
    b_age_45above* age_3_3+
    b_male * sex + 
    b_Rapidpass * rapidpass + 
    b_education_BSC * education_3_2 + 
    b_education_AboveBsc * education_3_3 + 
    #b_Accesibility_MRT*Accesibility_MRT+
    #b_Transit_Frequency*Transit_Frequency+
    b_H_size * household_size + 
    #b_EarningFM * earning_fm + 
    b_AvailablePV_yes_No * AvailablePV + 
    #b_Personal_Income * personal_income +
    b_Personal_Income_2 * personal_income_3_2 +
    b_Personal_Income_3 * personal_income_3_3 +
    #b_Family_income * family_income + 
    b_Family_Income_2 * family_income_3_2 +
    b_Family_Income_3 * family_income_3_3 +
    #b_ChangeinTransExpend * changeintrans_expend + 
    b_TRANS_expn_3months * trans_expn_3months + 
    b_Distance_500m * distance_500m + 
    b_Distance_1500m * distance_1500m + 
    b_FlexibilityandConv*Flexibility_and_Convenience+
    b_Family_inclination*Family_Inclined_People+
    #b_Pedestrian_Infrastruc*Pedestrian_Infrastructure+
    b_Reliability*Reliability+
    b_Safety*Safety+
    b_Inflation*Inflation+
    b_WalkingBenefits*Walking_Benefits+
    #b_SocialMedia*Social_Media+
    #b_Society*Society+
    b_access_distance * access_distance + 
    b_main_distance * total_distance+
    #b_main_distance_2 * main_distance_3_2+
    #b_main_distance_3* main_distance_3_3+
    b_Percofactivity * percofactivity+
    b_Scheduled_Flexibility*Scheduled_Flexibility+
    b_Access_egress_Fare*Access_egress_Fare
     #b_Time_save*Time_save
  
#b_WalkingInfrastructure * walking_infrastructure + 
#b_Walking_benefits * walking_benefits +   
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(MRT=2, bus=1), 
    #avail         = list(bus=av_bus, MRT=av_MRT), 
    choiceVar     = Y,
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  

  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs,
                        list(writeIter=FALSE))


# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
summary(model)



# Define a function to add significance stars based on p-values
add_significance_stars <- function(pvalues) {
  stars <- rep("", length(pvalues))
  stars[pvalues < 0.001] <- "***"
  stars[pvalues < 0.01 & pvalues >= 0.001] <- "**"
  stars[pvalues < 0.05 & pvalues >= 0.01] <- "*"
  stars[pvalues < 0.1 & pvalues >= 0.05] <- "."
  return(stars)
}

# Update the apollo_modelOutput function to include significance stars
apollo_modelOutput_with_stars <- function(model) {
  output <- summary(model)$coefficients
  pvalues <- output[, "Pr(>|z|)"]
  stars <- add_significance_stars(pvalues)
  
  output_with_stars <- cbind(output, stars)
  colnames(output_with_stars)[ncol(output_with_stars)] <- "Significance"
  
  print(output_with_stars)
}

# Call the updated function to display the model output with significance stars
apollo_modelOutput(model)



print(summary)



