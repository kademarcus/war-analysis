rm(list=ls())
#installs
#install.packages("dplyr")
#install.packages("gt")
#install.packages("haven")

#libraries
library(dplyr)
library(gt)
library(haven)

#Data Reading
leader_data <- read_dta("stata_data.dta")
leader_data <- leader_data %>%
  mutate(cinc = if_else(cinc < 0, 0, cinc))
print(mean(leader_data$cinc,na.rm = TRUE))

#predictive model
initiation_logit <- glm(initiation ~ age + milnoncombat + combat + rebel + warwin +
                          warloss + rebelwin + rebelloss + aut + cinc + tau_lead +
                          officetenure1000 + fiveyearchallengelag,
                        family = "binomial", 
                        data = leader_data) 

summary(initiation_logit)

#predicted probabilities
leader_data <- na.omit(War_Data)
predicted_probabilities <- predict(initiation_logit, type = "response", na.action = na.exclude())
leader_data$predicted_probabilities <- predicted_probabilities

#Israel and Arab Leaders only
leader_data <- leader_data%>%
  filter(ccode == 666|
           ccode == 651|
           ccode == 663|
           ccode == 645|
           ccode == 660|
           ccode == 652) %>%
  filter(year == 1948)
View(leader_data)

#export clean data
write_csv(leader_data, file = "leader_data.csv")

#table for probabilities
leader_predictions <- War_Data %>%
  select(leadername, predicted_probabilities) %>%
  distinct() %>%   # ensure one row per leader
  arrange(desc(predicted_probabilities))  

leader_predictions %>%
  gt() %>%
  fmt_number(columns = predicted_probabilities, decimals = 3) %>%
  tab_header(title = "Predicted Probabilities for Israeli/Arab Leaders in 1948")
