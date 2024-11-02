# -------------
# NOT USED BROR
# -------------

rm(list = ls())
graphics.off()
cat("\014")

setwd("~/Documents/RStudio/TMS150 - Stochastic Data Processing/AssignmentA6")

library(stats)
library(tidyverse)
#library(reshape2)

# Question 3
# Read in the data:
successes <- read_csv("successful.txt", col_names = FALSE)
unsuccesses <- read_csv("unsuccessful.txt", col_names = FALSE)
# Assign correct Z value
success_df <- tibble("x" = successes$X1, success = 1)
unsuccess_df <- tibble(x = unsuccesses$X1, success = 0)
# Combine to one dataframe
data_df <- bind_rows(success_df, unsuccess_df)

# Define ranges for mu and gamma
#mu_multiple <- 10:11
#gamma_multiple <- -2:1


# Define funtion 15
p <- function(x, mu, gamma){
  exp((x-mu)*exp(-gamma))/(1+exp((x-mu)*exp(-gamma)))
}

#for (i in 1:length(mu_multiple)) {
#  for (j in 1:length(gamma_multiple)) {
#    data_df <- data_df %>%
#      mutate(temp = p(x, mu_multiple[i], gamma_multiple[j]))
#    col.name = paste("mu_", mu_multiple[i], "_gamma_", gamma_multiple[j], sep = "")
#    names(data_df)[names(data_df) == "temp"] <- col.name
#  }
#}
#
#data_long <- data_df %>%
#  select(-p_success, -success) %>%
#  melt(id = "x")
#
#ggplot(data_long,
#       aes(x = x, y = value, color = variable))+
#  geom_line()

data_df <- data_df %>%
  mutate(mu_10_gamma_1 = p(x, 10, 1),
         mu_5_gamma_1 = p(x, 5, 1),
         mu_15_gamma_1 = p(x, 15, 1),
         mu_20_gamma_1 = p(x, 20, 1),
         mu_10_gamma_2 = p(x, 10, 2),
         mu_10_gamma_minus_1 = p(x, 15, -1),
         mu_20_gamma_minus_1 = p(x, 20, -1),
         mu_10_gamma_0 = p(x, 10,  0),
         mu_15_gamma_0 = p(x, 15,  0),)

ggplot(data_df, aes(x = x))+
  geom_point(aes(y = success))+
  geom_line(aes(y = mu_5_gamma_1, color = "mu_5_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_1, color = "mu_10_gamma_1"))+
  geom_line(aes(y = mu_15_gamma_1, color = "mu_15_gamma_1"))+
  geom_line(aes(y = mu_20_gamma_1, color = "mu_20_gamma_1"))+
  geom_line(aes(y = mu_10_gamma_2, color = "mu_10_gamma_2"), linetype = "dashed")+
  geom_line(aes(y = mu_10_gamma_minus_1, color = "mu_15_gamma_minus_1"), linetype = "dotdash")+
  geom_line(aes(y = mu_15_gamma_0, color = "mu_15_gamma_0"), linetype = "dotted")+
  labs(title = "Question 3a",
       subtitle = "Solid: gamma = 1, dashed: gamma = 2, dotted: gamma = 0, dotdash: gamma = -1\nLeftmost solid: mu = 5")
ggsave("figures/q3a.png")
# Gamma should be around 1, certainly not 2, not as small as 0
# Mu should be somewhere between 10 and 15, gamma = 5 does not mat probability for the lower x:es, 
# and gamma = 20 does not match the middle ones. 

