# Getting Data ####

data(credit_data, package='modeldata')
head(credit_data)

credit <- credit_data |> tibble::as_tibble()
credit

R.version

library(dplyr)

# make pretend data so we can simulate scoring a new person
fake_new <- credit |> slice_sample(n=10) |> select(-Status)
fake_new

# Train/Test ####

library(rsample)
credit_split <- initial_split(credit, prop=0.8, strata='Status')
credit_split

train <- training(credit_split)
test <- testing(credit_split)

train

# EDA ####
