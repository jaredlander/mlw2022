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

library(ggplot2)
ggplot(train, aes(x=Status)) + geom_bar()

# need to balance good/bad

ggplot(train, aes(x=Amount, y=Status, color=Job)) + geom_point()

ggplot(train, aes(x=Amount, y=Age, color=Status)) + geom_point()

# Choose Our First Model ####

# penalized regression
# boost tree

# Preprocessing ####

library(recipes)

# outcome: response, label, target, dependent variable (crappy term), y
# input: predictor, covariate, feature, x

train |> count(Home)

recipe(Status ~ ., data=train) |>
    step_factor2string(Home, Job) |>
    step_mutate(Home=if_else(is.na(Home), 'Missing', Home)) |>
    step_mutate(Job=if_else(is.na(Job), 'Missing', Job)) |>
    step_string2factor(Home, Job) |>
    # step_other(Home, other='misc') |>
    step_other(all_nominal_predictors(), other='misc') |>
    # step_dummy(Home, keep_original_cols=FALSE, one_hot=TRUE) |>
    # step_dummy(Job, one_hot=TRUE) |>
    step_dummy(all_nominal_predictors(), one_hot=TRUE) |>
    prep() |>
    bake(new_data=NULL)
