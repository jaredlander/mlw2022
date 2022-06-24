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

train |>
    summarize(across(where(is.factor), ~sum(is.na(.x))))

recipe(Status ~ ., data=train) |>
    themis::step_downsample(Status) |>
    step_nzv(all_predictors()) |>
    # step_impute_knn(Income) |>
    # step_normalize(all_numeric_predictors()) |>
    step_factor2string(Home, Job, Marital) |>
    step_mutate(Home=if_else(is.na(Home), 'Missing', Home)) |>
    step_mutate(Job=if_else(is.na(Job), 'Missing', Job)) |>
    step_mutate(Marital=if_else(is.na(Marital), 'Missing', Marital)) |>
    step_string2factor(Home, Job, Marital) |>
    # step_other(Home, other='misc') |>
    step_other(all_nominal_predictors(), other='misc') |>
    # step_dummy(Home, keep_original_cols=FALSE, one_hot=TRUE) |>
    # step_dummy(Job, one_hot=TRUE) |>
    step_novel(all_nominal_predictors(), new_level='unseen') |>
    step_dummy(all_nominal_predictors(), one_hot=TRUE) |>
    prep() |>
    bake(new_data=NULL)


rec1 <- recipe(Status ~ ., data=train) |>
    themis::step_downsample(Status) |>
    step_nzv(all_predictors()) |>
    step_factor2string(Home, Job, Marital) |>
    step_mutate(Home=if_else(is.na(Home), 'Missing', Home)) |>
    step_mutate(Job=if_else(is.na(Job), 'Missing', Job)) |>
    step_mutate(Marital=if_else(is.na(Marital), 'Missing', Marital)) |>
    step_string2factor(Home, Job, Marital) |>
    step_other(all_nominal_predictors(), other='misc') |>
    step_novel(all_nominal_predictors(), new_level='unseen') |>
    step_dummy(all_nominal_predictors(), one_hot=TRUE)
rec1

# Define the Model ####

library(parsnip)

boost_tree(mode='classification') |> set_engine('xgboost')
boost_tree(mode='classification') |> set_engine('C5.0')
boost_tree(mode='classification') |> set_engine('spark')
linear_reg() |> set_engine('glmnet')
rand_forest() |> set_engine('ranger')




# Hastie, Tibshirani and Friedman
# Hastie, Tibshirani, Whitten, Garrett


spec1 <- boost_tree(mode='classification', trees=100, tree_depth=4) |>
    set_engine('xgboost')
spec1

# Put Them Together ####

library(workflows)

flow1 <- workflow(preprocessor=rec1, spec=spec1)
flow1

# Train Our Model ####

fit1 <- fit(flow1, data=train)
fit1

fake_new
predict(fit1, new_data=fake_new)
predict(fit1, new_data=fake_new, type='prob')

# How Good is Our Model? ####

# log loss

library(yardstick)
metric_set1 <- metric_set(mn_log_loss, roc_auc)

mn_log_loss_vec(factor(c(TRUE), levels=c(TRUE, FALSE)), c(0.37))
mn_log_loss_vec(factor(c(FALSE), levels=c(TRUE, FALSE)), c(0.37))
mn_log_loss_vec(factor(c(FALSE), levels=c(TRUE, FALSE)), c(0.03))

# Cross Validation ####

cv1 <- vfold_cv(train, v=5, strata='Status')
cv1
cv1$splits[[1]]

library(tune)
val1 <- fit_resamples(flow1, resamples=cv1, metrics=metric_set1)
val1
val1$.metrics[[1]]
val1$.metrics[[2]]

val1 |> collect_metrics()

# Parameter Tuning ####

spec2 <- boost_tree(mode='classification', trees=500, tree_depth=5) |>
    set_engine('xgboost')
spec3 <- boost_tree(mode='classification', trees=600, tree_depth=10) |>
    set_engine('xgboost')

spec2 <- boost_tree(
    mode='classification', trees=tune(), tree_depth=tune()
) |>
    set_engine('xgboost')
spec2
spec1

flow2 <- flow1 |> update_model(spec2)
flow2

flow2 |>
    parameters()
flow2 |>
    hardhat::extract_parameter_set_dials()

library(dials)
params2 <- flow2 |>
    hardhat::extract_parameter_set_dials() |>
    update(
        trees=trees(c(50, 500)),
        tree_depth=tree_depth(c(2, 8))
    )
params2$object

450*7
grid2 <- grid_random(params2, size=100)
grid2

options(tidymodels.dark = TRUE)

tune2 <- tune_grid(
    flow2,
    resamples=cv1,
    grid=grid2,
    metrics=metric_set1,
    control=control_grid(verbose=TRUE)
)
tune2
tune2$.metrics[[1]] |>
    arrange(trees, tree_depth)
tune2 |> collect_metrics()

tune2 |> autoplot()
tune2 |> autoplot(metric='roc_auc')

tune2 |> show_best(n=5, metric='roc_auc')

tune2 |> select_best(metric='roc_auc')
tune2 |> select_best(metric='mn_log_loss')

tune2 |> select_by_one_std_err(metric='roc_auc', tree_depth)

best_params_2 <- tune2 |> select_best(metric='roc_auc')
best_params_2

mod2 <- flow2 |> finalize_workflow(parameters=best_params_2)
mod2


fit2 <- fit(mod2, data=train)
predict(fit2, new_data=fake_new, type='prob')

final2 <- last_fit(mod2, split=credit_split)
final2
final2 |> collect_metrics()
