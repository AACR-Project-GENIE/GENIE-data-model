smoke_clin <- read_rds(
  here('data', 'smoke', 'smoke_clin.rds')
)

# x <- select(
#   smoke_clin,
#   -c(record_id, ca_seq, contains("ca_cig"))
# )
#
# y <- select(
#   smoke_clin,
#   ca_cig_ever
# ) %>%
#   mutate(
#     ca_cig_ever = as.factor(ca_cig_ever)
#   )

# just the stuff we aspire to get in main GENIE.
smoke_clin_mg <- smoke_clin %>%
  select(
    ca_cig_ever,
    stage_dx,
    dob_ca_dx_yrs,
    institution,
    birth_year,
    naaccr_ethnicity_code,
    naaccr_race_code_primary,
    naaccr_sex_code
  ) %>%
  fastDummies::dummy_cols(
    remove_most_frequent_dummy = T,
    remove_selected_columns = T,
    select_columns = c(
      'stage_dx',
      'institution',
      'naaccr_ethnicity_code',
      'naaccr_race_code_primary',
      'naaccr_sex_code'
    )
  ) %>%
  mutate(ca_cig_ever = factor(ca_cig_ever))

library(parsnip)
library(recipes)
library(workflows)
library(rsample)

# Create a recipe with imputation
rec <- recipe(ca_cig_ever ~ ., data = smoke_clin_mg) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors())

# Create a decision tree model
xgboost_mod <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Update workflow
tree_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgboost_mod)


tree_fit <- tree_wf %>%
  fit(data = smoke_clin_mg)


# Calculate predictions and probability scores
results <- bind_cols(
  truth = smoke_clin_mg$ca_cig_ever,
  predict(tree_fit, new_data = smoke_clin_mg, type = "prob")
)


# Create ROC curve
library(pROC)
roc_obj <- roc(results$truth, results$.pred_TRUE)

# Plot ROC curve with ggplot2
library(ggplot2)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(size = 1, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  coord_equal() +
  labs(
    title = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"),
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal()


# Load required libraries
library(yardstick)
library(vip)

# Make predictions on the training data
predictions <- predict(tree_fit, new_data = smoke_clin_mg)
pred_probs <- predict(tree_fit, new_data = smoke_clin_mg, type = "prob")

# Combine actual outcomes with predictions
results <- bind_cols(
  truth = smoke_clin_mg$ca_cig_ever,
  predictions,
  pred_probs
)


# Create confusion matrix
conf_mat(results, truth = truth, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

# Plot variable importance
vip(tree_fit$fit$fit) +
  labs(title = "Variable Importance in XGBoost Model")
metrics <- metric_set(accuracy, precision, recall, f_meas, roc_auc)
model_metrics <- metrics(
  results,
  truth = truth,
  estimate = .pred_class,
  .pred_TRUE
)

# Print metrics
print(model_metrics)

# Create confusion matrix
conf_mat(results, truth = truth, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

# Plot variable importance
vip(tree_fit$fit$fit) +
  labs(title = "Variable Importance in XGBoost Model")
