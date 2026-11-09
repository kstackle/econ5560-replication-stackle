# ==============================================================================
# Master Pipeline Script: run_all.R
# Replication: Landowner Conservation Incentive Programs
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup Directories
# ------------------------------------------------------------------------------
dirs <- c("outputs", "logs")
for (d in dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}

cat("=== Starting Pipeline Execution at:", as.character(Sys.time()), "===\n\n")

# ------------------------------------------------------------------------------
# 1. Package Installation & Loading
# ------------------------------------------------------------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "tibble", "knitr", 
  "kableExtra", "mvtnorm", "ggplot2", "sandwich", "lmtest"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

invisible(lapply(required_packages, library, character.only = TRUE))
cat("Packages successfully loaded.\n\n")

# ------------------------------------------------------------------------------
# 2. Data Ingestion & Validation
# ------------------------------------------------------------------------------
data_path <- "decoded_data.xls"
if (!file.exists(data_path)) {
  stop(paste("Raw data file", data_path, "not found. Ensure it is placed in the project root directory."))
}

cat("Reading raw data from:", data_path, "...\n")
raw <- readxl::read_excel(data_path)
cat("Raw dataset dimensions:", nrow(raw), "rows,", ncol(raw), "columns.\n\n")

# Variable Renaming
raw_renamed <- raw |>
  rename(
    age = age_mid,
    income = income_mid,
    years_owned = Yearsowned,
    reside_on_land = resideonland,
    forest_landowner = forestlo3,
    hire_assistance = hireassistance,
    land_investment = Forlandinvestment,
    plant_replant_5yr = Plantoreplant5yr,
    never_applied_costshare = Neverappliedtoacostsharepr,
    mountain = Mountain
  )

model_vars <- c(
  "age", "income", "years_owned", "reside_on_land",
  "forest_landowner", "hire_assistance", "land_investment",
  "plant_replant_5yr", "never_applied_costshare", "mountain"
)

# Individual program subsets
forest_data <- raw_renamed |>
  filter(PWSIFORWOMB %in% c(0, 1)) |>
  select(participation = PWSIFORWOMB, all_of(model_vars)) |>
  drop_na()

farm_data <- raw_renamed |>
  filter(PWSIfarmWOMB %in% c(0, 1)) |>
  select(participation = PWSIfarmWOMB, all_of(model_vars)) |>
  drop_na()

nbs_data <- raw_renamed |>
  filter(PWSINBSWOMB %in% c(0, 1)) |>
  select(participation = PWSINBSWOMB, all_of(model_vars)) |>
  drop_na()

# ------------------------------------------------------------------------------
# 3. Table 1 & Sample Size Comparison
# ------------------------------------------------------------------------------
cat("Generating sample size comparisons and Table 1 checklist...\n")

paper_n <- c(Forestry = 146, Farm = 155, NBS = 153)
comparison_table <- tibble(
  Model = c("Forestry", "Farm", "NBS"),
  Paper_Cases = c(paper_n["Forestry"], paper_n["Farm"], paper_n["NBS"]),
  Replicated_Cases = c(nrow(forest_data), nrow(farm_data), nrow(nbs_data)),
  Difference = Paper_Cases - Replicated_Cases
)

write.csv(comparison_table, "outputs/table_sample_size_comparison.csv", row.names = FALSE)

table1_variable_check <- tibble(
  Variable = c(
    "Forestry participation: PWSIFOR (PWSIFORWOMB)",
    "Farm participation: PWSIFARM (PWSIfarmWOMB)",
    "NBS participation: PWSINBS (PWSINBSWOMB)",
    "Age (age_mid)",
    "Income (income_mid)",
    "Years owned (Yearsowned)",
    "Resides on land (resideonland)",
    "Forest landowner (forestlo3)",
    "Hires assistance (hireassistance)",
    "Land for investment (Forlandinvestment)",
    "Plant/replant within 5 years (Plantoreplant5yr)",
    "Never applied for cost-share (Neverappliedtoacostsharepr)",
    "Mountain region (Mountain)"
  ),
  Listed_in_Paper_Table_1 = "Yes",
  Available_in_Decoded_Data = "Yes"
)

write.csv(table1_variable_check, "outputs/table1_variable_checklist.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. Table 2: Summary Demographics Replication
# ------------------------------------------------------------------------------
cat("Computing Table 2 demographic statistics...\n")

table2_data <- raw_renamed |>
  filter(groupLO %in% c(1, 2)) |>
  mutate(group = if_else(groupLO == 1, "Forest", "Farm"))

group_totals <- table2_data |> count(group, name = "total_n")

income_ours <- table2_data |>
  filter(HHincome %in% 1:6) |>
  mutate(
    variable = "Income",
    category = case_when(
      HHincome == 1 ~ "Less than $24,999",
      HHincome == 2 ~ "$25,000-$49,999",
      HHincome == 3 ~ "$50,000-$74,999",
      HHincome == 4 ~ "$75,000-$99,999",
      HHincome == 5 ~ "$100,000-$149,999",
      HHincome == 6 ~ "$150,000 or more"
    )
  ) |>
  count(variable, category, group, name = "our_n") |>
  left_join(group_totals, by = "group") |>
  mutate(our_percent = round(100 * our_n / total_n, 1))

age_ours <- table2_data |>
  filter(!is.na(age)) |>
  mutate(
    variable = "Age",
    category = case_when(
      age == 24 ~ "18-30",
      age == 38 ~ "30-45",
      age == 53 ~ "46-60",
      age == 68 ~ "61-75",
      age == 80 ~ "75+"
    )
  ) |>
  count(variable, category, group, name = "our_n") |>
  left_join(group_totals, by = "group") |>
  mutate(our_percent = round(100 * our_n / total_n, 1))

education_ours <- table2_data |>
  filter(!is.na(Educationlevel)) |>
  mutate(
    variable = "Education",
    category = case_when(
      grepl("high|ged", Educationlevel, ignore.case = TRUE) ~ "High school or GED",
      grepl("some", Educationlevel, ignore.case = TRUE) ~ "Some college degree",
      grepl("associate", Educationlevel, ignore.case = TRUE) ~ "Associate's degree",
      grepl("bachelor", Educationlevel, ignore.case = TRUE) ~ "Bachelor's degree",
      grepl("advanced", Educationlevel, ignore.case = TRUE) ~ "Advanced degree"
    )
  ) |>
  count(variable, category, group, name = "our_n") |>
  left_join(group_totals, by = "group") |>
  mutate(our_percent = round(100 * our_n / total_n, 1))

our_table2 <- bind_rows(income_ours, age_ours, education_ours)

paper_table2 <- tibble(
  variable = c(
    rep("Gender", 3),
    rep("Income", 6),
    rep("Age", 5),
    rep("Education", 5)
  ),
  category = c(
    "Female", "Male", "Other",
    "Less than $24,999", "$25,000-$49,999", "$50,000-$74,999",
    "$75,000-$99,999", "$100,000-$149,999", "$150,000 or more",
    "18-30", "30-45", "46-60", "61-75", "75+",
    "High school or GED", "Some college degree", "Associate's degree",
    "Bachelor's degree", "Advanced degree"
  ),
  paper_forest_percent = c(
    13.0, 81.7, 5.3,
    0.8, 19.1, 14.5, 15.3, 19.1, 23.7,
    0.8, 2.3, 24.4, 50.4, 21.4,
    9.2, 21.4, 12.2, 26.0, 27.5
  ),
  paper_farm_percent = c(
    30.4, 69.6, 0.0,
    4.3, 7.2, 14.5, 24.6, 15.9, 26.1,
    0.0, 4.3, 21.7, 50.7, 23.2,
    15.9, 18.8, 15.9, 18.8, 29.0
  )
)

our_wide <- our_table2 |>
  select(variable, category, group, our_n, our_percent) |>
  pivot_wider(names_from = group, values_from = c(our_n, our_percent))

table2_comparison <- paper_table2 |>
  left_join(our_wide, by = c("variable", "category")) |>
  rename(
    our_forest_n = our_n_Forest,
    our_farm_n = our_n_Farm,
    our_forest_percent = our_percent_Forest,
    our_farm_percent = our_percent_Farm
  ) |>
  mutate(
    forest_diff = round(our_forest_percent - paper_forest_percent, 1),
    farm_diff = round(our_farm_percent - paper_farm_percent, 1)
  )

write.csv(table2_comparison, "outputs/table2_summary_comparison.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Joint Logistic Model Estimation
# ------------------------------------------------------------------------------
cat("Estimating joint multivariate logistic regression model...\n")

forest_eq <- PWSIFORWOMB ~
  age_mid + HHincome + Yearsowned + resideonland +
  forestlo3 + hireassistance + Forlandinvestment +
  Plantoreplant5yr + Neverappliedtoacostsharepr + Mountain

outcomes <- c("PWSIFORWOMB", "PWSIfarmWOMB", "PWSINBSWOMB")
predictors <- all.vars(forest_eq)[-1]

predictors_ok <- rowSums(is.na(raw[, predictors, drop = FALSE])) == 0
has_outcome <- rowSums(!is.na(raw[, outcomes, drop = FALSE])) > 0

model_data <- raw[predictors_ok & has_outcome, unique(c(predictors, outcomes)), drop = FALSE]
Y <- as.matrix(model_data[, outcomes])

X_raw <- model.matrix(delete.response(terms(forest_eq)), data = model_data)
if (qr(X_raw)$rank < ncol(X_raw)) {
  stop("Predictor matrix is not full rank.")
}

centers <- colMeans(X_raw[, -1, drop = FALSE])
scales  <- apply(X_raw[, -1, drop = FALSE], 2, sd)

X <- X_raw
X[, -1] <- scale(X_raw[, -1, drop = FALSE], center = centers, scale = scales)
k <- ncol(X)

make_R <- function(a) {
  r12 <- tanh(a[1])
  r13 <- tanh(a[2])
  partial23 <- tanh(a[3])
  r23 <- r12 * r13 + partial23 * sqrt((1 - r12^2) * (1 - r13^2))
  matrix(c(1, r12, r13, r12, 1, r23, r13, r23, 1), nrow = 3, byrow = TRUE)
}

neg_loglik <- function(parameters) {
  B <- matrix(parameters[seq_len(3 * k)], nrow = k)
  R <- make_R(parameters[3 * k + seq_len(3)])
  probabilities <- plogis(X %*% B)
  probabilities <- pmin(pmax(probabilities, 1e-12), 1 - 1e-12)
  
  log_likelihood <- numeric(nrow(Y))
  for (i in seq_len(nrow(Y))) {
    observed <- which(!is.na(Y[i, ]))
    y <- Y[i, observed]
    p <- probabilities[i, observed]
    
    if (length(observed) == 1) {
      log_likelihood[i] <- dbinom(y, size = 1, prob = p, log = TRUE)
    } else {
      signs <- ifelse(y == 1, 1, -1)
      upper <- signs * qnorm(p)
      R_observed <- R[observed, observed, drop = FALSE]
      R_signed <- R_observed * outer(signs, signs)
      joint_prob <- as.numeric(pmvnorm(lower = rep(-Inf, length(observed)), upper = upper, corr = R_signed, algorithm = TVPACK()))
      if (!is.finite(joint_prob) || joint_prob <= 0) return(1e100)
      log_likelihood[i] <- log(joint_prob)
    }
  }
  -sum(log_likelihood)
}

B_start <- sapply(seq_len(3), function(j) {
  obs <- !is.na(Y[, j])
  glm.fit(x = X[obs, , drop = FALSE], y = Y[obs, j], family = binomial(link = "logit"))$coefficients
})

start <- c(as.vector(B_start), 0, 0, 0)

joint_fit <- optim(
  par = start,
  fn = neg_loglik,
  method = "L-BFGS-B",
  lower = c(rep(-Inf, 3 * k), rep(-3, 3)),
  upper = c(rep( Inf, 3 * k), rep( 3, 3)),
  control = list(maxit = 1000)
)

B_scaled <- matrix(joint_fit$par[seq_len(3 * k)], nrow = k)
B_original <- B_scaled
B_original[-1, ] <- sweep(B_scaled[-1, , drop = FALSE], 1, scales, "/")
B_original[1, ] <- B_scaled[1, ] - colSums(sweep(B_original[-1, , drop = FALSE], 1, centers, "*"))
rownames(B_original) <- colnames(X_raw)
colnames(B_original) <- c("Forestry", "Farm", "NBS")

model_odds_ratios <- round(exp(B_original), 3)
cat("Estimated Model Odds Ratios:\n")
print(model_odds_ratios)

write.csv(as.data.frame(model_odds_ratios), "outputs/table4_model_odds_ratios.csv")

# ------------------------------------------------------------------------------
# 6. Regional Landowner Distribution Figure
# ------------------------------------------------------------------------------
cat("Creating regional landowner figure...\n")

plot_data <- raw %>%
  select(Region, forestlo3, farmlo3) %>%
  pivot_longer(
    cols = c(forestlo3, farmlo3),
    names_to = "landowner_type",
    values_to = "is_landowner"
  ) %>%
  filter(is_landowner == 1) %>%
  mutate(
    Region = recode(
      Region,
      Mountain = "Mountains",
      NorthCoastal = "North Coastal Plain",
      SouthCoastal = "South Coastal Plain"
    ),
    landowner_type = recode(
      landowner_type,
      forestlo3 = "Forest Landowners",
      farmlo3 = "Farm Landowners"
    )
  ) %>%
  count(Region, landowner_type, name = "count")

p <- ggplot(plot_data, aes(x = landowner_type, y = count, fill = landowner_type)) +
  geom_col(width = 0.85) +
  geom_text(aes(label = count), color = "white", vjust = 1.8, size = 4) +
  facet_wrap(~ Region) +
  scale_fill_manual(values = c("Forest Landowners" = "#417F00", "Farm Landowners" = "#D35400")) +
  labs(x = NULL, y = "Number of respondents", title = "Landowner Distribution by Region") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave("outputs/figure_landowner_distribution.png", plot = p, width = 8, height = 5, dpi = 300)

cat("\n=== Pipeline Execution Completed Successfully at:", as.character(Sys.time()), "===\n\n")

# ------------------------------------------------------------------------------
# 7. Environment Information (Required by Step 4, Item 4)
# ------------------------------------------------------------------------------
print(sessionInfo())