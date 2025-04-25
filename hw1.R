# This script provide the answer to the HW1 of International Trade Class provided by Thierry Mayer in 2024-2025

library(data.table)
library(fixest)
library(here)
library(modelsummary)

# Data --------------------------------------------------------------------

trade = fread(here("biltrade.csv"))

# Question 1  -------------------------------------------------------------
trade_1 = trade[year == 2016]
trade_1 = trade_1[flow >= 0.0, ]

model_fmp = list("OLS" = feols(log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto| iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
                 "Poisson" = fepois(flow ~ log(distw) + contig + comlang_off + comcur + fta_wto| iso_o + iso_d, cluster = ~iso_o, data = trade_1 )
                 )

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency", 
        "fta_wto" = "FTA/WTO")

add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio)
)

fmp_table = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
  # exponentiate = TRUE
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(fmp_table), "fmp_table.typ")

phi_hat_coef_ols = model_fmp$OLS$coefficients[["log(distw)"]]
phi_hat_coef_pois = model_fmp$Poisson$coefficients[["log(distw)"]]
fmp_fe_ols = fixef(model_fmp$OLS)$iso_d
fmp_fe_pois = fixef(model_fmp$Poisson)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model, 
# and the exponential of fixed effects, given they are dummy.
trade_1[, `:=` (phi_hat_ols = distw^phi_hat_coef_ols,
                fe_imp_ols = fmp_fe_ols[paste(iso_d)],
                phi_hat_pois = distw^phi_hat_coef_pois,
                fe_imp_pois = fmp_fe_pois[paste(iso_d)]
                )
        ]
trade_1[, `:=` (exp_fe_imp_ols = exp(fe_imp_ols), 
                exp_fe_imp_pois = exp(fe_imp_pois))]

# We create the fmp and gdp per capita variables
trade_1[, fmp_ols := sum(phi_hat_ols*exp_fe_imp_ols, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois := sum(phi_hat_pois*exp_fe_imp_pois, na.rm = TRUE), by = iso_o]
trade_1[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

list_model_1 <- list(
  "ln GDP" = list(
  "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), cluster = ~iso_o, data = trade_1),
  "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, cluster = ~iso_o, data = trade_1),
  "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), cluster = ~iso_o, data = trade_1),
  "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, cluster = ~iso_o, data = trade_1))
)

table1 = modelsummary(
  model = list_model_1 ,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = c("log(fmp_ols)" = "ln FMP","log(fmp_pois)" = "ln FMP"),
  output = "typst"
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(table1), con = "table1.typ")

# Question 2 --------------------------------------------------------------

trade_2 = trade[year %in% 2004:2016,]
trade_2 = trade_2[flow >= 0.0, ]

model_fmp_full = list(
  "OLS" = feols(log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto| iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "Poisson" = fepois(flow ~ log(distw) + contig + comlang_off + comcur + fta_wto| iso_o + iso_d, cluster = ~iso_o, data = trade_2 )
)

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency", 
        "fta_wto" = "FTA/WTO")

add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols = c(fitstat(model_fmp_full$OLS, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp_full$Poisson)$dispersion_ratio)
)

fmp_table_full = modelsummary(
  model = model_fmp_full,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
  # exponentiate = TRUE
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(fmp_table_full), "fmp_full_table.typ")

phi_hat_coef_ols = model_fmp_full$OLS$coefficients[["log(distw)"]]
phi_hat_coef_pois = model_fmp_full$Poisson$coefficients[["log(distw)"]]
fmp_fe_ols = fixef(model_fmp_full$OLS)$iso_d
fmp_fe_pois = fixef(model_fmp_full$Poisson)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model, 
# and the exponential of fixed effects, given they are dummy.
trade_2[, `:=` (phi_hat_ols = distw^phi_hat_coef_ols,
                fe_imp_ols = fmp_fe_ols[paste(iso_d)],
                phi_hat_pois = distw^phi_hat_coef_pois,
                fe_imp_pois = fmp_fe_pois[paste(iso_d)]
)
]
trade_2[, `:=` (exp_fe_imp_ols = exp(fe_imp_ols), 
                exp_fe_imp_pois = exp(fe_imp_pois))]

# We create the fmp and gdp per capita variables
trade_2[, fmp_ols := sum(phi_hat_ols*exp_fe_imp_ols, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, fmp_pois := sum(phi_hat_pois*exp_fe_imp_pois, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6))]

list_model_2 <- list(
  "ln GDP" = list(
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), cluster = ~iso_o, data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | year^iso_d, cluster = ~iso_o, data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), cluster = ~iso_o, data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | year^iso_d, cluster = ~iso_o, data = trade_2))
)

table2 = modelsummary(
  model = list_model_2 ,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = c("log(fmp_ols)" = "ln FMP","log(fmp_pois)" = "ln FMP"),
  output = "typst"
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(table2), con = "table2.typ")

# R&V - Draft -------------------------------------------------------------------
trade_1 = trade[year == 2016]
trade_1 = trade_1[flow >= 0.0, ]

model_fmp = list(
  "OLS (Log + 1)" = feols(log(flow + 1) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "OLS" = feols(log(flow) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "Poisson" = fepois(flow ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 )
)

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency",
        "fta_wto" = "FTA/WTO")

add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols_log1 = c(fitstat(model_fmp$`OLS (Log + 1)`, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio)
)

fmp_table_rv = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
  # exponentiate = TRUE
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(fmp_table_rv), "fmp_table_rv.typ")

lambda_1_hat_log1 = model_fmp$`OLS (Log + 1)`$coefficients[["log(distw)"]] 
lambda_1_hat_ols = model_fmp$OLS$coefficients[["log(distw)"]] 
lambda_1_hat_pois = model_fmp$Poisson$coefficients[["log(distw)"]] 

lambda_2_hat_log1 = model_fmp$`OLS (Log + 1)`$coefficients[["contig"]] 
lambda_2_hat_ols = model_fmp$OLS$coefficients[["contig"]] 
lambda_2_hat_pois = model_fmp$Poisson$coefficients[["contig"]] 

fmp_fe_log1 = fixef(model_fmp$`OLS (Log + 1)`)$iso_d
fmp_fe_ols = fixef(model_fmp$OLS)$iso_d
fmp_fe_pois = fixef(model_fmp$Poisson)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model,
# and the exponential of fixed effects, given they are dummy.
trade_1[, `:=` (dist_struct_log1 = distw^lambda_1_hat_log1,
                border_struct_log1 = contig^lambda_2_hat_log1,
                fe_imp_log1 = fmp_fe_log1[paste(iso_d)],
                dist_struct_ols = distw^lambda_1_hat_ols,
                border_struct_ols = contig^lambda_2_hat_ols,
                fe_imp_ols = fmp_fe_ols[paste(iso_d)],
                dist_struct_pois = distw^lambda_1_hat_pois,
                border_struct_pois = contig^lambda_2_hat_pois,
                fe_imp_pois = fmp_fe_pois[paste(iso_d)]
)
]
trade_1[, `:=` (exp_fe_imp_log1 = exp(fe_imp_log1),
                exp_fe_imp_ols = exp(fe_imp_ols),
                exp_fe_imp_pois = exp(fe_imp_pois))]

# We create the fmp and gdp per capita variables
trade_1[, fmp_log1 := sum(dist_struct_log1*border_struct_log1*exp_fe_imp_log1, na.rm = TRUE), by = iso_o]
trade_1[, fmp_ols := sum(dist_struct_ols*border_struct_ols*exp_fe_imp_ols, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois := sum(dist_struct_pois*border_struct_pois*exp_fe_imp_pois, na.rm = TRUE), by = iso_o]
trade_1[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

list_model_1 <- list(
  "ln GDP" = list(
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1), cluster = ~iso_o, data = trade_1),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1) | iso_d, cluster = ~iso_o, data = trade_1), 
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), cluster = ~iso_o, data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, cluster = ~iso_o, data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), cluster = ~iso_o, data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, cluster = ~iso_o, data = trade_1))
)

table1 = modelsummary(
  model = list_model_1,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  # coef_map = c("log(fmp_ols)" = "ln FMP","log(fmp_pois)" = "ln FMP", "ln FMP" = "log(fmp_log1)"),
  # output = "typst"
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)


