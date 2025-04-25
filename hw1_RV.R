library(data.table)
library(fixest)
library(here)
library(modelsummary)

# Data --------------------------------------------------------------------

trade = fread(here("biltrade.csv"))

# Question 1 -------------------------------------------------------------------
trade_1 = trade[year == 2016]
trade_1 = trade_1[flow >= 0.0, ]

model_fmp = list(
  "OLS (Log + 1)" = feols(log(flow + 1) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "OLS (Log + 1) - Aug." = feols(log(flow + 1) ~ log(distw) + contig + comlang_off + comcur + fta_wto
                                   | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "OLS" = feols(log(flow) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "OLS - Aug." = feols(log(flow) ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "Poisson" = fepois(flow ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_1 ),
  "Poisson - Aug." = fepois(flow ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                     | iso_o + iso_d, cluster = ~iso_o, data = trade_1 )
)

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency",
        "fta_wto" = "FTA/WTO")

add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols_log1 = c(fitstat(model_fmp$`OLS (Log + 1)`, type = "f")$f$p, NA),
  fmp_ols_log1_aug = c(fitstat(model_fmp$`OLS (Log + 1) - Aug.`, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$`OLS - Aug.`, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$`Poisson - Aug.`)$dispersion_ratio)
)

fmp_table_rv = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
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

# Augmented Estimation 
lambda_1_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["log(distw)"]] 
lambda_1_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["log(distw)"]] 
lambda_1_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["log(distw)"]] 
lambda_2_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["contig"]] 
lambda_2_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["contig"]] 
lambda_2_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["contig"]] 
lambda_3_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["comlang_off"]] 
lambda_3_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["comlang_off"]] 
lambda_3_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["comlang_off"]] 
lambda_4_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["comcur"]] 
lambda_4_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["comcur"]] 
lambda_4_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["comcur"]] 
lambda_5_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["fta_wto"]] 
lambda_5_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["fta_wto"]] 
lambda_5_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["fta_wto"]] 

fmp_fe_log1_aug = fixef(model_fmp$`OLS (Log + 1) - Aug.`)$iso_d
fmp_fe_ols_aug = fixef(model_fmp$`OLS - Aug.`)$iso_d
fmp_fe_pois_aug = fixef(model_fmp$`Poisson - Aug.`)$iso_d

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

# Augmented
trade_1[, `:=` (dist_struct_log1_aug = distw^lambda_1_hat_log1_aug,
                border_struct_log1_aug = contig^lambda_2_hat_log1_aug,
                comm_lang_log1_aug = comlang_off^lambda_3_hat_log1_aug,
                comcur_log1_aug = comcur^lambda_4_hat_log1_aug,
                fta_wto_log1_aug = fta_wto^lambda_5_hat_log1_aug,
                fe_imp_log1_aug = fmp_fe_log1_aug[paste(iso_d)],
                
                dist_struct_ols_aug = distw^lambda_1_hat_ols_aug,
                border_struct_ols_aug = contig^lambda_2_hat_ols_aug,
                comm_lang_ols_aug = comlang_off^lambda_3_hat_ols_aug,
                comcur_ols_aug = comcur^lambda_4_hat_ols_aug,
                fta_wto_ols_aug = fta_wto^lambda_5_hat_ols_aug,
                fe_imp_ols_aug = fmp_fe_ols_aug[paste(iso_d)],
                
                dist_struct_pois_aug = distw^lambda_1_hat_pois_aug,
                border_struct_pois_aug = contig^lambda_2_hat_pois_aug,
                comm_lang_pois_aug = comlang_off^lambda_3_hat_pois_aug,
                comcur_pois_aug = comcur^lambda_4_hat_pois_aug,
                fta_wto_pois_aug = fta_wto^lambda_5_hat_pois_aug,
                fe_imp_pois_aug = fmp_fe_pois[paste(iso_d)]
                )
        ]
trade_1[, `:=` (exp_fe_imp_log1_aug = exp(fe_imp_log1_aug),
                exp_fe_imp_ols_aug = exp(fe_imp_ols_aug),
                exp_fe_imp_pois_aug = exp(fe_imp_pois_aug))]


# We create the fmp and gdp per capita variables
trade_1[, fmp_log1 := sum(dist_struct_log1*border_struct_log1*exp_fe_imp_log1, na.rm = TRUE), by = iso_o]
trade_1[, fmp_ols := sum(dist_struct_ols*border_struct_ols*exp_fe_imp_ols, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois := sum(dist_struct_pois*border_struct_pois*exp_fe_imp_pois, na.rm = TRUE), by = iso_o]
trade_1[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

# Augmented FMP
trade_1[, fmp_log1_aug := sum(dist_struct_log1_aug*border_struct_log1_aug*comm_lang_log1_aug
                              *comcur_log1_aug*fta_wto_log1_aug*exp_fe_imp_log1_aug, na.rm = TRUE), by = iso_o]
trade_1[, fmp_ols_aug := sum(dist_struct_ols_aug*border_struct_ols_aug*comm_lang_ols_aug
                             *comcur_ols_aug*fta_wto_ols_aug*exp_fe_imp_log1_aug, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois_aug := sum(dist_struct_pois_aug*border_struct_pois_aug*comm_lang_pois_aug
                              *comcur_pois_aug*fta_wto_pois_aug*exp_fe_imp_pois_aug, na.rm = TRUE), by = iso_o]

list_model_1 <- list(
  "ln GDP" = list(
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1), cluster = ~iso_o, data = trade_1),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1) | iso_d, cluster = ~iso_o, data = trade_1),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1_aug) | iso_d, cluster = ~iso_o, data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), cluster = ~iso_o, data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, cluster = ~iso_o, data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols_aug) | iso_d, cluster = ~iso_o, data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), cluster = ~iso_o, data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, cluster = ~iso_o, data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois_aug) | iso_d, cluster = ~iso_o, data = trade_1))
)

map =  c(
  "log(fmp_log1)" = "ln FMP",
  "log(fmp_ols)" = "ln FMP",
  "log(fmp_pois)" = "ln FMP", 
  "log(fmp_log1_aug)" = "ln FMP (Aug.)",
  "log(fmp_ols_aug)" = "ln FMP (Aug.)",
  "log(fmp_pois_aug)" = "ln FMP (Aug.)"
  )

table1 = modelsummary(
  model = list_model_1,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = map,
  output = "typst"
)

writeLines(as.character(table1), "table1_rv.typ")

# Question 2  -------------------------------------------------------------
trade_2 = trade[flow >= 0.0, ]

model_fmp = list(
  "OLS (Log + 1)" = feols(log(flow + 1) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "OLS (Log + 1) - Aug." = feols(log(flow + 1) ~ log(distw) + contig + comlang_off + comcur + fta_wto
                                 | iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "OLS" = feols(log(flow) ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "OLS - Aug." = feols(log(flow) ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                       | iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "Poisson" = fepois(flow ~ log(distw) + contig | iso_o + iso_d, cluster = ~iso_o, data = trade_2 ),
  "Poisson - Aug." = fepois(flow ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                            | iso_o + iso_d, cluster = ~iso_o, data = trade_2 )
)

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency",
        "fta_wto" = "FTA/WTO")

add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols_log1 = c(fitstat(model_fmp$`OLS (Log + 1)`, type = "f")$f$p, NA),
  fmp_ols_log1_aug = c(fitstat(model_fmp$`OLS (Log + 1) - Aug.`, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$`OLS - Aug.`, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$`Poisson - Aug.`)$dispersion_ratio)
)

fmp_table_rv = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
)

writeLines(as.character(fmp_table_rv), "fmp_table_rv_full_sample.typ")

lambda_1_hat_log1 = model_fmp$`OLS (Log + 1)`$coefficients[["log(distw)"]] 
lambda_1_hat_ols = model_fmp$OLS$coefficients[["log(distw)"]] 
lambda_1_hat_pois = model_fmp$Poisson$coefficients[["log(distw)"]] 

lambda_2_hat_log1 = model_fmp$`OLS (Log + 1)`$coefficients[["contig"]] 
lambda_2_hat_ols = model_fmp$OLS$coefficients[["contig"]] 
lambda_2_hat_pois = model_fmp$Poisson$coefficients[["contig"]] 

fmp_fe_log1 = fixef(model_fmp$`OLS (Log + 1)`)$iso_d
fmp_fe_ols = fixef(model_fmp$OLS)$iso_d
fmp_fe_pois = fixef(model_fmp$Poisson)$iso_d

# Augmented Estimation 
lambda_1_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["log(distw)"]] 
lambda_1_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["log(distw)"]] 
lambda_1_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["log(distw)"]] 
lambda_2_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["contig"]] 
lambda_2_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["contig"]] 
lambda_2_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["contig"]] 
lambda_3_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["comlang_off"]] 
lambda_3_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["comlang_off"]] 
lambda_3_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["comlang_off"]] 
lambda_4_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["comcur"]] 
lambda_4_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["comcur"]] 
lambda_4_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["comcur"]] 
lambda_5_hat_log1_aug = model_fmp$`OLS (Log + 1) - Aug.`$coefficients[["fta_wto"]] 
lambda_5_hat_ols_aug = model_fmp$`OLS - Aug.`$coefficients[["fta_wto"]] 
lambda_5_hat_pois_aug = model_fmp$`Poisson - Aug.`$coefficients[["fta_wto"]] 

fmp_fe_log1_aug = fixef(model_fmp$`OLS (Log + 1) - Aug.`)$iso_d
fmp_fe_ols_aug = fixef(model_fmp$`OLS - Aug.`)$iso_d
fmp_fe_pois_aug = fixef(model_fmp$`Poisson - Aug.`)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model,
# and the exponential of fixed effects, given they are dummy.
trade_2[, `:=` (dist_struct_log1 = distw^lambda_1_hat_log1,
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
trade_2[, `:=` (exp_fe_imp_log1 = exp(fe_imp_log1),
                exp_fe_imp_ols = exp(fe_imp_ols),
                exp_fe_imp_pois = exp(fe_imp_pois))]

# Augmented
trade_2[, `:=` (dist_struct_log1_aug = distw^lambda_1_hat_log1_aug,
                border_struct_log1_aug = contig^lambda_2_hat_log1_aug,
                comm_lang_log1_aug = comlang_off^lambda_3_hat_log1_aug,
                comcur_log1_aug = comcur^lambda_4_hat_log1_aug,
                fta_wto_log1_aug = fta_wto^lambda_5_hat_log1_aug,
                fe_imp_log1_aug = fmp_fe_log1_aug[paste(iso_d)],
                
                dist_struct_ols_aug = distw^lambda_1_hat_ols_aug,
                border_struct_ols_aug = contig^lambda_2_hat_ols_aug,
                comm_lang_ols_aug = comlang_off^lambda_3_hat_ols_aug,
                comcur_ols_aug = comcur^lambda_4_hat_ols_aug,
                fta_wto_ols_aug = fta_wto^lambda_5_hat_ols_aug,
                fe_imp_ols_aug = fmp_fe_ols_aug[paste(iso_d)],
                
                dist_struct_pois_aug = distw^lambda_1_hat_pois_aug,
                border_struct_pois_aug = contig^lambda_2_hat_pois_aug,
                comm_lang_pois_aug = comlang_off^lambda_3_hat_pois_aug,
                comcur_pois_aug = comcur^lambda_4_hat_pois_aug,
                fta_wto_pois_aug = fta_wto^lambda_5_hat_pois_aug,
                fe_imp_pois_aug = fmp_fe_pois[paste(iso_d)]
)
]
trade_2[, `:=` (exp_fe_imp_log1_aug = exp(fe_imp_log1_aug),
                exp_fe_imp_ols_aug = exp(fe_imp_ols_aug),
                exp_fe_imp_pois_aug = exp(fe_imp_pois_aug))]


# We create the fmp and gdp per capita variables
trade_2[, fmp_log1 := sum(dist_struct_log1*border_struct_log1*exp_fe_imp_log1, na.rm = TRUE), by = .(year,iso_o)]
trade_2[, fmp_ols := sum(dist_struct_ols*border_struct_ols*exp_fe_imp_ols, na.rm = TRUE), by = .(year,iso_o)]
trade_2[, fmp_pois := sum(dist_struct_pois*border_struct_pois*exp_fe_imp_pois, na.rm = TRUE), by = .(year,iso_o)]
trade_2[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

# Augmented FMP
trade_2[, fmp_log1_aug := sum(dist_struct_log1_aug*border_struct_log1_aug*comm_lang_log1_aug
                              *comcur_log1_aug*fta_wto_log1_aug*exp_fe_imp_log1_aug, na.rm = TRUE), by = .(year,iso_o)]
trade_2[, fmp_ols_aug := sum(dist_struct_ols_aug*border_struct_ols_aug*comm_lang_ols_aug
                             *comcur_ols_aug*fta_wto_ols_aug*exp_fe_imp_log1_aug, na.rm = TRUE), by = .(year,iso_o)]
trade_2[, fmp_pois_aug := sum(dist_struct_pois_aug*border_struct_pois_aug*comm_lang_pois_aug
                              *comcur_pois_aug*fta_wto_pois_aug*exp_fe_imp_pois_aug, na.rm = TRUE), by = .(year,iso_o)]

list_model_1 <- list(
  "ln GDP" = list(
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1), cluster = ~iso_o, data = trade_2),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1) | iso_d, cluster = ~iso_o, data = trade_2),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1_aug) | iso_d, cluster = ~iso_o, data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), cluster = ~iso_o, data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, cluster = ~iso_o, data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols_aug) | iso_d, cluster = ~iso_o, data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), cluster = ~iso_o, data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, cluster = ~iso_o, data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois_aug) | iso_d, cluster = ~iso_o, data = trade_2))
)

map =  c(
  "log(fmp_log1)" = "ln FMP",
  "log(fmp_ols)" = "ln FMP",
  "log(fmp_pois)" = "ln FMP", 
  "log(fmp_log1_aug)" = "ln FMP (Aug.)",
  "log(fmp_ols_aug)" = "ln FMP (Aug.)",
  "log(fmp_pois_aug)" = "ln FMP (Aug.)"
)

table1 = modelsummary(
  model = list_model_1,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = map,
  output = "typst"
)

writeLines(as.character(table1), "table2_rv.typ")