<<<<<<< HEAD
library(data.table)
library(fixest)
library(here)
library(lmtest)
library(ggplot2)
library(modelsummary)

# Data --------------------------------------------------------------------

trade = fread(here("biltrade.csv"))

# Question 1 -------------------------------------------------------------------
trade_1 = trade[year == 2016]
trade_1 = trade_1[flow >= 0.0, ]

model_fmp = list(
  "OLS (Log + 1)" = feols(log(flow + 1) ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_1 ),
  "OLS (Log + 1) - Aug." = feols(log(flow + 1) ~ log(distw) + contig + comlang_off + comcur + fta_wto
                                   | iso_o + iso_d, vcov = "hetero", data = trade_1 ),
  "OLS" = feols(log(flow) ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_1 ),
  "OLS - Aug." = feols(log(flow) ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                | iso_o + iso_d, vcov = "hetero", data = trade_1 ),
  "Poisson" = fepois(flow ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_1 ),
  "Poisson - Aug." = fepois(flow ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                     | iso_o + iso_d, vcov = "hetero", data = trade_1 )
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
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1), vcov = "hetero", data = trade_1),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1) | iso_d, vcov = hetero, data = trade_1),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1_aug) | iso_d, vcov = hetero, data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), vcov = "hetero", data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, vcov = "hetero", data = trade_1),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols_aug) | iso_d, vcov = "hetero", data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), vcov = "hetero", data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, vcov = "hetero", data = trade_1),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois_aug) | iso_d, vcov = "hetero", data = trade_1))
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
  "OLS (Log + 1)" = feols(log(flow + 1) ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_2 ),
  "OLS (Log + 1) - Aug." = feols(log(flow + 1) ~ log(distw) + contig + comlang_off + comcur + fta_wto
                                 | iso_o + iso_d, vcov = "hetero", data = trade_2 ),
  "OLS" = feols(log(flow) ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_2 ),
  "OLS - Aug." = feols(log(flow) ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                       | iso_o + iso_d, vcov = "hetero", data = trade_2 ),
  "Poisson" = fepois(flow ~ log(distw) + contig | iso_o + iso_d, vcov = "hetero", data = trade_2 ),
  "Poisson - Aug." = fepois(flow ~ log(distw) + contig  + comlang_off + comcur + fta_wto
                            | iso_o + iso_d, vcov = "hetero", data = trade_2 )
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
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1), vcov = "hetero", data = trade_2),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1) | iso_d, vcov = "hetero", data = trade_2),
    "OLS (Log + 1)" = feols(log(gdp_o_cap) ~ log(fmp_log1_aug) | iso_d, vcov = "hetero", data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols), vcov = "hetero", data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols) | iso_d, vcov = "hetero", data = trade_2),
    "OLS" = feols(log(gdp_o_cap) ~ log(fmp_ols_aug) | iso_d, vcov = "hetero", data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois), vcov = "hetero", data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois) | iso_d, vcov = "hetero", data = trade_2),
    "Poisson" = fepois(log(gdp_o_cap) ~ log(fmp_pois_aug) | iso_d, vcov = "hetero", data = trade_2))
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

# Heteroskedasticity ------------------------------------------------------
# We compute the heteroskedasticity of the OLS regression

ols_1 = list_model_1$`ln GDP`$OLS
data = data.frame(
  fv = fitted(ols_1),
  rv = resid(ols_1))

ggplot(data, aes(x = fv, y =rv))+
  geom_point(alpha = 0.2) + 
  labs(
    x = "Fitted Values",
    y = "Residual Values"
  ) +
  theme_classic()
ggsave("heteroskedasticity.pdf", width = 10, height = 8)

  
bp_df <- data.frame(rv2 = rv^2, fv = fv)
bptest(rv2 ~ fv, data = bp_df)

bp_test = function(model){
  data = data.frame(
    fv = fitted(model),
    rv = resid(model))
  bp_df <- data.frame(rv2 = rv^2, fv = fv)
  test = bptest(rv2 ~ fv, data = bp_df)
  return(test)
}
=======
################################################################################
######################## INTERNATIONAL TRADE, 2025 - HM1 #######################
###################### Garson, Jonathan & Maestri, Andrea ######################

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
#### This script provide the answer to the HW1 of International Trade Class ####
#################### provided by Thierry Mayer in 2024-2025 ####################
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

################################################################################

##### 0. IMPORTING LIBRARIES AND DATA #####

# Libraries --------------------------------------------------------------------

library(data.table)
library(fixest)
library(here)
library(modelsummary)
library(ggplot2)
library(patchwork)

# Data -------------------------------------------------------------------------
projectdir = here()
setwd(projectdir)
trade = fread("data/biltrade.csv")

# Question 1 -------------------------------------------------------------------

##### 1. ESTIMATING PARAMETERS FOR FMP COMPUTATION #####

# Filter data for 2016 only, removing zero flows
trade_1 = trade[year == 2016]
trade_1 = trade_1[flow >= 0.0, ]

# Regressing exports on relevant variables with several specifications
# OLS (log), OLS, Poisson - Augmented version includes more parameters
# to estimate trade costs than in Redding and Venables (2004)
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

# Label map
coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency",
        "fta_wto" = "FTA/WTO")

# Regression stats
add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols_log1 = c(fitstat(model_fmp$`OLS (Log + 1)`, type = "f")$f$p, NA),
  fmp_ols_log1_aug = c(fitstat(model_fmp$`OLS (Log + 1) - Aug.`, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$`OLS - Aug.`, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$`Poisson - Aug.`)$dispersion_ratio)
)

# Print out regression results
fmp_table_rv = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
)

writeLines(as.character(fmp_table_rv), "output/fmp_table_rv.typ")

# Extract parameters
# Standard parameters - as in Redding and Venables (2004)
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


##### 2. COMPUTING FMP AND GDP PER CAPITA #####

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model,
# and the exponential of fixed effects, given they are dummy.
trade_1[, `:=` (dist_struct_log1 = lambda_1_hat_log1*log(distw),
                border_struct_log1 = lambda_2_hat_log1*contig,
                fe_imp_log1 = fmp_fe_log1[paste(iso_d)],
                dist_struct_ols = lambda_1_hat_ols*log(distw),
                border_struct_ols = lambda_2_hat_ols*contig,
                fe_imp_ols = fmp_fe_ols[paste(iso_d)],
                dist_struct_pois = lambda_1_hat_pois*log(distw),
                border_struct_pois = lambda_2_hat_pois*contig,
                fe_imp_pois = fmp_fe_pois[paste(iso_d)]
                )
        ]

# Compute country i's market access in country j
trade_1[, `:=` (mpj_log1 = exp(fe_imp_log1 + dist_struct_log1 + border_struct_log1),
                mpj_ols = exp(fe_imp_ols + dist_struct_ols + border_struct_ols),
                mpj_pois = exp(fe_imp_pois + dist_struct_pois + border_struct_pois))]

# We create the FMP and GDP per capita variables
trade_1[, fmp_log1 := sum(mpj_log1, na.rm = TRUE), by = iso_o]
trade_1[, fmp_ols := sum(mpj_ols, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois := sum(mpj_pois, na.rm = TRUE), by = iso_o]
trade_1[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

# Augmented FMP
trade_1[, `:=` (dist_struct_log1_aug = log(distw)*lambda_1_hat_log1_aug,
                border_struct_log1_aug = contig*lambda_2_hat_log1_aug,
                comm_lang_log1_aug = comlang_off*lambda_3_hat_log1_aug,
                comcur_log1_aug = comcur*lambda_4_hat_log1_aug,
                fta_wto_log1_aug = fta_wto*lambda_5_hat_log1_aug,
                fe_imp_log1_aug = fmp_fe_log1_aug[paste(iso_d)],
                
                dist_struct_ols_aug = log(distw)*lambda_1_hat_ols_aug,
                border_struct_ols_aug = contig*lambda_2_hat_ols_aug,
                comm_lang_ols_aug = comlang_off*lambda_3_hat_ols_aug,
                comcur_ols_aug = comcur*lambda_4_hat_ols_aug,
                fta_wto_ols_aug = fta_wto*lambda_5_hat_ols_aug,
                fe_imp_ols_aug = fmp_fe_ols_aug[paste(iso_d)],
                
                dist_struct_pois_aug = log(distw)*lambda_1_hat_pois_aug,
                border_struct_pois_aug = contig*lambda_2_hat_pois_aug,
                comm_lang_pois_aug = comlang_off*lambda_3_hat_pois_aug,
                comcur_pois_aug = comcur*lambda_4_hat_pois_aug,
                fta_wto_pois_aug = fta_wto*lambda_5_hat_pois_aug,
                fe_imp_pois_aug = fmp_fe_pois[paste(iso_d)]
)
]


# Compute country i's market access in country j
trade_1[, `:=` (mpj_log1_aug = exp(fe_imp_log1_aug + dist_struct_log1_aug + border_struct_log1 +
                                     comm_lang_log1_aug + comcur_log1_aug + fta_wto_log1_aug),
                mpj_ols_aug = exp(fe_imp_ols_aug + dist_struct_ols_aug + border_struct_ols +
                                comm_lang_ols_aug + comcur_ols_aug + fta_wto_ols_aug),
                mpj_pois_aug = exp(fe_imp_pois_aug + dist_struct_pois_aug + border_struct_pois +
                                 comm_lang_pois_aug + comcur_pois_aug + fta_wto_pois_aug))]

# We create the FMP and GDP per capita variables
trade_1[, fmp_log1_aug := sum(mpj_log1_aug, na.rm = TRUE), by = iso_o]
trade_1[, fmp_ols_aug := sum(mpj_ols_aug, na.rm = TRUE), by = iso_o]
trade_1[, fmp_pois_aug := sum(mpj_pois_aug, na.rm = TRUE), by = iso_o]


##### 3. REGRESS GDP PER CAPITA ON FMP #####

# Estimate regression models 
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

# Print regression results
table1 = modelsummary(
  model = list_model_1,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = map,
  output = "typst"
)

writeLines(as.character(table1), "output/table1_rv.typ")

##### 4. REPEAT ESTIMATING PARAMETERS ON MULTIPLE YEARS #####

# Question 2  -------------------------------------------------------------
# Here we keep all years!
trade_2 = trade[flow >= 0.0, ]


# Regressing exports on relevant variables with several specifications
# OLS (log), OLS, Poisson - Augmented version includes more parameters
# to estimate trade costs than in Redding and Venables (2004)
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

# Label map
coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency",
        "fta_wto" = "FTA/WTO")

# Regression stats
add <- data.frame(
  term = c("F-test", "Overdispersion"),
  fmp_ols_log1 = c(fitstat(model_fmp$`OLS (Log + 1)`, type = "f")$f$p, NA),
  fmp_ols_log1_aug = c(fitstat(model_fmp$`OLS (Log + 1) - Aug.`, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$OLS, type = "f")$f$p, NA),
  fmp_ols = c(fitstat(model_fmp$`OLS - Aug.`, type = "f")$f$p, NA),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$Poisson)$dispersion_ratio),
  fmp_pois = c(NA, performance::check_overdispersion(model_fmp$`Poisson - Aug.`)$dispersion_ratio)
)

# Print out regression results
fmp_table_rv = modelsummary(
  model = model_fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst"
)

writeLines(as.character(fmp_table_rv), "output/fmp_table_rv_full_sample.typ")

# Extract parameters
# Standard parameters - as in Redding and Venables (2004)
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


##### 2. COMPUTING FMP AND GDP PER CAPITA #####

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model,
# and the exponential of fixed effects, given they are dummy.
trade_2[, `:=` (dist_struct_log1 = lambda_1_hat_log1*log(distw),
                border_struct_log1 = lambda_2_hat_log1*contig,
                fe_imp_log1 = fmp_fe_log1[paste(iso_d)],
                dist_struct_ols = lambda_1_hat_ols*log(distw),
                border_struct_ols = lambda_2_hat_ols*contig,
                fe_imp_ols = fmp_fe_ols[paste(iso_d)],
                dist_struct_pois = lambda_1_hat_pois*log(distw),
                border_struct_pois = lambda_2_hat_pois*contig,
                fe_imp_pois = fmp_fe_pois[paste(iso_d)]
)
]

# Compute country i's market access in country j
trade_2[, `:=` (mpj_log1 = exp(fe_imp_log1 + dist_struct_log1 + border_struct_log1),
                mpj_ols = exp(fe_imp_ols + dist_struct_ols + border_struct_ols),
                mpj_pois = exp(fe_imp_pois + dist_struct_pois + border_struct_pois))]

# We create the FMP and GDP per capita variables
trade_2[, fmp_log1 := sum(mpj_log1, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, fmp_ols := sum(mpj_ols, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, fmp_pois := sum(mpj_pois, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

# Augmented FMP
trade_2[, `:=` (dist_struct_log1_aug = log(distw)*lambda_1_hat_log1_aug,
                border_struct_log1_aug = contig*lambda_2_hat_log1_aug,
                comm_lang_log1_aug = comlang_off*lambda_3_hat_log1_aug,
                comcur_log1_aug = comcur*lambda_4_hat_log1_aug,
                fta_wto_log1_aug = fta_wto*lambda_5_hat_log1_aug,
                fe_imp_log1_aug = fmp_fe_log1_aug[paste(iso_d)],
                
                dist_struct_ols_aug = log(distw)*lambda_1_hat_ols_aug,
                border_struct_ols_aug = contig*lambda_2_hat_ols_aug,
                comm_lang_ols_aug = comlang_off*lambda_3_hat_ols_aug,
                comcur_ols_aug = comcur*lambda_4_hat_ols_aug,
                fta_wto_ols_aug = fta_wto*lambda_5_hat_ols_aug,
                fe_imp_ols_aug = fmp_fe_ols_aug[paste(iso_d)],
                
                dist_struct_pois_aug = log(distw)*lambda_1_hat_pois_aug,
                border_struct_pois_aug = contig*lambda_2_hat_pois_aug,
                comm_lang_pois_aug = comlang_off*lambda_3_hat_pois_aug,
                comcur_pois_aug = comcur*lambda_4_hat_pois_aug,
                fta_wto_pois_aug = fta_wto*lambda_5_hat_pois_aug,
                fe_imp_pois_aug = fmp_fe_pois[paste(iso_d)]
)
]


# Compute country i's market access in country j
trade_2[, `:=` (mpj_log1_aug = exp(fe_imp_log1_aug + dist_struct_log1_aug + border_struct_log1 +
                                     comm_lang_log1_aug + comcur_log1_aug + fta_wto_log1_aug),
                mpj_ols_aug = exp(fe_imp_ols_aug + dist_struct_ols_aug + border_struct_ols +
                                    comm_lang_ols_aug + comcur_ols_aug + fta_wto_ols_aug),
                mpj_pois_aug = exp(fe_imp_pois_aug + dist_struct_pois_aug + border_struct_pois +
                                     comm_lang_pois_aug + comcur_pois_aug + fta_wto_pois_aug))]

# Augmented FMP variables
trade_2[, fmp_log1_aug := sum(mpj_log1_aug, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, fmp_ols_aug := sum(mpj_ols_aug, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, fmp_pois_aug := sum(mpj_pois_aug, na.rm = TRUE), by = .(iso_o, year)]

# Regressing log GDP per capita on log FMP
list_model_2 <- list(
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

writeLines(as.character(table1), "output/table2_rv.typ")


##### 5. PLOT GDP VS FMP #####
# Extract unique data for 2016 
plot_data = unique(trade_1[, .(iso_o, gdp_o_cap, fmp_log1, fmp_ols, fmp_pois)])
plot_data = plot_data[!is.na(gdp_o_cap)]

# Filter to remove zero-FMP observations
plot_data = plot_data[!((fmp_log1 < 0.00001) | (fmp_ols < 0.00001) | (fmp_pois < 0.00001))]


#Adjust theme
common_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# FMP OLS Log
fmp_log1_plot <- ggplot(data = plot_data, 
                       aes(x = log(fmp_log1), y = log(gdp_o_cap), 
                           label = iso_o)) +
  geom_text(size = 3) +
  theme_minimal(base_size = 14) +
  labs(x = "ln FMP (Log OLS)", y = "ln GDP per Capita") +
  common_theme

# FMP OLS Log
fmp_ols_plot <- ggplot(data = plot_data, 
       aes(x = log(fmp_ols), y = log(gdp_o_cap), 
           label = iso_o)) +
  geom_text(size = 3) +
  theme_minimal(base_size = 14) +
  labs(x = "ln FMP (OLS)", y = "ln GDP per Capita") +
  common_theme

# FMP Poisson
fmp_pois_plot <- ggplot(data = plot_data, 
                       aes(x = log(fmp_pois), y = log(gdp_o_cap), 
                           label = iso_o)) +
  geom_text(size = 3) +
  theme_minimal(base_size = 14) +
  labs(x = "ln FMP (Poisson)", y = "ln GDP per Capita") +
  common_theme


# Combine plots vertically
final_plot <- (fmp_log1_plot + 
                 fmp_ols_plot + 
                 fmp_pois_plot) +
  plot_layout(ncol = 1) +  # Vertical stacking
  plot_annotation(title = "GDP per capita vs FMP - Selected Estimations for FMP",
                  theme = theme(plot.title = element_text(hjust = 0.5)))

# Show the combined plot
final_plot

# Save
<<<<<<< HEAD:hw1_RV.R
plot_save_path = here("gdpvsfmp.png")
ggsave(plot_save_path, final_plot, width = 8.5, height = 11, units = "in")
>>>>>>> 671719ed89e0bcb8946d52679d3e8ed47844d035
=======
ggsave("output/gdpvsfmp.png", final_plot, width = 8.5, height = 11, units = "in")
>>>>>>> origin/main:src/hw1_RV.R
