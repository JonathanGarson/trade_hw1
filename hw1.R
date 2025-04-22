# This script provide the answer to the HW1 of International Trade Class provided by Thierry Mayer in 2024-2025

library(data.table)
library(fixest)
library(here)
library(modelsummary)

# Data --------------------------------------------------------------------

trade = fread(here("biltrade.csv"))

# Question 1  -------------------------------------------------------------
trade_1 = trade[year == 2016]

fmp = feols(log(flow) ~ log(distw) + contig + comlang_off + comcur + fta_wto| iso_o + iso_d, cluster = ~iso_o, data = trade_1 )

coef =c("log(distw)" = "ln Distance",
        "contig" = "Border",
        "comlang_off" = "Common Language",
        "comcur" = "Common Currency", 
        "fta_wto" = "FTA/WTO")

add = data.frame(
  term = "F-test", 
  p_value = fitstat(fmp, type = "f")$f$p
)

fmp_table = modelsummary(
  model = fmp,
  star = TRUE,
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = coef,
  add_row = add,
  output = "typst",
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(fmp_table), "fmp_table.typ")

phi_hat = fmp$coefficients[["log(distw)"]]
fmp_fe = fixef(fmp)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model, 
# and the exponential of fixed effects, given they are dummy.
trade_1[, `:=` (phi_hat = distw^phi_hat,
                fe_imp = fmp_fe[paste(iso_d)])
        ]
trade_1[, exp_fe_imp := exp(fe_imp)]

# We create the fmp and gdp per capita variables
trade_1[, fmp := sum(phi_hat*exp_fe_imp, na.rm = TRUE), by = iso_o]
trade_1[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

list_model_1 =  list(
  "ln GDP" = list(feols(log(gdp_o_cap) ~ log(fmp) , cluster = ~iso_o, data = trade_1)),
  "ln GDP"=  list(feols(log(gdp_o_cap) ~ log(fmp) | iso_d, cluster = ~iso_o, data = trade_1)))

table1 = modelsummary(
  model = list_model_1 ,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = c("log(fmp)" = "ln FMP"),
  output = "typst",
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(table1), con = "table1.typ")

# Question 2 --------------------------------------------------------------

trade_2 = trade[year %in% 2004:2016,]

fmp_full = feols(log(flow) ~ log(distw) | iso_o + iso_d, data = trade_2)

phi_hat = fmp_full$coefficients[["log(distw)"]]
fmp_fe = fixef(fmp_full)$iso_d

# We build our new column, we take the power of our coefficient phi_hat since distance is a continuous variable in our model, 
# and the exponential of fixed effects, given they are dummy.
trade_2[, `:=` (phi_hat = distw^phi_hat,
                fe_imp = fmp_fe[paste(iso_d)])
]
trade_2[, exp_fe_imp := exp(fe_imp)]

# We create the fmp and gdp per capita variables
trade_2[, fmp := sum(phi_hat*exp_fe_imp, na.rm = TRUE), by = .(iso_o, year)]
trade_2[, `:=` (gdp_o_cap = gdp_o/(pop_o*1e6), gdp_d_cap = gdp_d/(pop_d*1e6)) ]

list_model_2 =  list(
  "ln GDP" = list(feols(log(gdp_o_cap) ~ log(fmp) , cluster = ~iso_o, data = trade_2)),
  "ln GDP"=  list(feols(log(gdp_o_cap) ~ log(fmp) | year^iso_d, cluster = ~iso_o, data = trade_2)))

table2 = modelsummary(
  model = list_model_2 ,
  star = TRUE,
  shape = "cbind",
  gof_omit = c("AIC|BIC|RMSE|Std.|Within"),
  coef_map = c("log(fmp)" = "ln FMP"),
  output = "typst",
  # notes = "The standard are displayed between parenthesis and clustered at the exporter country level."
)

writeLines(as.character(table2), con = "table2.typ")


