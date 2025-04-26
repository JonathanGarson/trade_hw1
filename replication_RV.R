# This code replicates exactly Redding & Venables (2004)

library(data.table)
library(fixest)

# Data --------------------------------------------------------------------

trade = fread(here("biltrade.csv"))

# Transform Variables --------------------------------------------------------------------

trade[, totalexp := sum(flow), by = .(year, iso_o)]
trade[, ln1_totalexp := log(totalexp +1)]
trade[, ln_totalexp := log(totalexp)]

trade[, distw := distw/1000]
trade[, ln_distw := log(distw)] 

# Relabeling -------------------------------------------------------------

trade[, cty := as.integer(factor(iso_o))]
trade[, ptn := as.integer(factor(iso_o))]

# Count number of reporters and partners for dummies later ----------------

trade[, temp := .N, by = .(ptn, year)]
trade[, n_cty := max(temp)]
trade[, temp := NULL]

trade[, temp := .N, by = .(cty, year)]
trade[, n_ptn := max(temp)]
trade[, temp := NULL]

# Reshape -----------------------------------------------------------------
# We exclude the US to make it a reference point
trade[, ctn := relevel(iso_o, ref = "USA")]
fmp_log1 = feols(ln1_totalexp ~ ln_distw + contig | cnt + ptn , vcov = "hetero", data = trade)

# 2. Run the gravity regression
cc_vars <- grep("^cc", names(DT), value=TRUE)
pp_vars <- grep("^pp", names(DT), value=TRUE)
mod     <- feols(
  lexport ~ ldist + border + 
    paste(cc_vars, collapse="+") + 
    paste(pp_vars, collapse="+") - 1,
  data = DT, vcov = "HC1"
)

# 3. Extract coefs & predict
coefs <- coef(mod)
DT[, `:=`(
  lpred = predict(mod),
  pred  = exp(lpred),
  de1   = coefs["ldist"],
  de2   = coefs["border"]
)]

# 4. Build partner & reporter terms
DT[, `:=`(
  dm_p   = as.matrix(.SD[, pp_vars, with=FALSE]) %*% coefs[pp_vars],
  ds_c   = as.matrix(.SD[, cc_vars, with=FALSE]) %*% coefs[c("cc1","cc3","cc4",…)]
), .SDcols = c(pp_vars, cc_vars)
]

# 5. Aggregate into access measures
DT[, `:=`(
  trans  = dm_p + ds_c + de1*ldist + de2*border,
  edm_p  = exp(dm_p),
  eds_c  = exp(ds_c),
  t_rs   = (ldist^de1)*(border^de2),
  etrans = exp(trans),
  esuppl = exp(ds_c)
)]

DT[, fma := sum(etrans), by=.(reporter, year)]
DT[, fsa := sum(esuppl), by=.(partner,  year)]