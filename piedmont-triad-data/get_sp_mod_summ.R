get_sp_mod_summ = function(terms, mod) {
  coefs = as.data.frame(summary(mod)["beta_table"])
  row = row.names(coefs) %in% terms
  lower = coefs[row, 1] - 1.96*coefs[row, 2]
  upper = coefs[row, 1] + 1.96*coefs[row, 2]
  data.frame(terms = terms,
             Est = exp(coefs[row, 1]),
             LB = exp(lower),
             UB = exp(upper))
}