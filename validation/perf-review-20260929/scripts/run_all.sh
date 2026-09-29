#!/bin/bash
cd "$(dirname "$0")"
mkdir -p prof
for s in cont_lm_p15 bin_glm_p20 surv_cox_p10 bin_lasso_p20 bin_glm_c3_p10 cont_lm_t_p10 bin_rf_p10 surv_rf_p10; do
  /usr/bin/time -l Rscript run_one.R $s > prof/$s.log 2>&1
  grep "elapsed" prof/$s.log | tail -1; grep "maximum resident" prof/$s.log
done
echo ALL DONE
