# rddapp 0.1.0

- Initial commit.

- Add R functions: estimation, power analysis, and assumption checks.

- Add R tests: estimation and assumption checks.

- Add CARE dataset.

# rddapp 1.0.0

- Add shiny app.

- Add R function: shiny app launch.

# rddapp 1.1.0

- Shiny app has new download buttons, screenshots, and a draft manual

- Several bugfixes in functions that used the bootstrap (bad integrand issue fixed)

- Summary functions have been updated to displays results in more consistent format

- Summary functions are now including confidence intervals by default

- Stability fixes for dc_est function

- Default of t.design was changed to NULL to force user to always specify design

# rddapp work in progress

- Nonparametric version of frontier approach is added to mfrd_est and summary function is updated to display the new results

- Summary functions have been updated to display results according to selection of est.cov and est.itt

- Summary functions have been added for functions rd_power and mrd_power

- Bugfixes in mrd_impute function, including summary function

- Incorrect outputs for Cohen'd of covariates are removed from rd_est and mrd_est

- Help files have been updated e.g. example added for rd_type, more descriptions of arguments added for plot.mfrd and predict.rd
