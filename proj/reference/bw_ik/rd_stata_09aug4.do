**Test of stata vs. matlab runs:

/* output sent to rd_log_09aug4.log */
log using rd_log_09aug4, replace



/*  example of fuzzy regression discontinuity design */
/* read in data */
infile y w x z1 z2 z3 using art_fuzzy_rd.txt, clear

/* display summary statistics */
summ

/* estimate rd effect */
/* y is outcome */
/* x is forcing variable */
/* z1, z2, z3 are additional covariates */
/* w is treatment indicator */
/* c(0.5) implies that threshold is 0.5 */

rdob y x z1 z2 z3, c(0.5)  fuzzy(w)

/* if details on estimation are required */

rdob y x z1 z2 z3, c(0.5)  fuzzy(w) detail


clear
/*  example of sharp regression discontinuity design */

infile y x  z1 z2 z3 z4 using art_sharp_rd.txt, clear

summ
/* this can be run in two way, first as sharp design */
rdob y x z1 z2 z3 z4, c(0.2990)

/* it can also be run as a fuzzy design, with treatment indicator defined */
gen w=(x>0.2990)
rdob y x z1 z2 z3 z4, c(0.2990) fuzzy(w) 

log close