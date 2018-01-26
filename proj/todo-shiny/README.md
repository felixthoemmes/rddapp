## TODO ##

* [ ] Shiny Interface
	- [ ] Model Page
	  - [X] Input Panel
	  - [X] Data Panel
        - [X] Which variables had which roles and descriptive stats (TABLE 1.1)
        
            |                 | _N_ | _M_ | _SD_|
            |:--------------- |:---:|:---:|:---:|
            |var1 (T)         |     |     |     |
            |var2 (O)         |     |     |     |
            |var3 (A1)        |     |     |     |
            |var4 (A2)        |     |     |     |
            |var5             |     |     |     |
            |var6             |     |     |     |
        
        - [X] What was the cutoff, and what is the design (TABLE 1.2)
        
            |   A1    |   A2   | _N_ | _M_ | _SD_|
            |:--------|:-------|:---:|:---:|:---:|
            | < cut1  | < cut2 |     |     |     |
            | < cut1  | ≥ cut2 |     |     |     |
            | ≥ cut1  | < cut2 |     |     |     |
            | ≥ cut1  | ≥ cut2 |     |     |     |

        - [X] Raw data (TABLE 1.3)
	  - [X] Assumptions 
        - [X] Attrition analysis (TABLE 2.1)
        
            |                 | _N_ |  %  |
            |:--------------- |:---:|:---:|
            |Total sample size|     |     |
            |Missing on Y     |     |     |
            |Missing on T     |     |     |
            |Missing on X1    |     |     |
            |Missing on X2    |     |     |
            |Missing on Y\|T  |     |     |
        
        - [X] McCrary sorting test (Table + Plot)
        - [X] Specification test of linear assumption (Table + Interactive Plot)
    - [ ] Result Panel
        - [X] Summaries of parametric, and non-parametric analysis (Table)
        - [ ] Plot (2D or 3D)
    - [ ] Sensitivity Panel
        - [ ] Placebo test (Plot)
        - [ ] Ensure covariates are not showing discontinuties (Plot)
	  - [ ] R Code Panel
	- [ ] Power Page
	- [ ] Report Page
* [ ] Data Procedures
    - [ ] Read in Data
    - [ ] Check Data
* [ ] Estimators
    - [ ] Parametric RDD
    - [ ] Parametric Fuzzy RDD
    - [ ] Parametric Frontier RDD
    - [ ] Non-Parametric RDD
    - [ ] Non-Parametric Fuzzy RDD
    - [ ] Non-Parametric Frontier RDD
* [ ] Power Analysis
    - [ ] Parametric RDD
    - [ ] Parametric Fuzzy RDD
    - [ ] Parametric Frontier RDD
    - [ ] Non-Parametric RDD
    - [ ] Non-Parametric Fuzzy RDD
    - [ ] Non-Parametric Frontier RDD
* [ ] Graphs
       - [ ] Univariate distribution with kernel density estimate of assignment variable for sorting test
       - [ ] Binned dot plot with overlaid smoother of assignment variable and outcome
* [ ] Bandwith
    - [ ] Plugin 1
    - [ ] Plugin 2
    - [ ] Interactive
* [ ] Robust SE (`rddtools`)
* [ ] Regression Kink Design
* [ ] Multiple Imputation
* [ ] Utilities
  - [ ] McCrary Sorting Test (`rdd`)
  - [ ] Placebo Test (`rddtools`)
  - [ ] Sensitivity to Bandwidth (`rddtools`)
  - [ ] RDD coefficient prediction (`rddtools`)
  - [ ] Test for balanced covariates (`rddtools`)
  
