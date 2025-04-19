# Data, code and manuscript 'Estimating invasive rodent abundance using removal data and hierarchical models'

-   [analyses](https://github.com/oliviergimenez/counting-rodents/tree/main/analyses)
    -   `coypus.Rmd`: R code for running the coypus analyses
    -   `muskrats.Rmd`: R code for running the muskrats analyses
    -   `simulations.Rmd`: R code for running the simulations
    -   `simulations-closure.Rmd`: R code for running the simulations when the closure assumption is not met
    -   [data](https://github.com/oliviergimenez/counting-rodents/tree/main/analyses/data)
        -   `coypus.rds`: coypus data
        -   `temperature_netherlands.rds`: temperature data for 2014
        -   `temperature_netherlands_allperiod.rds`: temperature data for 1987-2014
-   [manuscript](https://github.com/oliviergimenez/counting-rodents/tree/main/manuscript)
    -   `rodent-abundance-from-removal.Rmd`: master file to produce manuscript 

**Author**:\
Gimenez, Olivier – CNRS Montpellier, France

**Abstract**: Invasive rodents pose significant ecological, economic, and public health challenges. Robust methods are needed for estimating population abundance to guide effective management. Traditional methods such as capture-recapture are often impractical for invasive species due to ethical, legal and logistical constraints. Here, I showcase the application of hierarchical multinomial N-mixture models for estimating the abundance of invasive rodents using removal data. First, I perform a simulation study which demonstrates minimal bias, as well as good precision and reliable coverage of confidence intervals across a range of sampling scenarios. I also illustrate the consequences of violating the population closure assumption, showing how between-occasion dynamics can bias inference. Second, I analyze removal data for two invasive rodent species, namely coypus (*Myocastor coypus*) in France and muskrats (*Ondatra zibethicus*) in the Netherlands. Using hierarchical multinomial N-mixture models, I examine the effects of temperature on abundance while accounting for imperfect and time-varying capture probabilities. I also show how to accommodate spatial variability using random effects, quantify uncertainty in parameter estimates, and account for violations of closure by fitting an open-population model to multi-year data. Overall, I hope to demonstrate the flexibility and utility of hierarchical models in invasive species management. 

