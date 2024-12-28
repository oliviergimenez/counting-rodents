# Data, code and manuscript 'Estimating invasive rodent abundance using removal data and hierarchical models'

-   [analyses](https://github.com/oliviergimenez/counting-rodents/tree/main/code)
    -   `coypus.Rmd`: R code for running the coypus analyses
    -   `muskrats.Rmd`: R code for running the muskrats analyses
    -   `simulations.Rmd`: R code for running the simulations
    -   [data](https://github.com/oliviergimenez/counting-rodents/tree/main/analyses/data) with and without spatial autocorrelation
        -   `coypus.rds`: coypus data
        -   `temperature_netherlands.rds`: temperature data for 2014
        -   `temperature_netherlands_allperiod.rds`: temperature data for 1987-2014
-   [manuscript](https://github.com/oliviergimenez/counting-rodents/tree/main/manuscript)
    -   `rodent-abundance-from-removal.Rmd`: master file to produce manuscript 

**Author**:\
Gimenez, Olivier – CNRS Montpellier, France

**Abstract**: Invasive rodents pose significant ecological, economic, and public health challenges. Robust methods are needed for estimating population abundance to guide effective management. Traditional methods such as capture-recapture are often impractical for invasive species due to ethical and logistical constraints. Here, I showcase the application of hierarchical multinomial N-mixture models for estimating the abundance of invasive rodents using removal data. First, I performed a simulation study which demonstrated minimal bias in abundance estimates across a range of sampling scenarios. Second, I analyzed removal data for two invasive rodent species: coypus (*Myocastor coypus*) in France and muskrats (*Ondatra zibethicus*) in the Netherlands. Using hierarchical multinomial N-mixture models, I examined the effects of temperature on abundance while accounting for imperfect and time-varying capture probabilities. I also showed how to accommodate spatial variability using random effects, and quantified uncertainty in parameter estimates. Overall, I hope to demonstrate the flexibility and utility of hierarchical models in invasive species management. I provide reproducible code and data to encourage broader adoption of multinomial N-mixture models.
