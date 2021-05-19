# statenem

Package for generating shiny dashboard with data from Enem. You can install statenem from github with:

```{r}
# install.packages("devtools")
devtools::install_github("fsbmat-ufv/statenem")
```

If you want to access the data used, you can use the functions:

```{r}
library(statenem)
enemdown(2018)
enemclear(2018)
```
To generate the shiny, just use:

```{r}
statenem::runExample()
```

The package is part of the results of the master's thesis work **Aplicação do Modelo de Regressão Logística em Dados de Rendimento do Exame Nacional do Ensino Médio (Enem)**. Defended by student Eveline Júnia Brant Mariz at the Federal University of Viçosa Campus UFV - Florestal in June 2021.

