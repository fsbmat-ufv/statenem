# statenem

Package for generating shiny dashboard with data from Enem. To install it just use:

```{r}
library(devtools)
install_github("fsbmat-ufv/statenem")
library(statenem)
```

The first functions to be used are:

```{r}
enemdown(2018)
enemclear(2018)
```
Then, turn shiny with the command:

```{r}
statenem::runExample()
```



