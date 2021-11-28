# ST558-Project3
Final project for ST 558: R for Data Science.

This app is made in partial fulfillment of the requirements for the course ST558. 
In addition, this is created in an attempt to create an app that predicts diagnosis 
of breast cancer as benign or malignant based on different variable inputs.

Below are the packages required for this app:

1. `shiny` - 
2. `shinywidgets` - 
3. `tidyverse` -
4. `readxl` -
5. `DT` - 
6. `shinythemes` -
7. `rattle` -
8. `caret` -
9. `ggplot2` - 
10. `imager` -

Copy the code below and run in R to install the packages:

```{r}
packages <- c("shiny", 
              "shinywidgets", 
              "tidyverse", 
              "readxl",
              "DT", 
              "shinythemes", 
              "rattle", 
              "caret", 
              "ggplot2", 
              "imager")

install.packages(packages)
lapply(packages, library, character.only = TRUE)
```

