# ST558-Project3
Final project for ST 558: R for Data Science.

This app is made in partial fulfillment of the requirements for the course ST558. 
In addition, this is created in an attempt to create an app that predicts diagnosis 
of breast cancer as benign or malignant based on different variable inputs.


Below are the packages required for this app:

1. `shiny` - framework for the app
2. `shinyWidgets` - interactive functionality
3. `tidyverse` - data manipulation and visualization
4. `readxl` - read-in excel data
5. `DT` - interactive table for Shiny
6. `shinythemes` - beautifying the app
7. `rattle` - visualize tree model
8. `caret` - train models
9. `ggplot2` - visualization
10. `imager` - incorporate images


Copy the code below to install the packages in R:
```{r}
packages <- c("shiny", "shinyWidgets", "tidyverse", 
              "readxl", "DT", "shinythemes", "rattle", 
              "caret", "ggplot2", "imager")

install.packages(packages)
lapply(packages, library, character.only = TRUE)
```


Copy the code below to run the app from GitHub:
```{r}
shiny::runGitHub("ST558-Project3", "jerryendrina", ref="main", subdir="/proj3_Endrina/")
```

