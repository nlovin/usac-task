# Lovin USAC Task

The best way to run the code would be to open the .Rproj first. This will ensure that `source("setup.R")` will find the find in its home directory. Just in case, I have included some code in the `usac_task_clean.R` script that should set the working directory to the file's location.

- `setup.R`: This file contains all the package installation and loading as well as the data collection and wrangling. It is called using `source()` in the other files.
- `usac_task_clean.R`: This file can setup and run all the code necessary to recreate the graphs from the report on an ad-hoc basis.
- `exe-summary.Rmd`: This file is used to generate the report. Open in Rstudio and knit to PDF.
- `dash-mockup.Rmd`: This file is used to generate the mock dashboard. Open in Rstudio and knit.