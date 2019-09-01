### Setup File

# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# install packages
packages<-c("tidyverse","RSocrata", "fs", "rlang")
check.packages(packages)

rm(check.packages, packages)



#Check if the folder "Data" exists in the current directory, if not creates it
#ifelse(!dir.exists("Data"), dir.create("Data"), "Folder exists already")

if(!dir_exists("Data")) {
  
  dir_create("Data")
  print("Data directory created")
  
} else {
  
  print("Data folder already exists")
  
}

