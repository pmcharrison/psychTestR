###### Video 1 - R Script for Introductory Video Series to PsychTestR
# Video series made by Anthony Chmiel (The MARCS Institute for Brain, Behaviour and Development. Western Sydney University. Copyright 2021)
# These steps are intended for people who are entirely new to R. If you are already familiar with R, proceed to Video #2 in this series.
# Thanks go to Peter M. C. Harrison and Seb Silas for their assistance throughout the creation of this video series.

#####

## 1. Installing R, and installing and loading packages within R ##
#First you need to install R (https://cloud.r-project.org/), and preferably also R Studio (https://www.rstudio.com/).
# Once these are installed, load up this script within R Studio.

# Next we will  install some necessary packages. Let's begin by installing the package devtools.
# Type in the below command, highlight it, and then hit the "Run" button in the top right (or use shortcut keys...command and enter on Mac, or control and enter on Windows)
install.packages("devtools")

# In the console window (below) you will R go through the process of installing the Devtools package and some other required packages.
# This will take some time. After all installation is ready, you will see the > symbol reappear in the console, signalling R is ready for you

# Now we will install another two packages called Shiny and readr. We won't necessarily be using Shiny, but it is useful to have in case you want to later on run online experiments
install.packages("shiny")
install.packages("readr")

# Fourth, we will install the PsychTestR package itself. But we will use a different type of command where we tell the devtools package to install psychTestR for us (so devtools must be installed beforehand)
devtools::install_github("pmcharrison/psychTestR")

#To install some of Peter Harrison's Gold-MSI experiments that run via PsychTestR, you can use the three commands below to install each of the packages cabat, mdt, and mpt
devtools::install_github("pmcharrison/cabat")
devtools::install_github("pmcharrison/mdt")
devtools::install_github("pmcharrison/mpt")

# In video 3 we will be testing out some of the mdt package features, so it is a good idea to install at least the mdt package now



# Importantly, each time you run R and want to use psychTestR you will need to make sure that you load up the psychTestR package. Do this with the below command
library(psychTestR)
# or similarly, use the below command to run the mdt (Melodic Discrimination test) experiment
library(mdt)


#####

## 2. Loading in excel, csv, and rds files
# Many existing PsychTestR experiments will save csv or xlsx files (spreadsheet files) and also RDS files (files that default to R or R studio).
# We wont be looking at results files in this video, but in Video 2 we will be. So it is a good idea for us to learn how to open these files in R - existing R users can likely skip this part. 

# First, make sure that you have the three files that were supplied along with this guide (results.rds, results.csv, and results.xlsx) and be sure to note where these have been placed. If in doubt, just place them on the desktop.
# (N.B., the data within these files are just dummy data that I manually created)

# First, let's read the csv file results.csv by running the below code:
read.csv("~/Desktop/results.csv")
# The data will automatically appear in our console window
# We can also decide to save this data within the "Global environment" of R, which is your available working memory for the program. The Global Environment is in the top right of the screen.
# In the below command we are using the same results.csv file, but we will create a file within R called "csv_data". Once you run the command, a new object called CSVdata will appear in the GLobal Environment
csv_data <- read.csv("~/Desktop/results.csv")
# Watch Barton Poulson's for more information on how to read this object and manipulate the contents, but as one quick example you can get an overview of the data with these two commands:
summary(csv_data)
View(csv_data)   #this second command is essentially the same as the read.csv command above, but it will open in a new window



# Now we will also read an xlsx file (in case they are saved that way instead of csv files). First we need to load a package called readxl
library(readxl)
# Next I will create an object within R called "xlsxdata" and I am telling R to read the .xlsx file that is located on my desktop to create that object. Change the file path to wherever you like, so long as the xlsx file has been placed there
xlsx_data <- read_excel("~/Desktop/results.xlsx")
# The object xlsx_data will appear in my "Global environment". As before, watch Poulson's video for more information on this as it is a key functionality of using R.
# We can also get R to list the data for us with the two commands below:
summary(xlsx_data)
View(xlsx_data)


# Finally, I will create another object within R, this time called "rds_results" and I am telling R to read the .rds file that is located on my desktop. Change the file path to wherever you like, so long as the rds file has been placed there
rds_results <- readRDS("/Users/anthonychmiel/Desktop/results.rds")
# As above, after I successfully run this command I will see the "rds_results" object appear in the "Global environment" of R
# The RDS Data will be split into spearate files for each participant/session. As above we can view the RDS data within R
summary(rds_results)
View(rds_results)
# When we view the RDS data, you can use your cursor to click on the expandable arrows to view data within
# As we will discuss in video 3, there are more complex ways that you can use PsychTestR to save and view RDS data. 
# If you would like more information on viewing and manipulating data in R watch Barton Poulson's video (referred to at the beginning of this video), and there are numeorus other free R tutorial videos online that discuss RDS data.


#####
#This is the end of Video 1 - in the next video we will begin creating our own psychTestR experiment.
