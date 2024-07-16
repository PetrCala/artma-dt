# Clean the environment
rm(list = ls())

# Load packages
invisible({
  if (!require("rstudioapi", quietly = TRUE)) install.packages("rstudioapi")
  suppressPackageStartupMessages(library("rstudioapi", quietly = TRUE))
})

# Working directory
if (!getwd() == dirname(getActiveDocumentContext()$path)) {
  newdir <- dirname(getActiveDocumentContext()$path)
  if (newdir == "") {
    stop("Setting the working directory failed. Try running the script again.")
  }
  cat(sprintf("Setting the working directory to: %s\n", newdir))
  setwd(newdir) # Set WD to the current file location
}

# Time the script run
# system.time(source("main"))

# Run the main file
source("main.R")
