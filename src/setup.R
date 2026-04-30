#-------------------------------------------------------------
# Name:         setup.R
# Description:  script sets up a working environment,
#               defines file paths for data import and output,
#               and loads required packages
# Author:       Florian Franz
# Contact:      florian.franz@nw-fva.de
#-------------------------------------------------------------



# 01 - setup working environment
#--------------------------------

# create directory called 'data' with subfolders
required_data_dirs <- c(
  'data',
  'data/raw_data',
  'data/raw_data/BI',
  'data/raw_data/orga',
  'data/raw_data/tree_species',
  'data/processed_data',
  'data/metadata'
)

for (dir_path in required_data_dirs) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
}

# create directory called 'src'
if (!file.exists(paste('src'))) {
  
  dir.create('src')
  
} else {
  
  invisible()
  
}

# create directory called 'Rmd'
if (!file.exists(paste('Rmd'))) {
  
  dir.create('Rmd')
  
} else {
  
  invisible()
  
}

# create directory called 'docs'
if (!file.exists(paste('docs'))) {
  
  dir.create('docs')
  
} else {
  
  invisible()
  
}

# create directory called 'scripts'
if (!file.exists(paste('scripts'))) {
  
  dir.create('scripts')
  
} else {
  
  invisible()
  
}

# create directory called 'output'
if (!file.exists(paste('output'))) {
  
  dir.create('output')
  
} else {
  
  invisible()
  
}

# list the files and directories
list.files(recursive = TRUE, include.dirs = TRUE)



# 02 - file path definitions
#---------------------------

# define raw data directory
raw_data_dir <- 'data/raw_data/'

# define processed data directory
processed_data_dir <- 'data/processed_data/'

# define output directory
output_dir <- 'output/'



# 03 - package loading
#----------------------

# load (and install) required packages
load_packages <- function(packages) {
  
  for (pkg in packages) {
    
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      
      message(paste("Package '", pkg, "' not found, attempting to install...", sep=""))
      install.packages(pkg, dependencies = TRUE)
      
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        
        stop(paste("Package '", pkg, "' not found and could not be installed.", sep=""))
        
      }
    }
  }
}

# install GitHub package if needed
load_github_package <- function(pkg, repo) {
  
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    
    message(paste("Package '", pkg, "' not found, attempting GitHub install from '", repo, "'...", sep = ""))
    
    if (!require("remotes", character.only = TRUE, quietly = TRUE)) {
      install.packages("remotes", dependencies = TRUE)
    }
    
    remotes::install_github(repo)
    
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(paste("Package '", pkg, "' not found and could not be installed from GitHub repo '", repo, "'.", sep = ""))
    }
  }
}


load_packages(c(
  'terra', 'lidR', 'sf', 'stats', 'moments',
  'dplyr', 'ggplot2', 'caret', 'exactextractr',
  'readr', 'randomForest', 'mapview',
  'scam', 'cowplot', 'mgcv', 'ggrepel',
  'DBI', 'odbc', 'tidyr', 'mase'
))

load_github_package('TreeGrOSSinR', 'rnuske/TreeGrOSSinR')