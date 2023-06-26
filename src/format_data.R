# function to perform data formatting steps

format_data <- function(df) {
  
  # column names to lower case
  names(df) <- tolower(names(df))
  
  dat <- data.frame(name = names(df), pos_sep = NA)
  
  # get position of last separator in column name strings
  for (i in 1:dim(dat)[1]) {
    
    y <- dat[i,]
    sep <- unlist(gregexpr('_', y, fixed = T))
    dat[i,]$pos_sep <- max(sep)
    
  }
  
  # replace column names
  name_n <- substr(dat$name, dat$pos_sep + 1, nchar(dat$name))
  names(df) <- name_n
  
  # remove intermediate variables
  rm(dat, y, sep, i, name_n)
  
  return(df)
  
}