# rs_create_statement_helpers.R
# after: https://github.com/sicarul/redshiftTools

#' Get size of character columns
#'
#' @param col
#'
#' @return integer size
#'
calculateCharSize <- function(col){
  col=as.character(col)
  maxChar = max(nchar(col), na.rm=TRUE)
  if(is.infinite(maxChar)){
    maxChar=1000
    warning('Empty column found, setting to 1024 length')
  }

  sizes = 2^c(3:16) # From 8 to 65536, max varchar size in redshift
  fsizes = sizes[ifelse(sizes>maxChar, TRUE, FALSE)]
  if(length(fsizes)==0){
    warning("Character column over maximum size of 65536, set to that value but will fail if not trimmed before uploading!")
    warning(paste0('Example offending value: ', head(col[nchar(col) > 65536], 1)))
    return(max(sizes, na.rm=TRUE))
  }else{
    return(min(fsizes, na.rm=TRUE))
  }
}

#' Get Redshift Data Type From R Column
#'
#' @param col from a R data frame
#' @param compression logical flag
#'
#' @return string with Redshift data type
#'
colToRedshiftType <- function(col, compression) {
  class = class(col)[[1]]
  switch(class,
         logical = {
           return('boolean')
         },
         numeric = {
           return('float8')
         },
         integer64 = {
           return('bigint')
         },
         integer = {
           if(all(is.na(col))){ #Unknown column, all null
             return('int')
           }
           if(max(col, na.rm = TRUE) < 2000000000){ # Max int is 2147483647 in Redshift
             return('int')
           } else if (max(col, na.rm=TRUE) < 9200000000000000000){ #Max bigint is 9223372036854775807 in redshift, if bigger treat as numeric
             return('bigint')
           } else{
             return('numeric(38,0)')
           }

         },
         Date = {
           return('date')
         },
         POSIXct = {
           return('timestamp')
         },
         POSIXlt = {
           return('timestamp')
         }

  )
  charSize = calculateCharSize(col)
  if(compression){
    return(paste0('VARCHAR(', charSize, ') encode zstd'))
  }else{
    return(paste0('VARCHAR(', charSize, ')'))
  }

}


#' Get Redshift Data Types From a Data Frame
#'
#' @param df  The data frame
#' @param compression logical flag
#'
#' @return list of data types as characters
getRedshiftTypesForDataFrame <- function(df, compression) {
  return(
    sapply(
      df,
      FUN = colToRedshiftType,
      compression
    )
  )
}