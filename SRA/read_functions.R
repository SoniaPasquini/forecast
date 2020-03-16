#read functions

library(xlsx)
library(readxl)

read_calendar <- function(path) {
  df = read_excel(paste0(path, "Calendar Eventi.xlsx"))
}

read_basi_calls <- function(path) {
  df = read_excel(paste0(path, "Calls_Basi_CY20.xlsx"))
}


#dati=read_excel("Calls_Basi_CY20.xlsx")
#eventi=read_excel("Calendar Eventi.xlsx")