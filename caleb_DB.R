library(openxlsx)
library(dplyr)
wb <- loadWorkbook("data/Caleb's_scouting_database.xlsx")
turing <- read.xlsx(wb, sheet = "Turing", colNames = TRUE, startRow = 4)

write.csv(turing, file = "turing_caleb_data.csv")
