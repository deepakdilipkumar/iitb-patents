library(readxl)

 ee <- read_excel("..//data//patents.xlsx",1,skip=1)
 health <- read_excel("..//data//patents.xlsx",2,skip=1)
 ict <- read_excel("..//data//patents.xlsx",3,skip=1)
 manchem <- read_excel("..//data//patents.xlsx",4,skip=1)
 other <- read_excel("..//data//patents.xlsx",5)
 granted <- read_excel("..//data//patents.xlsx",6,skip=1)

