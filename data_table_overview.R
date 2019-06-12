# data analysis with data.table 
# -----------------------------
library(data.table)
# 3 features:
# - goals of DT: 1) reduce programming time & 2) reduce compute time
# - general form: DT[i, j, by] -> in SQL: DT[where, select, groupby]
# - data.table is a data.frame too (DT looks to see where it's called from & redirects to a DF for those packages thhat don't know DT)!

mtcarsDT<- data.table(mtcars)
mtcarsDT[mpg>20, 
         .(AvgHP=mean(hp), "MinWT(kg)"=min(wt*453.6)), 
         by=.(cyl, under5gears=gear<5) ]

# general form:
DT<- data.table(A=1:6, B=c("a","b","c"), C=rnorm(6), D=TRUE)

# i argument
# ---
# select rows by number in i..
DT[3:5,]
# ..or:
DT[3:5]
# selecting same row twice..
DT[c(2,2,3)]
# select 2nd to last row..
DT[2:.N]







