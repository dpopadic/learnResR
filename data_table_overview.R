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

# j argument
# ---
# select columns..
DT[,.(B, C)]
# computing on columns..
DT[,.(total=sum(A), avg=mean(C))]
# recycling in j (similar to mutate)..
DT[,.(B, C=sum(C))]
DT[,.(B,val=A*C)]
# throw anything in j..
DT[,plot(A,C)]
DT[, {
  print(A)
  hist(C)
  NULL
}]
# return a column as vector..
DT[,B]

# by argument
# ---
# doing j by group..
DT[, .(x1=sum(C), x2=mean(C)), by=.(B)]
# function calls in by..
DT[, .(x1=sum(C)), by=.(Grp=A%%2)]
# grouping only by subset..
DT[2:4, sum(C), by=A%%2]
# remember: no need to use .() if only 1 argument in i & j!

# some more basics..
# ---
DT<- data.table(iris)
# for each Species, compute mean of Sepal.Length..
DT[, mean(Sepal.Length), by=Species]
# compute mean Sepal.Length, grouping by first letter of Species..
DT[, mean(Sepal.Length), by=substr(Species,1,1)]
# group the specimens by Sepal area (to the nearest 10 cm2) and count how many occur in each group..
DT[, .N, by = 10 * round(Sepal.Length * Sepal.Width / 10)]
# name the output columns `Area` and `Count`..
DT[, .(Count=.N), by = .(Area=10 * round(Sepal.Length * Sepal.Width / 10))]
# return multiple numbers in j..
DT<- data.table(A = rep(letters[2:1], each = 4L), 
                B = rep(1:4, each = 2L), 
                C = sample(8))
# new data.table DT2
DT2<- DT[,.(C=cumsum(C)),by=.(A, B)]
# last two values from C while you group by A..
DT2[,.(C=tail(C,2)),by=A]





