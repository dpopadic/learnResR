# data analysis with data.table 
# -----------------------------
library(data.table)
# 3 features:
# - goals of DT: 1) reduce programming time & 2) reduce compute time
# - general form: DT[i, j, by] -> in SQL: DT[where, select, groupby]
# - data.table is a data.frame too (DT looks to see where it's called from & redirects to a DF for those packages thhat don't know DT)!

# --- 1 data.table novice
# -----------------------
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
irisDT<- data.table(iris)
# for each Species, compute mean of Sepal.Length..
irisDT[, mean(Sepal.Length), by=Species]
# compute mean Sepal.Length, grouping by first letter of Species..
irisDT[, mean(Sepal.Length), by=substr(Species,1,1)]
# group the specimens by Sepal area (to the nearest 10 cm2) and count how many occur in each group..
irisDT[, .N, by = 10 * round(Sepal.Length * Sepal.Width / 10)]
# name the output columns `Area` and `Count`..
irisDT[, .(Count=.N), by = .(Area=10 * round(Sepal.Length * Sepal.Width / 10))]
# return multiple numbers in j..
DT<- data.table(A = rep(letters[2:1], each = 4L), 
                B = rep(1:4, each = 2L), 
                C = sample(8))
# new data.table DT2
DT2<- DT[,.(C=cumsum(C)),by=.(A, B)]
# last two values from C while you group by A..
DT2[,.(C=tail(C,2)),by=A]


# --- 2 data.table yeoman
# -----------------------
DT<- data.table(A=c("c","b","a","c","b","a"), B=as.integer(1:6))

# chaining operations together..
# ---
# normal..
ans<- DT[, sum(B), by=A]
ans[order(A)]
# chaining..
ans<- DT[, sum(B), by=A][order(A)]

# basics of chaining
# ---
DT <- data.table(A = rep(letters[2:1], each = 4L), 
                 B = rep(1:4, each = 2L), 
                 C = sample(8))

# combine the 2 steps in a one-liner:
# long version..
DT2<- DT[,.(C=cumsum(C)),by=.(A, B)]
DT2[,.(C=tail(C,2)),by=A]
# chaned version..
DT2<- DT[, .(C = cumsum(C)), by = .(A, B)][, .(C = tail(C, 2)), by = A]
# chains on calculations..
irisDT[, .(Sepal.Length = median(Sepal.Length), 
       Sepal.Width = median(Sepal.Width), 
       Petal.Length = median(Petal.Length),
       Petal.Width = median(Petal.Width)), 
       by = Species][order(-Species)]
# quite tedious if 100 columns.. how to compute this for all columns? -> .SD
# .SD: refers to the subset of data for each unique value of by argument
irisDT[, lapply(.SD, median), by=Species]

# .SDcols: allows you to apply a function to all rows of a data.table, but only to some of the columns..
DT<- data.table(grp=c(6,6,8,8,8),Q1=c(5,5,2,4,5),Q2=c(2,2,5,2,3),Q3=c(2,2,4,2,5),H1=c(5,5,3,2,4),H2=c(4,3,4,4,4))
# basic..
DT[, lapply(.SD, sum), .SDcols=2:4]
DT[, lapply(.SD, sum), .SDcols=paste0(c("H1","H2"))]
# select all but the 1. row of groups 1 and 2, returning only the grp column and the Q columns..
DT[,.SD[-1], .SDcols=paste0(c("Q1","Q2","Q3")), by=grp]

# mixing all functionalities..
DT<- data.table(x=c(2,1,2,1,2,2,1), y=c(1,3,5,7,9,11,13), z=c(2,4,6,8,10,12,14))
# sum of all columns x, y and z and the # rows in each group while grouping by x..
DT[,lapply(.SD,sum),.SDcols=paste0(c("x","y","z")),by=x]
# cumulative sum of column x and y while grouping by x and z > 8..
DT[,lapply(.SD,cumsum),.SDcols=paste0(c("x","y")),by=.(by1=x, by2=z>8)]

# add/update columns in j
# ---
DT<- data.table(x=c(1,1,1,2,2), y=c(6,7,8,9,10))

# 2 columns..
DT[, c("x","z") := .(rev(x), 10:6)]
# 1 column..
DT[, x:=rev(x)]
# remove columns by reference..
DT[,c("y","z"):=NULL] # or if just removing 1 column: DT[,y:=NULL]
# remove columns whose names are stored in a variable..
DT<- data.table(x=c(1,1,1,2,2), y=c(6,7,8,9,10))
DT[, c("x","z") := .(rev(x), 10:6)]
rm.clm<- c("y","z")
DT[,(rm.clm):=NULL]
# alternatively, generate column-names to delete on the fly: DT[,paste0("colNamePrefix", 1:4):=NULL]
# functional := (values alongside column-names, easier to read)..
DT[,`:=`(y=6:10, z=1)]
# update for a subset by group..
DT<- data.table(x=c(2,2,1,1,1), y=c(6,7,8,9,10), z=as.numeric(rep(NA,5)))
DT[2:4, z :=sum(y), by=x]
# adding, updating & removing columns
DT<- data.table(A = letters[c(1, 1, 1, 2, 2)], B = 1:5)
# add column by reference..
DT[,Total:=sum(B),by=A]
# add 1 to column B, but only for rows 2,4..
DT[c(2,4),B:=B+1L]
# add a new column Total2, sum(B) grouped by A over rows 2,3,4..
DT[c(2,3,4),Total2:=sum(B),by=A]
# remove Total column..
DT[,Total:=NULL]
# select the third column..
DT[[3]]

# updating with functional form..
DT<- data.table(A = c(1, 1, 1, 2, 2), B = 1:5)
# update B, add C & D..
DT[,`:=`(B=B+1, C=A+B, D=2)]
# delete my_cols..
my_cols<- c("B", "C")
DT[,(my_cols):=NULL]
# delete column 2 by number..
DT[,2:=NULL]

# using set..
# ---
# if you want to repeatedly update DT by reference, use set instead of for-loop
# set can't be used for grouping operations!
# for-loop: for (i in 1:5) DT[i, z:=i+1]
DT<- data.table(x=c(1,2,3,4,5), y=c(1,8,1,1,1), z=c(2,5,4,2,3))
for (i in 1:5) set(DT, i, 3L, i+1) # update 3rd column
# renaming..
setnames(DT, "y", "y2")
# reordering DT-columns..
setcolorder(DT, c("y2","x","z"))

# another example..
DT<- data.table(A=c(2,2,3,5,2,5,5,4,4,1),B=c(2,1,4,2,4,3,4,5,2,4),C=c(5,2,4,1,2,2,1,2,5,2),D=c(3,3,3,1,5,4,4,1,4,3))
# loop through columns 2, 3, 4 - for each one, select 3 rows at random and set the value of that column to NA..
for (i in c(2,3,4)) set(DT, sample(1:10,3,replace=FALSE), i, NA)
setnames(DT,colnames(DT),tolower(colnames(DT)))


# --- 3 data.table expert
# -----------------------
# analysing indexing functionality
DT<- data.table(A=c("c","b","a","c","b","a"),B=c(1,2,3,4,5,6))

# filtering..
# ---
DT[A=="a"]
DT[A %in% c("a","c")]
# what's happening? conceptually: 1. w<- DT[,A=="a"], 2. DT[w]
# .. data.table automatically creates an index on A the 1st time you use column A - so the 2nd time is much faster!

# changing/deleting column-names dynamically.
setnames(irisDT, gsub("^Sepal\\.", "", names(irisDT)))
irisDT[, grep("^Petal", names(irisDT)) := NULL]

# filtering..
irisDT[,is_large := Width*Length>25]
irisDT[is_large==TRUE] # or: irisDT[(is_large)]

# creating & using a key..
# ---
 # sorts DT by key & references by column..
setkey(DT, A)
# identify key's of DT..
key(DT)
# after setting key, you can look-up values in key-column:
DT["b"]
# using "mult"..
DT["b", mult="first"]
DT["b", mult="last"]
# nomatch..
DT[c("b","d")]
DT[c("b","d"), nomatch=0] # ..return only matching rows

# 2-column key:
DT<- data.table(A=c("c","b","a","c","b","a"),B=c(1,2,3,4,5,6),C=c(1,2,3,4,5,6))
setkey(DT, A, B)
DT[.("b",5)]
DT[.("b",6)]
DT[.("b")]
# select 1st & last row of the groups..
DT[c("b", "c"), .SD[c(1, .N)], by = .EACHI]
# print out the group before returning the 1st & last row from it (use curly brackets to include 2 separate instructions inside the j argument)..
DT[c("b", "c"), { print(.SD); .SD[c(1, .N)] }, by = .EACHI]


# rolling joins..
# ---
DT<- data.table(A=c("a","a","b","b","c","c"), B=c(2,6,1,5,3,4), C=c(6,3,2,5,4,1))
setkey(DT,A,B)
DT[.("b",4)] # -> no match!
# ..roll the prevailing observation forward:
DT[.("b",4), roll=TRUE]
# alternatively: nearest, +Inf, -Inf (always roll forward/backward)
DT[.("b",4), roll="nearest"]
DT[.("b",4), roll=+Inf]
DT[.("b",4), roll=-Inf]
# join to prevailing observation but only if prevailing observation falls within a certain window of data that you're joining from..
DT[.("b",4), roll=2]
DT[.("b",4), roll=-2]

# control ends..
DT[.("b",7:8), roll=TRUE]
DT[.("b",7:8), roll=TRUE, rollends=FALSE]






