library(sparklyr)
library(tidyr)
library(dplyr)
library(pryr)

# 1. Starting to use Spark with dplyr Syntax
# ------------------------------------------
utils::sessionInfo()

# potential problem: spark is new & sparklyr even newer, meaning some features are missing or 
# difficult to use & error messages aren't clear. scala/python interfaces to spark 
# are more mature

# workflow:
# 1. connect using spark_connect()
# 2. work
# 3. disconnect using spark_disconnect()

# install spark on local system..
spark_install()

# connect to spark cluster..
spark_conn <- spark_connect(master = "local")
# version of spark..
spark_version(sc = spark_conn)
# disconnect from spark..
spark_disconnect(sc = spark_conn)
# note: connecting to a cluster takes several seconds, so it's impractical to regularly connect & disconnect.


# copying data into spark..
# ---
# ..be warned: copying data is a fundamentally slow process. In fact, a lot of strategy regarding optimizing performance 
# when working with big datasets is to find ways of avoiding copying the data from one location to another

# copy track_metadata to spark..
spark_conn <- spark_connect(master = "local", version = "2.3.2")
track_metadata <- readRDS("track_metadata.rds") # not working for some reason, use the sample table below..
# track_metadata<- data.frame(x=rnorm(1000000), y=rnorm(1000000, 2 , 5))
track_metadata_tbl <- copy_to(dest=spark_conn, df=track_metadata)
# list the data frames available in spark..
src_tbls(spark_conn)

# link to the track_metadata table in spark..
track_metadata_tbl <- tbl(spark_conn, "track_metadata")
# see how big the dataset is..
dim(track_metadata_tbl)
# how much memory does the object take up..
pryr::object_size(track_metadata_tbl)
glimpse(track_metadata_tbl)

# limitations:
# - square bracket indexing is not currently supported in sparklyr -> tbl[, c("x", "y", "z")]
# - filtering: sparklyr converts your dplyr code into SQL database code before passing it to Spark. That means that only a limited 
# number of filtering operations are currently supported. For example, you can't filter character rows using regular 
# expressions with code like: tbl %>% filter(grepl("a regex", x)) -> available functionality: translate_sql()

track_metadata_tbl %>% filter(x > 0, x < 1)



# 2. Tools of the Trade: Advanced dplyr Usage
# -------------------------------------------

# collecting data back from spark..
# tibbles don't store a copy of the data. Instead, the data stays in Spark, and the tibble simply stores the details 
# of what it would like to retrieve from Spark.
# -> moving data from spark to r: collect()
class(track_metadata_tbl)
collected<- track_metadata_tbl %>% collect()
class(collected)
# copy_to: moving data from R to Spark
# collect: moving data from Spark to R
# .. copying data between R and Spark is a fundamentally slow task. That means that collecting the data, as you saw in the 
# previous exercise, should only be done when you really need to.

# storing intermediate results (sre results in a temporary data frame in spark)..
comp<- track_metadata_tbl %>% filter(x > 0, x < 1) %>% compute("xfilt")
src_tbls(spark_conn)
class(comp)

# sql queries..
library(DBI)
q <- "SELECT * FROM track_metadata WHERE x<1 AND y>3"
res <- dbGetQuery(spark_conn, q)


# 3. Use the native Interface to manipulate spark data frames
# -----------------------------------------------------------
# methods above use spark's sql interface, meaning they convert r-code into sql before passing it to spark..
# -> good for basic data manipulation, but issues with more complicated processing..
track_metadata_tbl %>% summarize(xm = mean(x)) # .. ok
track_metadata_tbl %>% summarize(xm = median(x)) # .. error
# .. sparklyr has 2 "native" interfaces (calling java/scala code to access spark libraries directly, w\o conversion to sql)


# sparklyr supports 2 more interfaces:
# 1. Spark DataFrame Application Programming Interface (API)
# spark dataframe functions named "sdf_"

# 2. Spark's machine learning library, MLlib
# feature transformation function named "ft_"
# ML functions named "ml_"

# note: spark is much stricter about variable types than R, meaning you need to convert logical/integer into numeric & back again

# transforming continuous variables to logical..
tmp <- track_metadata_tbl %>% select(x) %>% ft_binarizer("x", "xt", threshold = 0.5) %>% collect() %>% mutate(xt = as.logical(xt))
# transforming continuous variables to categorical..
splt <- seq(-5,6,1)
tmp <- track_metadata_tbl %>% select(x) %>% ft_bucketizer("x", "xcat", splits = splt) %>% collect() %>% mutate(xcat=factor(xcat))
# transforming continuous variables to categorical by quantile..
splt_lab <- c("very small", "small", "medium", "big", "very big")
tmp <- track_metadata_tbl %>% select(x) %>% 
                              ft_quantile_discretizer("x", "xcat", num_buckets = 5) %>% 
                              collect() %>% 
                              mutate(xcat = factor(xcat, labels = splt_lab))

# note:  there is currently no method for unnesting data on spark, so for now, you have to collect it to R before transforming it..
# list all DataFrames on Spark that sparklyr can see..
src_tbls(spark_conn)
sdf_schema(track_metadata_tbl)


# check memory..
library(pryr)
object_size(df.clust.def0) # size of single object
mem_used() # total size of all objects in memory
mem_change() # how memory changes during code execution -> mem_change(a <- 1:1e6)
# r uses garbage collection, meaning it automatically releases memory when an object is no longer used


# example..
spark_conn <- spark_connect(master = "local")
load('df.clust.def0.RData')
df_clust_def0_tbl <- copy_to(dest = spark_conn, df = df.clust.def0)
spark_disconnect(sc = spark_conn)



A <- data.frame(mtcars)
As <- copy_to(dest = spark_conn, df = A)
Av <- As %>% filter(hp >= 105) %>% ml_linear_regression(response = "hp", features = c("qsec","drat"))
summary(Av)
Ac <- As %>% filter(hp >= 105) %>% collect()











