# OBJECT ORIENTED PROGRAMMING IN R
# --------------------------------


# 1 OOP in R
# ----------
# one of the principles of OOP is that functions can behave differently for different kinds of object:
a_numeric_vector <- rlnorm(50)
a_factor <- factor(sample(c(LETTERS[1:5], NA), 50, replace = TRUE))
a_data_frame <- data.frame(n = a_numeric_vector, f = a_factor)
a_linear_model <- lm(dist ~ speed, cars)
# summary object illustration
summary(a_numeric_vector)
summary(a_data_frame)
summary(a_linear_model)
summary(a_factor)


# overview of 9 oop systems in R: ReferenceClasses, OOP, S3, S4, R5, R6, mutatr, proto, R.oo
# functional oop (objects contain data, class methods seperate from objects, not mutable): S3, S4
# encapsulated oop (objects contain data & methods, mutable): r6, ReferenceClasses

# - R5 and mutatr were experimental frameworks and difficult to use now
# - oop no longer availability
# - proto no longer used really (was used in ggplot2)
# - R.oo is active but not widely used
# - S3 is widely used, available since 1980s, mature and very simple system (only implements one feature 
#   of oop: the ability to have functions working in different ways on different types of objects)
# - S4 was introduced in the 4th version of the S language, it's mature but contains certain strange behaviour 
#   and therefore not the 1st choice for new projects (bioconductor pkg's use S4)
# - ReferenceClasses tries to emulate oop structure found in Java/C sharp (encapsulation, inheritance)
# - R6 is similar to ReferenceClasses but in a simpler way and is higher performance 
# in summary, use S3 in most cases and for more powerful requirements, use R6.
# also note while S3 mainly consists of polymorphism, R6 implements polymorphism, encapsulation & inheritance
# how r differentiates between objects
mat <- matrix(rnorm(12), 3)
# r class
class(mat)
# the exact object R understands in its internal c language code (useful for S3)
typeof(mat)

# 3 kind of functions in R:
# - closures: most of the functions that you come across are called closures.
# builtin: a few important functions, like length() are known as builtin functions (use a special evaluation mechanism to make them faster)
# special: language constructs, like if and while



# S3 Principles
# -------------

# function overloading: input-dependent function behavior -> used to simplify code
# methods are named: generic.class
# note:
# - check if a function is an S3 generic by calling pryr::is_s3_generic("sort")
# - check if a function is an S3 method by calling pryr::is_s3_method("seq.Date")


# find methods for generics: 
methods("mean")
methods(class = "glm")
.S3methods(class = "glm")
.S4methods(class = "glm")

# primitive generics (methods called in C when no R method detected): 
.S3PrimitiveGenerics
# for example, when calling is.na(list(TRUE, FALSE, NA)), neither is.na.default nor is.na.list exists, so primitive is.na is called

# 1. creating a generic class
get_n_elements <- function(x, ...) {
  UseMethod("get_n_elements")
}

# 2. create a data.frame method for get_n_elements
get_n_elements.data.frame <- function(x, ...) {
  nrow(x) * ncol(x)
}

# 3. call the method on the sleep dataset
get_n_elements(sleep)

# defining default methods:
get_n_elements.default <- function(x, ...) {
  length(unlist(x))
}

get_n_elements(ability.cov)


# multiple classes: classes should be ordered from more specific to more general as you move left to right:
kitty <- "Miaow!"
# assign classes
class(kitty) <- c("cat", "mammal", "character")
# check classes
inherits(kitty, "cat")
inherits(kitty, "mammal")
inherits(kitty, "character")
is.character(kitty)

# when objects have multiple classes, you may wish to call methods for several 
# of these classes. This is done using NextMethod():
# generic declaration
what_am_i <- function(x, ...) {
  UseMethod("what_am_i")
}

# cat method
what_am_i.cat <- function(x, ...) {
  message("I'm a cat")
  NextMethod("what_am_i")
}

# mammal method
what_am_i.mammal <- function(x, ...) {
  message("I'm a mammal")
  NextMethod("what_am_i")
}

# character method
what_am_i.character <- function(x, ...){
  message("I'm a character vector")
}

# Call what_am_i()
what_am_i(kitty)


# the idea of an object is really just to bundle data and corresponding methods together:
mo <- list(x = 5, get_x = function() "x was 5")
class(mo) <- "myClass"
class(mo)
mo$x
mo$get_x()

# however, this can lead to confusing & hard to debug code and therefore constructors should be used:
# a constructor for myClass...
myClass <- function(x){
  structure(class = "myClass", list(
    # attributes
    x = x,
    # methods
    get_x = function() paste("x was", x)
  ))
}

mo <- myClass(7)
class(mo)
mo$x
mo$get_x()




# R6 Principles
# -------------
library(R6)
# data and objects can be stored in the same variable in R6
# some conventions:
# - define class generators with R6Class()
# - class-names should be UpperCamelCase
# - data fields stored in private list
# - create object with $new method

# encapsulation: separating implementation from UI
# - store data in private list
# - store functions in public list
# - active bindings (to allow controlled access to private fields)
# use-cases: shiny, dplyr, processx


# a basic R6 class example:

Accumulator <- R6::R6Class("Accumulator",
                           public = list(
                             sum = 0,
                             add = function(x) {
                               self$sum <- self$sum + x # use self to access object properties
                               invisible(self)
                             }
                           )
)
# create instance of a class
x <- Accumulator$new()
x$add(4)
x$add(10)$add(10)
x$sum

# accessing fields in R6 classes:
# - private$ accesses private fields
# - self$ accesses public methods in self
# - super$ accesses public methods in parent

# inheritance: when a class is inherited, methods can either be overwritten (give same name) or 
# extended (give new name)


# environments: copy by reference (in contrast to lists which are copy by value)



















