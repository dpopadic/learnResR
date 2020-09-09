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
# - builtin: a few important functions, like length() are known as builtin functions (use a special evaluation mechanism to make them faster)
# - special: language constructs, like if and while



# S3 Principles
# -------------

# function overloading: input-dependent function behavior -> used to simplify code
# methods are named: generic.class
# note:
# - check if a function is an S3 generic by calling pryr::is_s3_generic("sort")
# - check if a function is an S3 method by calling pryr::is_s3_method("seq.Date")

# --- generics
# find methods for generics: 
methods("mean")
methods(class = "glm")
.S3methods(class = "glm")
.S4methods(class = "glm")

# primitive generics (methods called in C when no R method detected): 
.S3PrimitiveGenerics
# for example, when calling is.na(list(TRUE, FALSE, NA)), neither is.na.default nor is.na.list exists, so primitive is.na is called

# --- creating a simple S3 class
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


# --- object with multiple classes
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


# --- s3 constructor
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


# create and assign class in one step
foo <- structure(list(), class = "foo")

df <- data.table::data.table(val = rnorm(10))
class(df)
typeof(df)
df <- structure(df, class = c("data.table", "prep"))
class(df)




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

# --- simple example
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
# - public: contains the functionality available to the user
# - private$ accesses private fields
# - self$ accesses public methods in self
# - super$ accesses public methods in parent

# inheritance: when a class is inherited, methods can either be overwritten (give same name) or 
# extended (give new name)
# environments: copy by reference (in contrast to lists which are copy by value)


# --- private, public & active fields in R6 classes
# class generator (note that new method doesn't need to be defined)
# - initialize() lets you set the values of the private fields when you create an R6 object
# - finalize() lets you run custom code when objects are destroyed (finalize() should take 
#   no arguments) - eg. finalize = function() {message("closing statement.")}

microwave_oven_factory_org <- R6::R6Class(
  "MicrowaveOven",
  private = list(
    power_rating_watts = 800,
    door_is_open = FALSE
  ),
  public = list(
    cook = function(time_seconds) {
      Sys.sleep(time_seconds)
      print("Your food is cooked!")
    },
    open_door = function() {
      private$door_is_open <- TRUE
    },
    close_door = function() {
      private$door_is_open <- FALSE
    },
    # add initialize() method here
    initialize = function(power_rating_watts, door_is_open) {
      if(!missing(power_rating_watts)) {
        private$power_rating_watts <- power_rating_watts
      }
      if(!missing(door_is_open)) {
        private$door_is_open <- door_is_open
      }
      
      
    }
  )
)

# instantiate an instant of the class
microwave_oven <- microwave_oven_factory_org$new()
# with fields
microwave_oven <- microwave_oven_factory_org$new(power_rating_watts = 650, door_is_open = TRUE)
microwave_oven$cook(1)



# --- active bindings
# active bindings: exposing data in the class (note that the data stored by an R6 object is deliberately 
# hidden away from the user by keeping it in the private element. This is the principle of encapsulation.
# If you want to provide access to any of the data values, you can use an active binding. These are 
# functions that behave like variables.
# - elements of private, public & active must all have different names
# - to distinguish between active & private fields, it's useful to start all private fields with ..

# example with microwave exposing power rating watts:
microwave_oven_factory <- R6::R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800
  ),
  active = list(
    # add the active binding here
    "power_rating_watts" = function() {
      private$..power_rating_watts
    }
  )
)
microwave_oven <- microwave_oven_factory$new()
# get the power rating
microwave_oven$power_rating_watts


# Active bindings can also be used to set private fields. In this case, the binding function should 
# accept a single argument, named "value":
microwave_oven_factory <- R6::R6Class(
  "MicrowaveOven",
  private = list(
    ..power_rating_watts = 800,
    ..power_level_watts = 800
  ),
  active = list(
    power_level_watts = function(value) {
      if(missing(value)) {
        private$..power_level_watts
      } else {
        # assert that value is a number
        assertive::assert_is_a_number(value)
        # assert that value is in a closed range from 0 to power rating
        assertive::assert_all_are_in_closed_range(
          value, lower = 0, upper = private$..power_rating_watts
        )
        # set the private power level to value
        private$..power_level_watts <- value
      }
    }
  )
)

a_microwave_oven <- microwave_oven_factory$new()
a_microwave_oven$power_level_watts
# try setting the power level to "400"
a_microwave_oven$power_level_watts <- "400"
# try to setting the power level to 1600 watts
a_microwave_oven$power_level_watts <- 1600
# try setting the power level to 400 watts
a_microwave_oven$power_level_watts <- 400


# --- inheritance
# inherit microwave oven
fancy_microwave_oven_factory <- R6::R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory_org
)
# inheritance means that the methods of the child class are exact copies of those in the parent class:
microwave_oven <- microwave_oven_factory_org$new()
fancy_microwave <- fancy_microwave_oven_factory$new()
# verify that these are the same
identical(microwave_oven$power_rating_watts, fancy_microwave$power_rating_watts)
# cook with each microwave
microwave_oven$cook(1)
fancy_microwave$cook(1)


# - extend the inherited class definition:
fancy_microwave_oven_factory <- R6::R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory_org,
  # add a public list with a cook baked potato method
  public = list(
    cook_baked_potato = function() {
      self$cook(3)
    }
  )
)

a_fancy_microwave <- fancy_microwave_oven_factory$new()
# call cook_baked_potato method
a_fancy_microwave$cook_baked_potato()


# override inherited class definition:
# note: - do this by defining methods with the same name as that of the parent
#       - child classes can access public methods from their parent class by prefixing the name with super$

fancy_microwave_oven_factory <- R6::R6Class(
  "FancyMicrowaveOven",
  inherit = microwave_oven_factory_org,
  # add a public list with a cook method
  public = list(
    cook = function(time_seconds) {
      super$cook(time_seconds)
      message("Enjoy your dinner!")
    }
  )
)

a_fancy_microwave <- fancy_microwave_oven_factory$new()
a_fancy_microwave$cook(1)


# - multi-level inheritance
# By default, R6 classes only have access to the functionality of their direct parent. To allow access 
# across multiple generations, the intermediate classes need to define an active binding that exposes their parent: super_
# define a high-end microwave oven class
high_end_microwave_oven_factory <- R6::R6Class(
  "HighEndMicrowaveOven",
  inherit = fancy_microwave_oven_factory,
  public = list(
    cook = function(time_seconds) {
      # access methods across several generations: great_grand_parent_method <- super$super_$super_$method()
      super$super_$cook(time_seconds)
      message("Cooking my own meal now.")
    }
  )
)

high_end_microwave <- high_end_microwave_oven_factory$new()
high_end_microwave$cook(1)


# --- working with environments
# - environment variable type is similar to a list in that it can contain other variables
# - Environments use a different system, known as "copy by reference", so that all copies are identical; changing 
#   one copy changes all the copies
# - R6 objects use the same copy by reference behavior as environments. Use a_value <- an_r6_object$clone() to copy by value
# - If an R6 object contains another R6 object in one or more of its fields, then by default clone() will copy the R6 
#   fields by reference. To copy those R6 fields by value: clone(deep = TRUE)


# define a new environment
env <- new.env()
# add an element named perfect
env$perfect <- c(6, 28, 496)
# add an element named bases
env[["bases"]] <- c("A", "C", "G", "T")

# copy-by-reference illustration: list vs environments
# list:
lst <- list(
  perfect = c(6, 28, 496),
  bases = c("A", "C", "G", "T")
)
lst2 <- lst
# change lst's bases element
lst$bases[4] <- "U"
# test lst and lst2 identical
identical(lst2, lst) 

# environment:
env <- list2env(lst)
env2 <- env
env$bases[4] <- "U"
identical(env, env2)


# --- sharing fields across classes
# sharing fields in R6 classes by reference: R6 classes can use environments' copy by reference behavior 
# to share fields between objects. To set this up, define a private field named shared..

# steps: 
# 1. Create a new environment
# 2. Assign any shared fields to that environment
# 3. Return the environment
# The shared fields should be accessed via active bindings. These work in the same way as other active bindings 
# that you have seen, but retrieve the fields using a private$shared$ prefix.

microwave_oven_factory <- R6::R6Class(
  "MicrowaveOven",
  private = list(
    shared = {
      # create a new environment named e
      e <- new.env()
      # assign safety_warning into e
      e$safety_warning <- "Warning. Do not try to cook metal objects."
      # return e
      e
    }
  ),
  active = list(
    # add the safety_warning binding
    safety_warning = function(value) {
      if(missing(value)) {
        safety_warning <- private$shared$safety_warning
      } else {
        safety_warning <- value
      }
    }
  )
)

microwave_oven <- microwave_oven_factory$new()
another_microwave_oven <- microwave_oven_factory$new()
# change the safety warning for microwave_oven
microwave_oven$safety_warning <- "Warning. If the food is too hot you may scald yourself."
# verify that the warning has change for another_microwave
another_microwave_oven$safety_warning








