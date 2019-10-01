# --- PKG NOTES

# mandatory information:
# - R directory
# - man directory
# - NAMESPACE file

# core devtools functions:
# - create(): create the package
# - document()
# - check()
# - build()
# - test()

# include a number of additional directories containing elements such as vignettes (user guides), data 
# and unit tests: use_*-functions -> eg. use_data(), use_vignette()

# imports from other packages:
# @import to import an entire package, or @importFrom to import a single function
# #' @importFrom tidyr gather

# exported functions:
# - visible to end user
# - key package functionality

# non-exported functionality:
# - not visible to end user
# - utility functions

# documenting data objects: see "pkg_notes_data_docu"

# R-package check includes:
# - if package can be installed
# - description info is correct
# - dependencies
# - code syntax errors
# - documentation is complete
# - tests run
# - vignettes build

# .. devtools:check() helps here

# suggests: used in examples or tests but not required to use the package
# Depends & Imports in DESCRIPTION: difference between them is that the packages listed in depends are 
# attached when the package is loaded, whereas the packages listed in imports are not.
# add dependecies automatically with: use_package()

# devtools::build() to build pkg source version (default)

# continuous integration:
# - automatically runs checks when code changed
# - used with version control
# - runs every time you make an update
# .. use use_travis("my_pkg"), use_github()

# unit-tests:
# a function that works correctly now may not behave as expected in the future if:
# - supporting or connected code could be added or modified
# - a later version of R or packages are used
# - the code is run on new data
# - the code is run on a different operating system
# .. call use_testthat() to setup the test framework -> creates test directory
# - testthat.R contains R code to run the tests
# - see "pkg_notes_testthat.png" for common test functions


# --- Setting up the test structure
# You can set up a test framework in a package using the function use_testthat().
# This will create a tests directory that contains:
# - A script testthat.R.
# - A directory testthat.
# - You save your tests in the tests/testthat/ directory in files with filenames beginning with test-. So, 
# for example, the simutils package has tests named:
# - test-na_counter.R
# - test-sample_from_data.R
# - for group-tests, see "pkg_notes_group_tests.png"

# Executing unit tests:
# With your tests scripts saved in the package structure you can always easily re-run your tests using 
# the test() function in devtools. This function looks for all tests located in the tests/testhat or 
# inst/tests directory with filenames beginning with test- and ending in .R, and executes each of them. 
# As with the other devtools functions, you supply the path to the package as the first argument to the test() 
# function


