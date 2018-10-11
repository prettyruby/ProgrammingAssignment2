## makeCacheMatrix() creates an R object that stores a matrix and its inverse matrix.
## First, it initializes objects x (as a matrix) and m.
## Second, it defines the functions for objects of type makeCacheMatrix.
## get and getsolve are program modules in makeCacheMatrix() that retrieve data within an object
## set and setsolve are program modules in makeCacheMatrix() that set the data values within an object
## Third, the code assigns each of these functions (set, get, setsolve, getsolve) as an element
## within a list() and returns it to the parent environment. This list() is a fully formed object of
## type makeCacheMatrix() to be used by downstream R code and also allows us to use $ to access
## functions by name.

## cacheSolve() is required to populate and/or retrieve the inverse matrix from an object of type makeCacheMatrix()
## cacheSolve() attempts to retrieve an inverse matrix from the object passed in as the argument. 
## Since makeCacheMatrix() sets the cached inverse matrix to NULL whenever a new vector is set into the object,
## if the value here is not equal to NULL, we have a valid cached inverse matrix and can return it to the
## parent environment. 

## You can use myMatrix$set() to reset value with a new matrix instead of reinitializing.

## makeCacheMatrix() builds a set of functions that stores a matrix and
## its inverse matrix and returns the functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the inverse
## matrix from the cached value that is stored in the makeCacheMatrix() object's environment.
## It checks to see if the inverse matrix has already been calculated.
## If so, it gets the inverse matrix of the data from the cache
## and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the
## value of the inverse matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Return a matrix that is the inverse of 'x'
## You have to treat makeCacheMatrix() as an object:
## myMatrix <- makeCacheMatrix(matrix())
## Define your matrix(), for example
## matrix(c(2, -3, 4, -7), nrow = 2, ncol = 2, byrow = TRUE)

## To solve for the inverse matrix:

## myMatrix <- makeCacheMatrix(matrix(c(2, -3, 4, -7), nrow = 2, ncol = 2, byrow = TRUE))
## cacheSolve(myMatrix)
