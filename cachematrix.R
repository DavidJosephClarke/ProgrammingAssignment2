# This is my attempt to solve the Programming Assignment 2 of the Coursera R Programming course.

# makeCacheMatrix creates a list of functions: set, get, setinverse, and getinverse.
# The set and get functions set and get the value of the matrix respectively.
# The setinverse and getinverse do the same with the value of the inverse
# cacheSolve will resolve this list of functions.
 

makeCacheMatrix <- function(x = numeric()) {
  
  m <- NULL
  
  # store matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # returns stored matrix
  get <- function() x
  
  # cache given argument
  setinverse <- function(solve) m <<- solve
  
  # get the cached value
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse
       getinverse = getinverse)
}


# cacheSolve detects whether the inverse of the matrix has already been calculated
# If so, it returns the inverse, otherwise it uses solve() to calculate it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # Checking if already solved, in which case get cached data
  
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  }
  
  # Otherwise solve it
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}