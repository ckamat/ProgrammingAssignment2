# The following two functions are used to cache the inverse of a matrix.

# The makeCacheMatrix function provides a the following list of functions for a given matrix (provided as a parameter to the function) 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() return(x)
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() return(inv)
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix to be inversed is a square matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## EXAMPLE 1 :
## > x <- rbind(c(1, -1/4), c(-1/4, 1))
## > m <- makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## EXAMPLE 2:
## y = matrix(sample(1:16), 4, 4)
## m <- makeCacheMatrix(y)
## m$get()
## [,1] [,2] [,3] [,4]
## [1,]   14   15    7   10
## [2,]   13    1   11    6
## [3,]    4    5   16    2
## [4,]    3    8   12    9
##
## > cacheSolve(m)
## [,1]         [,2]         [,3]        [,4]
## [1,]  0.03605584  0.054469854  0.004870805 -0.07745768
## [2,]  0.07306207 -0.087318087  0.054351054 -0.03504604
## [3,] -0.02667063  0.007484407  0.060647461  0.01116721
## [4,] -0.04140184  0.049480249 -0.130798931  0.15319275


