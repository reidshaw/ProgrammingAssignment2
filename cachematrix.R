## Put comments here that give an overall description of what your
## functions do

## Given an invertible matrix, the following two functions will calculate the inverse or retrieve the inverse of the matrix from the cache.

## Write a short comment describing this function

## Function "makeCacheMatrix" creates a matrix object that can cache its inverse
## "makeCacheMatrix" contains four functions:
## 1. "set" changes the vector stored in the main function
## 2. "get" returns the vector x stored in the main function
## 3. "setinverse" and "getinverse" store and then return the value of the input in a variable called "m"


makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function () x
     setinverse <- function(solve) m <<- solve
     getinverse <- function () m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function

## "cacheSolve" computes the inverse of the matrix returned by the "makeCacheMatrix" function above.
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache.
## If the inverse has not been calculated, then data gets the matrix stored with "makeCacheMatrix", "m" calculates the inverse, and "x$setinverse stores it in the object "m"


cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
