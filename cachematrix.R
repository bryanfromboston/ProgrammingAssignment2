## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix(x):
## makeCacheMatrix creates an "object" that is, in reality, a list of functions that:
## 1. sets the value of a matrix
## 2. gets the value of a matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix

## param x: A matrix

makeCacheMatrix <- function(x = matrix()) {
    ## The inverse i is, by default, NULL
    i <- NULL
    
    ## set() sets the value of the matrix to y, passing it to the outside environment
    ## and also sets the inverse of the matrix back to NULL (since it's a new matrix)
    set <- function(y) {
        # Note: x and i are in the environment above the scope of this function
        x <<- y 
        i <<- NULL
    }
    
    ## get() gets the matrix, stored in x when set()
    get <- function() x
    
    ## setInverse() sets the inverse of the matix x, to i
    setInverse <- function(inverse) i <<- inverse
 
    ## getInverse() gets the value of the inverse of the matrix x, in i
    getInverse <- function() i
    
    ## we return a list containing the list of "methods" set to names
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## CacheSolve(x, ...)
## CachSolve solves for the inverse of a matrix using a CacheMatrix "object"
## If the inverse of the matrix has been cached, it will return the cached matrix,
## otherwise, it will calculate the inverse, set it into the cache, and then return that result

## param x: A special "CacheMatrix" "object."
## param ...: Any other parameters passed to the function will be passed on to solve

cacheSolve <- function(x, ...) {
    ## First, we we simply get the inverse of x and store to i
    i <- x$getInverse()
    ## If the inverse is not NULL, we simply return the cached inverse
    if (!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    ## If the inverse wasn't found in the cache, we get the actual matrix stored in x:
    data <- x$get()
    ## Calculate the inverse, passing along whatever other parameters were passed to this function
    i <- solve(data, ...)
    ## Set the cache with this inverse
    x$setInverse(i)
    ## And then return the inverse
    i  
}
