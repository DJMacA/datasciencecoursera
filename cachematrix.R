## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The pair of functions below create a special matrix calculate the inverse of that matrix 
## and once calculated they store the result in a cache for retrieval to avoid repeating 
## the costly inversion computation.


## makeCacheMatrix creates a special matrix object, stores it and cache's the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {

  ## Initialize inv
    inv <- NULL
    
    ## Assign the input argument to the x object in the parent environment, and
    ## Assign the value of NULL to the inv object in the parent environment.
    
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## Define the functions that will be used to compute and retrieve the inverse in cacheSolve
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    ## Assigns each functions as an element within a list(), and returns it to the parent environment.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix. If the inverse of the matrix 
## has already been calculated cacheSolve retrieves the calculated inverse of this matrix from the cache.

cacheSolve <- function(x, ...) {
  
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    ## If the inverse has already been computed inform the user that the inverse is being retrieved from the cache
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ## If not the inverse has not already been computed retrieve the stored special matrix 
    data <- x$get()
    
    ## Calculate the inverse of the special matrix
    inv <- solve(data, ...)
    
    ## Output the computed inverse of the special matrix
    x$setinv(inv)
    inv

}
