## These functions calculate the inverse of a given matrix, store the result in the cache (i.e. parent frame),
## and return it to the caller. These functions do not handle cases where a matrix is singular
## Usage example:
##      p <- matrix(rnorm(1000000), c(1000,1000))
##      q <- makeCacheMatrix(p)
##      cacheSolve(q)
## While the following usage is valid, it does not take the advantage of the caching mechanism.
##      cacheSolve(makeCacheMatrix(matrix(rnorm(1000000), c(1000,1000))))

## makeCacheMatrix: take a given matrix and return a list of functions.
## These functions are:
##      set: store the original matrix and set the inverse to NULL
##      get: return the original matrix
##      setinverse: store the calculated inverse in cache
##      getinverse: return the stored inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse of the matrix to NULL
    x_inverse <- NULL

    # set the original matrix and initialize the inverse matrix, x_inverse, to NULL
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    
    # return the original matrix
    get <- function() x
    
    # store the calculated inverse into cache, x_inverse
    setinverse <- function(inversed) x_inverse <<- inversed
    
    # return the caclcualted inverse. If the inverse hasn't been calculated, NULL is returned
    getinverse <- function() x_inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Calculate the inverse for a given matrix constructed by makeCacheMatrix function. The inverse is calculated
## only once for a given matrix and stored in cache for subsequent requests

cacheSolve <- function(x, ...) {
    inversed <- x$getinverse()
    
    # check and return the inverse immediately if it is available
    if(!is.null(inversed)) {
        message("getting cached inverse matrix")
        return(inversed)
    }
    
    # calculate the inverse if it is not available.
    data <- x$get()
    inversed <- solve(data, ...)
    x$setinverse(inversed)
    
    inversed
}
