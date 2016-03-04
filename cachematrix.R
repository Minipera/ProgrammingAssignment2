makeCacheMatrix <- function(x = matrix()) {

    # initialize the inverse matrix value
    inversed <- NULL
    
    # set the value of the matrix
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    setInverse <- function(newInversed) inversed <<- newInversed
    getInverse <- function() inversed
    
    # return a list of all the above functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    # check if the inverse is cached,
    inversed <- x$getInverse()
    if(!is.null(inversed)) {
        message("getting cached inverse")
        return(inversed)
    }
    
    data <- x$get()
    inversed <- solve(data, ...)
    # Put in cache
    x$setInverse(inversed)
    inversed
}