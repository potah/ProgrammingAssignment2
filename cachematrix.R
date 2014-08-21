## utility functions to hold a matrix and cache the associated inverse
## makeCacheMatrix creates the structure, cacheSolve calculates
## and caches the inverse

## returns a list structure to hold functions to 
## get and set a matrix value
## along with it's associated inverse
## get,set for getting and setting the matrix
## getinverse, setinverse for getting and setting the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        get = get,
        set = set,
        setinverse = setinverse,
        getinverse = getinverse)
}


## uses the structure returned from makeCacheMatrix to 
## query and return an already cached inverse value
## for a matrix
## if the inverse has not been cached, then it is 
## calculated, cached in the structure and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
