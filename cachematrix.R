## utility functions to hold a matrix and cache the associated inverse
## makeCacheMatrix creates the structure, cacheSolve calculates
## and caches the inverse

## returns a list structure to hold functions to 
## get and set a matrix value
## along with it's associated inverse
## get,set for getting and setting the matrix
## getinverse, setinverse for getting and setting the inverse
makeCacheMatrix <- function(my_matrix = matrix(), my_matrix_inverse = NULL) {
    list(
        get = 
            function() my_matrix,
        set = 
            function(in_matrix) {
                my_matrix <<- in_matrix
                my_matrix_inverse <<- NULL
            },
        setinverse =
            function(in_matrix_inverse) {
                # let's hope this is the inverse of my_matrix
                # set and return the value just set
                (my_matrix_inverse <<- in_matrix_inverse)
            },
        getinverse = 
            function() my_matrix_inverse
    )
}

## uses the structure returned from makeCacheMatrix to 
## query and return an already cached inverse value
## for a matrix
## if the inverse has not been cached, then it is 
## calculated, cached in the structure and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(is.null(inv)) {
        inv <- x$setinverse(solve(x$get(), ...))
    } else {
        message("getting cached data")
    }
    
    inv
}
