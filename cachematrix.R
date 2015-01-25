## The following two functions create an special object
## of a matrixand caches its inverse.

## makeCacheMatrix returns a list of functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)
}


## cacheSolve returns a matrix that is the inverse of 'x'.
## Will return cached value if calculation was done before.

cacheSolve <- function(x, ...) {
        ## Return cached value if not empty
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## Fetch data and perform calculation if not cached
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
