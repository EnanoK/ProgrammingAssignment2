## makeCacheMatrix - Creates a matrix in the cache
## cacheSolve - caches the inverse of a matrix

## Creates a special "matrix", which is really a list containing a function to
#   1.  set the value of the matrix
#   2.  get the value of the matrix
#   3.  set the value of the inverse
#   4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInv <- function(inv2) inv <<- inv2
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Calculates the inverse of a matrix and saves it to the cache.
## If the inverse already exists it returns the cached inverse

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
