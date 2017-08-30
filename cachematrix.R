## Below are two functions that will invert a given matrix and
## cache the inverse

## Creates a list of functions to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    ivm <- NULL
    set <- function(y) {
        x <<- y
        ivm <<- NULL
    }
    get <- function() x
    setivm <- function(inverse) ivm <<- inverse
    getivm <- function() ivm
    list(set = set, get = get,
         setivm = setivm,
         getivm = getivm)
}


## Checks if the inverse is already calculated, if not it is calculated 
## and the value of the inverse is cached

cacheSolve <- function(x, ...) {
    ivm <- x$getivm()
    if(!is.null(ivm)) {
        message("getting cached matrix")
        return(ivm)
    }
    m <- x$get()
    ivm <- solve(m, ...)
    x$setivm(ivm)
    ivm
}
