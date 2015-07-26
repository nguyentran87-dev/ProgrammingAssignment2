# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <- y
        invert <<- null
    }
    get <- function() {
        x
    }
    set_invert <- function(inv) {
        invert <<- inv
    }
    get_invert <- function() {
        invert
    }
    list(x = x, get = get, set = set, set_invert = set_invert, get_invert = get_invert)
}


# The following function calculates the inverse of the matrix created with the
# above function. It first checks if the inverse has already been computed.
# If so, it gets the result from the cache and skips the computation.
# If not, it computes the inverse, sets the value in the cache via
# set_inverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    invert <- x$get_invert()
    if (is.null(invert)) {
        message("calculate inverse of the matrix for the first time")
        data <- x$get()
        invert <- solve(data)
        x$set_invert(invert)
    } else {
        message("get inverse from cached data")
    }
    return(invert)
}
