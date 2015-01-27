## Caching the computed inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(current_matrix = matrix())
{
    current_inverse <- NULL

    getMatrix <- function() {
        current_matrix
    }
    getInverse <- function() {
        current_inverse
    }
    setMatrix <- function(new_matrix) {
        if (!matequal(current_matrix, new_matrix)) {
            current_matrix <<- new_matrix
            current_inverse <<- NULL
        }
    }
    setInverse <- function(new_inverse) {
        current_inverse <<- new_inverse
    }

    list(getMatrix = getMatrix, getInverse = getInverse,
         setMatrix = setMatrix, setInverse = setInverse)
}


# This function computes the inverse of the special "matrix" returned by
# `makeCacheMatrix` above. If the inverse has already been calculated (and the
# matrix has not changed), then `cacheSolve` retrieves the inverse from the
# cache.
cacheSolve <- function(cacheMatrix, ...)
{
    inverse <- cacheMatrix$getInverse()
    if (is.null(inverse)) {
        inverse <- solve(cacheMatrix$getMatrix(), ...)
        cacheMatrix$setInverse(inverse)
    }
    else {
        message("getting cached data")
    }

    inverse
}

# Compare two matrices for equality.
# Source: https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
matequal <- function(x, y)
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
