## This function sets the value of a special matrix, 
# gets the value of the matrix, 
# sets the value of the inverse of the matrix,
# gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        mxinv <- NULL
        set <- function(y) {
                x <<- y
                mxinv <<- NULL
        }
        get <- function () x
        setinv <- function(inverse) mxinv <<- inverse
        getinv <- function() mxinv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function computes the inverse of the special matrix returned by makeCacheMatrix 
# or retrieves the inverse from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mxinv <- x$getinv()
        if(!is.null(mxinv)) {
                message("getting cached data")
                return(mxinv)
        }
        data <- x$get()
        mxinv <- solve(data, ...)
        x$setinv(mxinv)
        mxinv
}
