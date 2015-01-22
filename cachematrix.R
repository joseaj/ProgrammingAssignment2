
## makeCacheMatrix(x) creates a special "matrix" object
## that can cache ist inverse
## x is a numeric matrix
## defaults to: x = matrix()
##
## caches the matrix "x" and its "inverse"
## > z <- makeCacheMatrix(x)
## returns a list of the form:
## z$get()        => returns the stored matrix x
## z$set(y)       => changes the stored matrix
##                   and returns the new cached matrix
## z$setinverse() => calculates the inverse of matrix x
##                   and caches the inverse
## z$getinverse() => returns the cached inverse
##
## example of use:
##
## > a <- matrix(c(1,2,3,1,2,1,-2,3,4), ncol=3)
## > b <- makeCacheMatrix(a)
## > b$get() %*% cacheSolve(b) ## returns the 3x3 identity matrix
## > b$get() %*% cacheSolve(b) ## returns the same 3x3 identity matrix
## > "getting cached data"
## > d <- matrix(c(1,2,3,3), ncol=2)
## > b$set(d)
## > b$get() %*% cacheSolve(b) ## returns the 2x2 identity matrix

makeCacheMatrix <- function(x = matrix()) {
        (function() {
                i <- NULL
                list(
                        get = function() {
                                x
                        },
                        set = function(y = matrix()) {
                                x <<- y
                                i <<- NULL
                                x
                        },
                        getinverse = function() {
                                i
                        },
                        setinverse = function() {
                                i <<- solve(x)
                                i
                        }
                )
        })()
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix()
## If the inverse has already been caculated (and the matrix
## has not changed), then cacheSolve(x) retrieves the inverse
## from the cache
## by accessing the cached x$getinverse() and, if needed, x$setinverse() 

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (is.null(i)) {
                i <- x$setinverse()
        } else {
                print("getting cached data")
        }
        i
}
