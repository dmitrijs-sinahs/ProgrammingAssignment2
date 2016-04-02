## Inverse matrix computation and its results cashing.

## The function creates a special "matrix" object, which is really a list containing a function to
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
}


## The function calculates the inverse matrix, if it haven't been calculated already.
## Otherwise, the function gets the inversematrix from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
