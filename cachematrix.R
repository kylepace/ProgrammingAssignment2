## Create a memory cacheable matrix with makeCacheMatrix and get inverse of said matrix using cacheSolve

## Takes a square matrix and outputs list with dims set,get setinverse and getinverse, providing
## access to the underlying datatype.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Takes a cacheable matrix (see above) and stores inverse to avoid future computation

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)) {
		message("Returning data from the cache")
		return(m)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setinverse(inv)
	inv
}
