## Matrix inversion is a costly computation and their may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly
## These two functions cache the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" object that can
## cache its inverse. It is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m<<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above
## If the inverse has already been calculated and the matrix has not changed
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
