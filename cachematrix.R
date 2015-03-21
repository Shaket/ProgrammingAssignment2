## This pair of functions grab an invertible matrix and return 
## its inverse, caching it for above porpuses. It is necessary to 
## store the result of the first function in a variable to use it 
## later in the 2nd.

## The 1st function creates a list of functions to set and to get 
## the matrix and its inverse values.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setsolve <- function(solve) inv <<- solve
	getsolve <- function () inv
	list (set = set, get = get, setsolve = setsolve,
		getsolve = getsolve)
}

## The 2nd function calculates de inverse of the matrix if it's 
## not already stored in the cache, and returns the result.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getsolve()
	if(!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setsolve(inv)
	inv
}
