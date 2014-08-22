# Creates as list containing a function to
# 1. set the value of matrix
# 2. get the value of matrix
# 3. set the value of inverse of matrix
# 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = numeric()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# Returns a matrix which is inverse of matrix in argument
# Assumes matrix is always invertible
# Uses makeCacheMatrix to return the cached inverse otherwise calc inverse

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
  	data <- x$get()
  	s <- solve(data, ...)
  	x$setsolve(s)
  	s
}
