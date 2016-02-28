#matrix_test <- matrix(c(2, 0, 0,
#                        0, 2, 0,
#                        0, 0, 2), c(3, 3))
# test <- makeCacheMatrix(matrix_test)
# cacheSolve(test)
#result: 
#     [,1] [,2] [,3]
# [1,]  0.5  0.0  0.0
# [2,]  0.0  0.5  0.0
# [3,]  0.0  0.0  0.5


## the makeCacheMatrix function returns a list of functions.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function(){
		x
	} 

	setInverse <- function(y){ 
		m <<- y
	}

	getInverse <- function(){ 
		m
	}

	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and 
##  the matrix has not changed), then the cachesolve should retrieve the inverse
##  from the cache.
cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)

	m
}