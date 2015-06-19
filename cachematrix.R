## Two functions to inverse a matrix and cache the inverted matrix


## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	
	##Initializing the inverse
	i <- NULL
	
	##Set the matrix
	set <- function(matrix){
		m <<- matrix
		i <<- NULL
	}
	
	##Get the matrix
	get <- function(){
		m
	}
	
	##Set the inverse
	setInverse <- function(inverse){
		i <<- inverse
	}
	
	##Return the inverse
	getInverse <- function(){
		i
	}
	
	##Return a list of the methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Compute the inverse of the matrix returned by makeCacheMatrix method.
## If the inverse is already computed, it is retrieved from the cache.

cacheSolve <- function(x, ...) {
	
	## Inverse of x
	m <- x$getInverse()
	
	## Check if inverse is already set and returns it
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}
	
	##Get the matrix
	data <- x$get()
	
	## Calculate the inverse
	m <- solve(data) %*% data
	
	##Set the inverse
	x$setInverse(m)
	
	## Return the matrix m
	m
}
