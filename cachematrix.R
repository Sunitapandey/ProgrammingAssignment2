## The first function creates a special matrix
## The second function calculates the inverse of the matrix created from first function


## This function creates the special matrix 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL #sets the value of m to NULL, if cacheSolve is not used
	y <- NULL #sets the value of y to NULL, if cacheSolve is not used
## the function is used for setting the value of matrix
	set <- function(y) {
		x <<- y #caches the matrix that was input
		m <<- NULL #sets the value of m(matrix inverse) to NULL, if cacheSolve is used

	}
## getting the value of matrix
	get<-function() x
## setting the value of the inverse of the matrix
## "solve" solves the equation AX=B, for X, giving inverse of the matrix in m
	setinverse<-function(solve) m <<- solve
## getting the value of the inverse of the matrix (m)
	getinverse<-function() m
## creates the list to store the above functions	
	list(set = set, get = get, 
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the above function
## However, it first checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the mean in the cache

cacheSolve <- function(x, ...) {
	m <- x$getinverse() #retrieves the inverse of matrix if calculated already
## checks to see if cacheSolve has been run before	
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
## if not, follows the following steps
## retrieves the value of input matrix by calling the get() function
	data <- x$get()
## runs the set function to cache the input matrix
	x$set(data)
## calculates the inverse of the matrix
	m <- solve(data, ...)
## runs the setinverse() function on the matrix to cache the inverse of matrix
	x$setinverse(m)
	m
}
