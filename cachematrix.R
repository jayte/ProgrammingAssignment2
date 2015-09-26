## The following functions, when used together, will allow the developer to 
## cache the inverse of a matrix to improve performance.

## In order to cache a matrix the developer needs to pass their matrix into this
## function on its construct or through its set method.  Once built the
## developer can use the get method to retrieve the matrix, the set method to
## define the matrix.  The getSolve and setSolve methods should not be used,
## instead the developer should use the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) 
{
	#defines a variable accessable in this scopre to store the cache inverse
	matrixSolve <- NULL
	get  		<- function()
	{
		x
	}
	set			<- function(matrix)
	{
		# resets the matrix and clears any previously store cache
		x 			<<- matrix
		matrixSolve <<- NULL
	}
	getSolve  	<- function()
	{
		matrixSolve
	}
	setSolve 	<- function(theSolve)
	{
		# stores the value to be cached
		matrixSolve <<- theSolve
	}
	
	#Returns the following vector of functions
	list(
		  get = get
		, set = set
		, getSolve = getSolve
		, setSolve = setSolve
	)
}


## This function extends the functionality of solve.  This function stores the
## resulting solved / inverse value with the matrix when the developer passes
## a makeCacheMatrix matrix.  The inverse will be returned

## ... allows the developer to pass additional arguments into this function
## which the wish to be passed to solve.  The problem is that IF the developer
## wants to process solve with different arguments, this function will only
## cache the latest inverse value processed.

cacheSolve <- function(x, ...)
{
	# Attempts to see if this matrix (x) has an inverse
	matrixSolve <- x$getSolve()
	if (!is.null(matrixSolve))
	{
		# If the matrix (x) has a cached inverse it returns that value instead of
		# reprocessing the inverse using the solve function.
		print("Retrieving cached matrix solve -- no wait necessary (:")
		return(matrixSolve)
	}
	
	# In the case the matrix (x) does not have a stored inverse we need to
	# process the inverse and then store it with the matrix.
	
	# Additional solve arguments the developer passed into this function will
	# be passed into solve using ...
	matrixSolve <- solve(x$get(), ...)
	x$setSolve(matrixSolve)
	
	matrixSolve
}
