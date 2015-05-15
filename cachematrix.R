##The first function, makeCacheMatrix creates a special "mstrix", which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	z <- NULL
	set <- function(y){
		x <<- y
		z <<- NULL
	}

	get <- function(){
		x
	}

	setInverse <- function(inverse){
		z <<- inverse
	}

	getInverse <- function(){
		z
	}
	list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    z <- x$getInverse()
    if(!is.null(z)){
    	message("caching matrix data");
    	return(z)
    }
    data <- x$get()
    z <- solve(data,...)
    x$setInverse(z)
    z
}