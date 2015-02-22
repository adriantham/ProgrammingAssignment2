## Calculates the inverse of a matrix and caches the inverse for potential
## future use

## Creates a special matrix that is a list of functions to 
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, and get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i 
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns inverse of the special matrix created by makeCacheMatrix
## If matrix has been solved before, it will return the cached inverse matrix;
## if not, it will solve the matrix and cache the inverse for future use
cacheSolve <- function(x, ...) {
    i <- x$getInverse() 

    if(!is.null(i)) { #pulls inverse if it already exists
    	message("getting cached data")
    	return(i)
    }
    else { #calculates inverse if it has not been calculated before
    	message("solving for matrix")
	    data <- x$get()
	    i <- solve(data, ...)
	    x$setInverse(i) #store inverse for future use
	    return(i)
	} 
}