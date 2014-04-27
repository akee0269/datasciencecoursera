## The functions below create a list of functions, check if inverse of a given matrix
## has already been calculated. If already calculated it returns the value stored
## If not it calculates the inverse value, prints it and stores it for future use

## This function creates a special vector (basically a list) of four functions
##set function takes a matrix as an argument and sets it to x
##get function returns value of x
##setInverse takes an inverse matrix as an argument (sol) and stores it in a global variable m
##getInverse returns the value stored in m (by setInverse)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(sol) m <<- sol
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function first checks whether m already has an inverse of matrix 'x'
## If present it first returns the message and then the stored value in m
## If not, it calculates the inverse using solve function, stores the inverse in m,
## sets the calculated value in 'global' m (in makeCacheMAtrix function)
## and then prints out the inverse in m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
