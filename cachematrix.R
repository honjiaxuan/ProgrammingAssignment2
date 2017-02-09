## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## inv is the inverse matrix of m and is initialiazed as null
	  set <- function(y) {   ## the set function may be used to change the matrix without re-initializing makeCacheMatrix
                x <<- y   ## this assigns the value of y to m in the parent environment  
		    inv <<- NULL	    ## this assigns null to m in the parent environment
	  }	
	  get <- function() x   ## this gets m from the parent environment
        setinverse <- function(inverse) inv <<- inverse  ## this assigns the value inverse to inv in the parent environment
	  getinverse <- function() inv  ## this gets the inverse matrix
	  list(set = set, get = get,
 		 setinverse = setinverse,
		 getinverse = getinverse)  ## this returns a list of named functions
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()  ## it gets the inverse matrix from makeCacheMatri
	  if(!is.null(inv)) {
		    message("getting cached data")
		    return(inv)
	  }  ## this checks if inv (the inverse matrix) is null or not. If not, it return the cached inv
	  mat <- x$get()  ## if inv is null, then it gets mat from makeCacheMatrix
	  inv <-solve(mat, ...)  ## it calculates the inverse of matrix and assigns it to inv
	  x$setinverse(inv) ## in the setinverse function within makeCacheMatrix the inverse matrix 
	  		    ## is assigned to i in the parent environment
	  inv
}
