## What follows are 2 functions, largely modelled after the assignment 2 examples.
## The first creates a matrix. The second solves for the inverse of the matrix.
## This inverted matrix is cached, and quickly retrieved if the same matrix
## is asked to be inverted consecutively.

## This first function creates the matrix, but performs additional tasks when 
## called by the cacheSolve function. 
makeCacheMatrix <- function(x = matrix()) {  
  
  ## i will store our inverted matrix. It resets to NULL everytime a new matrix 
  ## is created.  
  i <- NULL 
            
  ## This function also creates our matrix object. Not called by cacheSolve, 
  ## used for debugging purposes.
  set <- function(y) { 
    ## Saves inputted matrix.                  
    x <<- y             
    ##Resets our cached inverted matrix to NULL.
    i <<- NULL 
  }
  
  ## Returns our inputted matrix.
  get <- function() x 
  
  ## This stores the inverted matrix. It is called by cacheSolve when the inputted
  ## matrix is new.
  setinverse <- function(solve) i <<- solve 
  
  ## This function is called by cacheSolve when consecutive inverted requests for 
  ## the same matrix is made and it returns the cached inverted matrix.
  getinverse <- function() i
  
  ## A list of internal functions created each time makeCacheMatrix is called. 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverted matrix either by solving for it, or by
## accessing the cached inverted matrix if the same matrix is asked to be 
## inverted consecutively.
cacheSolve <- function(x, ...) {
  
  ## This calls on the getinverse function within the makeCacheMatrix function
  ## and assigns whatever is retrieved to i.
  i <- x$getinverse()
  
  ## This checks if there is a cached inverted matrix. If there is a cached
  ## inverted matrix, a message is printed, the inverted matrix is supplied, and
  ## the function is exited.
  if(!is.null(i)) {
    message("Obtaining cached data.")
    return(i)
  }
  
  ## If there is no cached inverted matrix, the inputted matrix is retrieved from
  ## makeCacheMatrix and assigned to data.
  data <- x$get()
  
  ## The inverted matrix is solved for.
  i <- solve(data, ...)
  
  ## The inverted matrix is cached through the makeCacheMatrix function.
  x$setinverse(i)
  
  ## The inverted matrix is supplied.
  i
}
