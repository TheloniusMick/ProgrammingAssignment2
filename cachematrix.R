## The 2 functions un this file together
## create a "matrix" object - actually a list
## of functions - which uses the <<- operator
## to create and "cache" the inverse of a matrix,
## rather than re-calculate it every time it's
## needed. The "cache us implemented by preserving 
## the inverse in an enveloping environment.
##
##
## function makeCacheMatrix()
## Creates a list of functions to set and get
## a matrix, x,  and its inverse
## The inverse is cacjed using the <<- operator
##
makeCacheMatrix <- function( x = matrix() ) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y
    inv <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  
  list(set = set, get = get, setInverse = setInverse,   
       getInverse = getInverse)
  
}
##
## function cacheSolve()
## Using a list of functions, x, received as an 
## argument, the inverse of a matrix, x, is returned.
## If inverse has already been calculated, i.e., 
## it's Not NULL, it will be retrieved from the 
## cache. Otherwise it will be calculated using "solve()"
##
cacheSolve <- function(x, ...) {         
  ## Return a matrix that is the inverse of a x
  
  inv <- x$getInverse() 
  if(!is.null(inv)) { 
    message("Get cache data.") 
    return(inv) 
  } 
  data <- x$get() 
  inv <- solve(data) 
  x$setInverse(inv) 
  inv