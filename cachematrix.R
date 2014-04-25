
## makeCacheMatrix function takes matrix as an input and returns a special list
# with function objects. Function objects in general are used to manipulate
# (e.g get, set etc) on the input matrix  and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   #cache variable used to store the inverse of matrix. Default NULL
  
  #Set the internal variable with the original input marix 
  set <- function(y) {  
    x <<- y
    inv <<- NULL
  }
  
  #Return the original matrix
  get <- function() x
  
  #Cache the matrix inverse 
  setinv <- function(inverse) inv <<- inverse
  
  #Return the  cached matrix inverse 
  getinv <- function() inv
  
  #Prepare and return the special list with function objects
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function take the  list returned by makeCacheMatrix as input.
## It returns the inv of matrix if value is already cached and available. otherwise 
## this function computes the inverse of matrix , stores the value in the cache 
## and returns the inverse of matrix. 

cacheSolve <- function(x, ...) {
    
  inv <- x$getinv()
  
  ## Check if cached already available 
  if(!is.null(inv)) {
    message("Getting  matrix inv from cached data")
  
    ## Return from cache - a matrix that is the inverse of 'x'
    return(inv)
  }
  
  ##Compute the inverse of matrix if cached value not available
  mat <- x$get()
  inv <- solve(mat)
  
  ##Cached the computed inverse 
  x$setinv(inv)
  
  ## Return the computed the inverse of matrix
  inv
}
