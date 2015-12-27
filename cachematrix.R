
##The first function, makeCacheVector creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of that matricx
##get the value of the inverse of that matricx


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


##The following function calculates the matrix's inverse. 
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverted matrix from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the matrix and sets it's value
##in the cache via the setinv function.

##Assumption is that inversion of the matrix is possible.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
  }
  
}
