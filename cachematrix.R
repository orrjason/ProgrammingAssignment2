## These 2 functions cache and compute the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" that caches its inverse


makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y) {
x <<- y;
inverse <<- NULL;
}
get <- function() return(x);
setinv <- function(inv) inverse <<- inv;
getinv <- function() return(inverse);
return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## The following function computes the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
inverse <- x$getinv()
if(!is.null(inverse)) {
message("Getting cached data...")
return(inverse)
}
data <- x$get()
invserse <- solve(data, ...)
x$setinv(inverse)
return(inverse)
}
