## These 2 functions cache and compute the inverse of a matrix

## The first function, makeCacheMatrix creates a special "matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function computes the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data.")
return(inv)
}
data <- x$get()
inv <- solve(data)
x$setinverse(inv)
inv
}
