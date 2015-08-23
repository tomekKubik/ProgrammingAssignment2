## The functions enables cached calculation of matrix inversion.

## The function creates an object, which contains a matrix and its inverted version.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inversed) inv <<- inversed
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates inversion of the matrix contained in the object created by makeCacheMatrix() function
## If the inverted matrix has already been caclulated, the existing result is returned. If it has not
## been calculated or the input matrix has been changed, the inversion is calculated by calling the solve
## function and the result is returned and cached.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
        	message('getting cached data')
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
