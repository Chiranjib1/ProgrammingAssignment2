## These functions calculate and cache the inverse of a square matrix 
## Write a short comment describing this function
## function returns a list of function that will  set, get,setinvesrse and getinverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Write a short comment describing this function
## calculates and returns the invesrse of a matrix if it is not cached but returns the cached invesrse if it is available
##cacheSolve <- function(x, ...) {   THIS WAS WHAT CAME WITH IT
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 		i <- x$getinverse()
        		if(!is.null(i)) {
                		message("getting cached data for inverse")
                		return(i)
        	
        				}
        		data <- x$get()
        		if(det(data)!=0) {
        		i <- solve(data)
        		x$setinverse(i)
        		i
        		}
        		else print("Singular matrix- can't be inversed")
}
