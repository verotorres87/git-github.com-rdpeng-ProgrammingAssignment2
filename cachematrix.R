## makeCacheMatrix function creates a special "matrix" 
object that can cache its inverse.
## functions do set the value of the vector
get the value of the vector
set the value of the mean
get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
		set <- function(y) {

     		x <<- y

        	inv<<-NULL
        
}
        
	get <- function() x
        
	setinv <- function() inv <<- solve(X)
        
	getinv <- function() inv        
	list(set = set, 
	     get = get,
            	
 	     setinv = setinv,
             
	     getinv = getinv)
}


## cacheSolve function computes the inverse of the 
special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        x <- mean(data, ...)
        x$setinv(inv)
        inv
}
