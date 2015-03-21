## This program calculates the matrix inverse. It creates a cache to save time so 
## the inverse does not need to be calculated many times.  
## 

## 
## This function creates a list containing functions to set the matrix, 
## get the matrix, set the inverse and get the inverse. When there is no cache m is 
## NULL

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <-function(solve) m <<-solve
	getinverse <- function()m
	list(set=set,get=get,
	     setinverse=setinverse,
	     getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
