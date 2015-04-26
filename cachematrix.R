##  The <<- operator can be used to assign a value to an object in an environment that is different from the current environment. 
## The functions below are used to create a special object that stores a matrix and caches its inverse.
## Creating a cache saves time so the inverse (of the same matrix) does not need to be calculated many times in the program.  
## 

## 
## This function creates a list containing functions to set the matrix, 
## get the matrix, set the inverse and get the inverse. It creates a special "matrix" object. 
## 

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


## This function calculates the inverse of the special "matrix" object returned from the makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated and cached, if so
## it uses the cached inverse, thereby skipping the expensive computation. If not, it calculates the inverse,
## and then caches it.  

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
