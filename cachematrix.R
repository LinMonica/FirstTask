## My function can cache the inverse of a matrix, which is benefit for the matrix computing.

## This function create a special "matrix" that can be cached its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	
	setinv<-function(inverse) m<<-inverse
	getinv<-function() m
	list(set=set,
	get=get,
	setinv=setinv,
	getinv=getinv)
}


## This function computes the inverse of the special "matrix"returned by above function.

cacheSolve <- function(x, ...) {

	m<-x$getinv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m<-solve(data, ...)
	x$setinv(m)
	m   
}
