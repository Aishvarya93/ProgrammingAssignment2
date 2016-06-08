# The following function returns a special vector ( actually a list), that contains a function
# to set a value ,get the value, set the mean and get the mean

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
	set<-function(y) {
	x<<-y
	m<<-NULL
	}
  		get<-function() x
        	setmatrix<-function(solve) m <<-solve
        	getmatrix<-function() m
      
	list(set = set, get = get,setmatrix = setmatrix,getmatrix = getmatrix)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then cacheSolve will retrieve the inverse from the cache, denoted by variable 'm'


cacheSolve<-function(x=matrix(), ...) {
        m<-x$getmatrix()
        			if(!is.null(m)) {
                		message("getting cached data")
                		return(m)
        			}
        required_matrix<- x$get()
        m <- solve(required_matrix, ...)
        x$setmatrix(m)
        m
}