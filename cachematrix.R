## This  pair of functions catch the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
		i<-NULL
		setMatrix<-function(y){
			x<<-y
			i<<-NULL
		}
		getMatrix<-function()x
		setInverse<-function(i) i<<-i
		getInverse<-function() i
		list(setMatrix=setMatrix, getMatrix=getMatrix, 
		setInverse=setInverse,getInverse=getInverse)

}

## This function makes a special matrix where input is a matrix 
## and output is a list of functions.

cacheSolve <- function(x, ...) {
		i<-x$getInverse()
		if(!is.null(i)){
			message("getting cached data")
			return(i)
		}
		data<-x$getMatrix()
		i<-solve(data,...)
		x$setInverse(i)
		i
       
 ## Return a matrix that is the inverse of 'x'
}
