## These functions find the inverse of an invertible square matrix and caches the result. 
## If the same matrix is called, the inverse read from the cache instead of being
## computer again. 

## makeCacheMatrix takes as an argument an invertible square matrix and outputs a list
## of four functions: set(), get(), setinv() and getinv().

makeCacheMatrix <- function(x = matrix()) {

## im is a variable in the environment of makeCacheMatrix which stores the inverse matrix 
## of 'x'. By default it is set to NULL, indicating that the matirx inverse has not been 
## computed

	im <- NULL 

## set() allows us to set the matrix 'x' for which its inverse is to be computed. 
## 'x' is stored in the environment of makeCacheMatrix. im is then set to NULL indicating
## that the matrix inverse of 'x' has not been computed yet. To implement, type 
## makeCacheMatrix$set('x') to the console to set the matrix whose inverse is to be computed. 

	set <- function(y) { 
		x <<- y 
		im <<- NULL 
	} 		

## get() recalls the matrix 'x' from the environment of makeCacheMatrix

	get <- function() x

## setinv() assigns its argument (which is supposed to be the inverse of 'x') 
## to 'im'. 'im' is not in the environment of setinv() so
## the <<- assignment operator is used to assign the argument to 'im'. 

	setinv <- function(inv) im <<- inv

## getinv() returns the inverse matrix of 'x'

	getinv <- function() im 	

## this creates the list of the four functions used. 

	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve reads the list created by makeCacheMatrix into its environment and assigns
## 'im' either the value of NULL if a new inverse is to be computed, or the inverse of a 
## previously computed inverse. This is then checked against a conditional. If 'im' is 
## not NULL, then the function terminates returning a message and the cached inverse. 
## Otherwise, the program proceeds to compute the inverse of 'x' and stores it in the 
## cache (environment of makeCacheMatrix). 

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'

	im <- mat$getinv()
	if(!is.null(im)){ 
		message("getting cached data")
		return(im)	
	}	
	dat <- mat$get()
	im <- solve(dat,...) 
	mat$setinv(im)
	im
}
