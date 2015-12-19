## These two next functions provide a mean to preserve
##  in memory (cache) the cpu expensive inverse of a
##  matrix computation so that if it is needed again
##  there is no need to repeat that computation.

## makeCacheMatrix: Creates a "special" matrix
##  whose inverse, once computed, is kept in memory
##  i.e. is cached.

makeCacheMatrix <- function(x = matrix()) {
		stim <- NULL
		set <- function(y) {
				x <<- y
				stim <<- NULL
		}
		get <- function() x
		setim <- function(im) stim <<- im
		getim <- function() stim
		list(set = set, get = get,
			setim = setim,
			getim = getim)
}


## cacheSolve: Returns the inverse of a matrix created
##  with makeCacheMatrix. The inverse matrix is computed
##  when first called and stored in the environment of
##  the "matrix" returned by makeCacheMatrix. Further
##  calls just return the stored inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		im <- x$getim()
		if(!is.null(im)) {
				message("getting cached inverse matrix")
				return(im)
		}
		im <- solve(x$get(), ...)
		x$setim(im)
		im
}
