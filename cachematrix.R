##       The bellow functions are dedicated to my assignement, 
## they intent to compute the inverse of an invertible squared matrix

## This function prepare a matrix cache for the matrix to invert or it's final invert

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatInv <- function(MatInv) m <<- MatInv
        getMatInv <- function() m
        list(set = set, get = get,
             setMatInv = setMatInv,
             getMatInv = getMatInv)
}



## This function first try to get the inverted square matrix from "makeCacheMatrix",
## Then if the inverted matrix not found, compute it using the "solve()" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatInv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
		data <- x$get()
        m <- solve(data)
        x$setMatInv(m)
        m
}
