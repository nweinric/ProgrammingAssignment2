## I try to invert a matrix. This is the second version


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.?

cacheinvers <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        message("Berechnung der Inversen")
        m
}
# Mit diesen testdaten hat das Beispiel funktioniert
#CR <- c(1,2,3,4)
#matrix (CR,2,2) -> Norbert

#meineMatrix <- makeCacheMatrix(Norbert)
#meineMatrix$get()
#meineMatrix$set(matrix(CR, 1, 4))
#meineMatrix$get()
#cacheinvers(meineMatrix)
#cacheinvers(meineMatrix)
