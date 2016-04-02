## Caching of a materix inversion is usually better solution than computing 
## the inversion since it saves us time and makes the computation not as heavy. 


## makeCacheMatrix returns a list by setting the value of the 
## matrix, getting the value of the matrix, setting the value of the matrix inversion
## and getting the value of the matrix inversion
        
        makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(inverse) inv <<- inverse
                getinv <- function() inv
                list(set = set, get = get, setinv = setinv, getinv = getinv)
        }
        
        
## cacheSolve returns the inversion of the matrix. If the inversion is already computed, 
## it returns the result. If not, it calculates the inversion and sends the inverted value
## to the cache 
        
        cacheSolve <- function(x, ...) {
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data)
                x$setinv(inv)
                inv
        }