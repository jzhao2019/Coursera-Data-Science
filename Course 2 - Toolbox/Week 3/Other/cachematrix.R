## These pair of functions are created to cache the inverse of a matrix



## MakeCacheMatrix is a function that returns a list a functions with an argument of x as a matrix. The four functions are 
## 1. "set": set a matrix in the makeCacheMatrix environment.
## 2. "get": return a matrix passed through the function argument "x" or set by the "set" function
## 3. "set_cache": set the inverse of the matrix to the object "inv" for future use. This function is called by cacheSolve
## 4. "get_Cache": return the stored the inverse matrix cached in "inv"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(matr) {
                x <<- matr
                inv <- NULL
        }
        get <- function() x
        set_cache <- function(m) inv <<- m   #cache won't be set if use "inv <- m"
        get_inv <- function() inv
        list(set = set, get = get,
             set_cache = set_cache,
             get_inv = get_inv)
}


## CacheSolve call the function list (makeCacheMatrix), check if the cache is Null. 
## if it is, inverse the matrix, cache it and return the inverse matrix.
## if not, print "getting cached data" and return the inversed matrix.

cacheSolve <- function(funclist, ...) {
        ## Return a matrix that is the inverse of 'x'
        in_ma<-funclist$get_inv()
        if (!is.null(in_ma)) {
                message("getting cached data")
                return(in_ma)
        }
        in_ma<-solve(funclist$get())
        funclist$set_cache(in_ma)
        in_ma
        
}
