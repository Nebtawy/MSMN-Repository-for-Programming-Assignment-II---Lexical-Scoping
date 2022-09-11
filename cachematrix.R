## The two functions below, makeCacheMatrix and cacheSolve instantiate and 
## allow the user to interact with an in-memory, cached version of the inverse
## of the input matrix passed to makeCacheMatrix. MakeCacheMatrix handles creating 
## the cached matrix data structure along with get and set methods for the input
## matrix and the get and set methods for the instance of the inverse matrix that
## is created and saved in memory (cached). This particular version of cacheSolve
## calculates the inverse of the input matrix passed to makeCacheMatrix when it is
## invoked. After calculating the inverse, cacheSolve updates the cache matrix data
## structure instance created by MakeCacheMatrix. We should be able to extend this 
## approach to other situations in R when we have a "computationally expensive"
## operation/action to perform, where the results might need to be re-used later.
## Rather than repeating (re-calculating) the operation/action, we can make use of
## the cached result if it hasn't changed. We could modify the type of data structure
## returned and the operation (say, calculate the standard deviation) implemented by 
## the "cacheSolve" function role.

## This function, makeCacheMatrix, creates a matrix data structure, uses 4 nested
## sub-functions and exploits R's lexical scoping features to create a shared
## environment like the S3 classes discussed here:
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-lexicalScoping.md
## Effectively, makeCacheMatrix creates an instance of an object that includes it's
## own environment and methods along with the created data structure itself.
## makeCacheMatrix includes two methods to get and set the input matrix (x), and two
## methods to get (getmatrix) and set (setmatrix) the cached matrix.
makeCacheMatrix <- function(x = matrix()) {
        cache_matrix <- matrix()
        set <- function(y) {
                x <<- y
                cache_matrix <<- matrix()
        }
        get <- function() x
        setmatrix <- function(my_matrix) cache_matrix <<- my_matrix
        getmatrix <- function() cache_matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve operates on the object (x argument to the function) and environment 
## created by a previous external call to makeCacheMatrix. cacheSolve first checks 
## to see if the cache matrix already contains a result or if it is empty (all NAs).
## If it contains a cached result, cacheSolve returns the cached data and saves the 
## computational effort. If the cache matrix is empty, cacheSolve, gets the input 
## matrix, calculates the inverse using the solve() ({base}, Solve a System of 
## Equations) function, and stores the result in the cache matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache_matrix <- x$getmatrix()
        if(!all(is.na(cache_matrix))) {
                message("getting cached data")
                return(cache_matrix)
        }
        data <- x$get()
        ## The inverse of the matrix is calculated here by "solve". Since the "b"
        ## argument to solve is missing, it returns the inverse of the data matrix.
        cache_matrix <- solve(data)
        x$setmatrix(cache_matrix)
        cache_matrix
}
