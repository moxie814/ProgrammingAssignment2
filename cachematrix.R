# Function to create a special matrix object with caching ability
makeCacheMatrix <- function() {
  # Initialize a matrix
  mat <- NULL
  # Initialize a cache for the inverse
  cache <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    # Set the matrix
    mat <<- matrix
    # Invalidate the cache since the matrix has changed
    cache <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    # Return the matrix
    mat
  }
  
  # Function to set the inverse of the matrix in the cache
  setInverse <- function(inverse) {
    # Set the inverse in the cache
    cache <<- inverse
  }
  
  # Function to get the inverse from the cache
  getInverse <- function() {
    # Return the cached inverse
    cache
  }
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of a matrix with caching
cacheSolve <- function(cacheMatrix) {
  # Check if the inverse is already cached
  cachedInverse <- cacheMatrix$getInverse()
  if (!is.null(cachedInverse)) {
    # If cached, return the cached inverse
    message("Getting cached data.")
    return(cachedInverse)
  }
  
  # If not cached, compute the inverse
  mat <- cacheMatrix$get()
  inverse <- solve(mat)
  
  # Cache the computed inverse
  cacheMatrix$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}
