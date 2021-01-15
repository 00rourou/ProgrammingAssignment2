## Caching the inverse of the matrix
## Github: 00rourou
## Data: 15th Jan, 2021

## To create the matrix object and store its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m_cache <- NULL
  set <- function(y){
    x <<- y
    m_cache <<- NULL
  }
  get <- function() x
  setCache <- function(cache) m_cache <<- cache
  getCache <- function() m_cache
  list(set = set, 
       get = get,
       setCache = setCache,
       getCache = getCache)
}

## To compute the inverse of the matrix that created by makeCacheMatrix()
## If the inverse of certain matrix has already been calculated and the 
## the matrix has not changed, then return the inverse from the cache.
                                            

cacheSolve <- function(x, ...) {
  m_cache <- x$getCache()
  if(!is.null(m_cache)){
    message("getting cached data")
    return(m_cache)
  }
  data <- x$get()
  m_cache <- solve(data,...)
  x$setCache(m_cache)
  m_cache
  ## Return a matrix that is the inverse of 'x'
}

## Testing functions
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getCache()
cacheSolve(my_matrix) 
cacheSolve(my_matrix)
