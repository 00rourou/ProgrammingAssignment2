## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getCache()
cacheSolve(my_matrix)
cacheSolve(my_matrix)

