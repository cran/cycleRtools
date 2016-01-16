context("Checking zone functions")

"%btwn%" <- function(x, rng) x > rng[[1]] & x <= rng[[2]]
"inc<-"  <- function(x, value) x <- x + value

zone_index_R <- function(x, zb) {
  zones  <- mapply("c", c(0, zb), c(zb, max(x) + 10), SIMPLIFY = FALSE)
  zindex <- lapply(zones, function(z) which(x %btwn% z))
  out <- numeric(length(x)); count <- 1
  for (i in zindex) {
    out[i] <- count; inc(count) <- 1
  }
  out
}

test_that("zone indexing", {
  x  <- floor(abs(rnorm(100, 500, 200)))
  zb <- sort(sample(seq(from = min(x), to = max(x), by = 1), 4))
  all.equal(zone_index(x, zb), zone_index_R(x, zb))
})
