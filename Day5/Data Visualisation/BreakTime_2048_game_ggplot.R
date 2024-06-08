## Code sourced from: https://coolbutuseless.github.io/2021/11/14/a-simple-2048-game-engine-in-r/
# Check out https://coolbutuseless.github.io/ for other interesting uses of R
#
# This game is built entirely with ggplot and base R functions. Whilst this is a mile away from our usual approach to coding in research, it is an interesting example of the potential opportunities R provides us. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#    #####    ###   #        #####
#   #     #  #   #  #    #  #     #
#         # #     # #    #  #     #
#    #####  #     # #    #   #####
#   #       #     # ####### #     #
#   #        #   #       #  #     #
#   #######   ###        #   #####
#
#
#
# Board Size: Default = 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2)
N <- 4L

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Collapse a vector in the style of 2048
#
# Collapse vector by removing zeros, then summing pairs of adjacent values
#
# Result is padded with zeros to length of input
#
# @param x vector of integers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
collapse_vec <- function(x) {
  new  <- x[x > 0]
  
  i <- 1L
  while (i < length(new)) {
    if (new[i] == new[i + 1L]) {
      new[i] <- new[i] * 2L
      new    <- new[-(i + 1L)]
    }
    
    i <- i + 1L
  }
  
  c(new, integer(N - length(new)))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialise game board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
m       <- matrix(0L, N, N)
init    <- sample(N*N, 2)
m[init] <- 2L
m_prior <- m


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot game board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_m <- function(m) {
  grid <- expand.grid(1:N, 1:N)
  idx  <- arrayInd(which(m > 0), .dim = c(N, N))
  df   <- cbind(as.data.frame(idx), V3 = factor(m[m > 0], levels = 2^(1:11)))
  ggplot(df, aes(V2, N + 1L - V1)) +
    geom_tile(data = grid, aes(Var1, Var2), fill = 'grey90', width = 0.9, height = 0.9) +
    geom_tile(aes(fill = V3), width = 0.9, height = 0.9) +
    geom_text(aes(label = V3), colour = 'white', size = 10) +
    theme_void() +
    xlim(0, N+1L) + ylim(0, N+1L) +
    coord_equal() +
    theme(legend.position = 'none') +
    scale_fill_viridis_d(option = 'D', drop = FALSE, end = 0.9)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Game loop
#    Use WASD to collapse the board in up/left/down/right directions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print(plot_m(m))

while (max(m) < 2048 && any(m == 0)) {
  if (!identical(m, m_prior)) {
    # if the board has changed, then add a new '2' tile
    m[sample(which(m == 0), 1)] <- 2L
    
    print(m)
    print(plot_m(m))
  }
  
  m_prior <- m
  # cmd <- readline(prompt = "move?: ")
  m <- switch(
    readline(prompt = "move?: "),
    'q' = break,
    'w' = apply(m, 2, collapse_vec),                            # up
    'a' = t(apply(m, 1, collapse_vec)),                         # left
    's' = apply(m[nrow(m):1,], 2, collapse_vec)[nrow(m):1,]   , # down
    'd' = t(apply(m[,nrow(m):1], 1, collapse_vec))[,nrow(m):1], # right
    m
  )
}

if (max(m) >= 2048) {
  message("You won!")
}
