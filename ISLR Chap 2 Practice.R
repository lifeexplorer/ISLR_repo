Wage

q7.dat <- tibble(x1 = c(0, 2, 0, 0, -1, 1), 
       x2 = c(3, 0, 1, 1, 0, 1), 
       x3 = c(0, 0, 3, 2, 1, 1), 
       y = factor(c("Red", "Red", "Red", "Green", "Green", "Red")))

select(q7.dat, x1, x2, x3) %>% rbind(c(0, 0, 0)) %>% dist(x = ., method = 'euclidean')

