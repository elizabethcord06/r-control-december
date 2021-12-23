way <- ''
file.name.in = 'Delivery'
file.name.out = 'Sales'

days <- 10
price.in1 <- 100
price.sale1 <- 140
price.util1 <- 15

price.in2 <- 150
price.sale2 <- 200
price.util2 <- 7

price.in3 <- 130
price.sale3 <- 170
price.util3 <- 20

price.in4 <- 70
price.sale4 <- 100
price.util4 <- 7

range1 <- c(500,600)

period <- c(1:days)
period

#---------

input.1 <- sample(x = c(range1[1]:range1[2]), size = days)
input.1
sale.1 <- as.integer(input.1 * runif(min = 0.7, max = 1, n = days))
sale.1
util.1 <- input.1 -sale.1
pr.1 <- price.sale1 * sale.1 - price.in1 * input.1 - price.util1 * util.1
pr.1

input.2 <- sample(x = c(range1[1]:range1[2]), size = days)
input.2
sale.2 <- as.integer(input.2 * runif(min = 0.7, max = 1, n = days))
sale.2
util.2 <- input.2 -sale.2
pr.2 <- price.sale2 * sale.2 - price.in2 * input.2 - price.util2 * util.2
pr.2

input.3 <- sample(x = c(range1[1]:range1[2]), size = days)
input.3
sale.3 <- as.integer(input.3 * runif(min = 0.7, max = 1, n = days))
sale.3
util.3 <- input.3 -sale.3
pr.3 <- price.sale3 * sale.3 - price.in3 * input.3 - price.util3 * util.3
pr.3

input.4 <- sample(x = c(range1[1]:range1[2]), size = days)
input.4
sale.4 <- as.integer(input.4 * runif(min = 0.7, max = 1, n = days))
sale.4
util.4 <- input.4 -sale.4
pr.4 <- price.sale4 * sale.4 - price.in4 * input.4 - price.util4 * util.4
pr.4



#-------------
tab10 <- data.frame(
  'Day' = period,
  'Bread, unit delivery' = input.1,
  'Bread, unit sale' = sale.1,
  'Bread, unit expiration' = util.1,
  'Bread, profit rubles' = pr.1,
  'Chicken, unit delivery' = input.2,
  'Chicken, unit sale' = sale.2,
  'Chicken, unit expiration' = util.2,
  'Chicken, profit rubles' = pr.2,
  'Cheese, unit delivery' = input.3,
  'Cheese, unit sale' = sale.3,
  'Cheese, unit expiration' = util.3,
  'Cheese, profit rubles' = pr.3,
  'Banana, unit delivery' = input.4,
  'Banana, unit sale' = sale.4,
  'Banana, unit expiration' = util.4,
  'Banana, profit rubles' = pr.4
)

View(tab4)

tab.in <- data.frame(
  'Day' = period,
  'Bread, units delivery' = input.1,
  'Chicken, units delivery' = input.2,
  'Cheese, units delivery' = input.3,
  'Banana, units delivery' = input.4
)

tab.out <- data.frame(
  'Day' = period,
  'Bread, units sales' = sale.1, 
  'Chicken, units delivery' = input.2,
  'Cheese, units delivery' = input.3,
  'Banana, units delivery' = input.4
)

write.table(
  x = tab.in,
  file = paste0(way, file.name.out, '.in'),
  col.names = TRUE,
  row.names = FALSE
)

write.table(
  x = tab.out,
  file = paste0(way, file.name.out, '.out'),
  col.names = TRUE,
  row.names = FALSE
)


#-----------------------------------------------
input.1
sale.1
input.2
sale.2
input.3
sale.3
input.4
sale.4

min.y <- min(input.1, sale.1) * 0.9
max.y <- max(input.1, sale.1) * 1.1

plot(
  x = period,
  y = input.1,
  type = 'l',
  lwd = 4,
  col = 'blue',
  main = 'Bread dinamics of delivery and sales - store 10',
  xlab = 'Period',
  ylab = 'Units delivery and sales',
  ylim = c(min.y, max.y)
)

lines(
  x = period,
  y = sale.1,
  type = 'l',
  lwd = 2,
  col = 'red',
  xlab = 'Period',
  ylab = 'Sales units'
)

abline(v = period, col = 'gray')

legend(x = 'topright',
       legend = c('Delivery', 'Sales'),
       fill = c('blue', 'red')
)

#----------------------------------------

min.y <- min(input.2, sale.2) * 0.9
max.y <- max(input.2, sale.2) * 1.1
plot(
  x = period,
  y = input.2,
  type = 'l',
  lwd = 4,
  col = 'blue',
  main = 'Chicken dinamics of delivery and sales - store 10',
  xlab = 'Period',
  ylab = 'Units delivery and sales',
  ylim = c(min.y, max.y)
)

lines(
  x = period,
  y = sale.2,
  type = 'l',
  lwd = 2,
  col = 'red',
  xlab = 'Period',
  ylab = 'Sales units'
)

abline(v = period, col = 'gray')

legend(x = 'topright',
       legend = c('Delivery', 'Sales'),
       fill = c('blue', 'red')
)


#------------------------------------

min.y <- min(input.3, sale.3) * 0.9
max.y <- max(input.3, sale.3) * 1.1
plot(
  x = period,
  y = input.3,
  type = 'l',
  lwd = 4,
  col = 'blue',
  main = 'Cheese dinamics of delivery and sales - store 10',
  xlab = 'Period',
  ylab = 'Units delivery and sales',
  ylim = c(min.y, max.y)
)

lines(
  x = period,
  y = sale.3,
  type = 'l',
  lwd = 2,
  col = 'red',
  xlab = 'Period',
  ylab = 'Sales units'
)

abline(v = period, col = 'gray')

legend(x = 'topright',
       legend = c('Delivery', 'Sales'),
       fill = c('blue', 'red')
)

#-------------------------------------------------

min.y <- min(input.4, sale.4) * 0.9
max.y <- max(input.4, sale.4) * 1.1
plot(
  x = period,
  y = input.4,
  type = 'l',
  lwd = 4,
  col = 'blue',
  main = 'Banana dinamics of delivery and sales - store 10',
  xlab = 'Period',
  ylab = 'Units delivery and sales',
  ylim = c(min.y, max.y)
)

lines(
  x = period,
  y = sale.4,
  type = 'l',
  lwd = 2,
  col = 'red',
  xlab = 'Period',
  ylab = 'Sales units'
)

abline(v = period, col = 'gray')

legend(x = 'topright',
       legend = c('Delivery', 'Sales'),
       fill = c('blue', 'red')
)

#-------------------


getwd()
setwd("/Users/elizabethcordero/Desktop/programming/R/kr1/Analysis")
getwd()
dir()
read.table("store1_sales.in")

in1 <- read.table("store1_sales.in", head = TRUE)
out1 <- read.table("store1_sales.out", head = TRUE)
in2 <- read.table("store2_sales.in", head = TRUE)
out2 <- read.table("store2_sales.out", head = TRUE)
in3 <- read.table("store3_sales.in", head = TRUE)
out3 <- read.table("store3_sales.out", head = TRUE)
in4 <- read.table("store4_sales.in", head = TRUE)
out4 <- read.table("store4_sales.out", head = TRUE)
in5 <- read.table("store5_sales.in", head = TRUE)
out5 <- read.table("store5_sales.out", head = TRUE)
in6 <- read.table("store6_sales.in", head = TRUE)
out6 <- read.table("store6_sales.out", head = TRUE)
in7 <- read.table("store7_sales.in", head = TRUE)
out7 <- read.table("store7_sales.out", head = TRUE)
in8 <- read.table("store8_sales.in", head = TRUE)
out8 <- read.table("store8_sales.out", head = TRUE)
in9 <- read.table("store9_sales.in", head = TRUE)
out9 <- read.table("store9_sales.out", head = TRUE)
in10 <- read.table("store10_sales.in", head = TRUE)
out10 <- read.table("store10_sales.out", head = TRUE)





















