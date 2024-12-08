## ---- pathPractical1
library('CircStats')
library(ggplot2)
library('adehabitatLT') ## dealing with ltraj object
N <- 1e5

# Parameters Section ------------------------------------------------------
gamma.wc <- c(0.92, 0.99)
mu.wc <- c(0, 0)  

mu.ln <- c(-4, -3)
sigma.ln <- c(0.3, 0.3)

trans.mat <- matrix(c(0.999, 0.005, 0.001, 0.995), ncol=2)
State <- rep(1,N)
for(i in 1:(N-1))
{
  State[i+1]<- sample(1:2, size=1, prob = trans.mat[State[i],])
}

# simulation Section ------------------------------------------------------

phi <- sapply(1:N, function(d){rwrpcauchy(1, location=mu.wc[State[d]], rho=gamma.wc[State[d]])})
ggplot(data.frame(phi = phi), aes(x = phi)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.6) +
  theme_minimal() +
  labs(title = "Histogram of Phi", x = "Phi", y = "Frequency")

V <- exp(rnorm(N, mean=mu.ln[State]-sigma.ln[State]^2/2, sigma.ln[State]))
ggplot(data.frame(phi = phi, V = V, State = as.factor(State)), aes(x = phi, y = V, color = State)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Phi vs V", x = "Phi", y = "V", color = "State")



## ---- pathPractical2
# Plot section ------------------------------------------------------------
dt = 0.01
x <- cumsum(c(0,V*dt*cos(cumsum(phi))))
y <- cumsum(c(0,V*dt*sin(cumsum(phi))))


date=  as.POSIXlt(Sys.time()-12*24*3600 + cumsum(c(0,rep(10, N))), origin = "GMT")

path <- data.frame(x=x, y=y, date=date)

ggplot(path, aes(x = x, y = y)) +
  geom_path(color = "blue", alpha = 0.6) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Path Trajectory", x = "X", y = "Y")


## ---- pathPractical3
ggplot(path, aes(x = x, y = y, color = as.factor(State))) +
  geom_path(alpha = 0.6) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Path with States", x = "X", y = "Y", color = "State")


# Sampling section --------------------------------------------------------
sample.path <- function( initial.path, acquisition.step=1)
{
  observed.path <- initial.path[seq(1,N, acquisition.step),]
  return(observed.path)
}

## ---- pathPractical4
for (s in 1:20) {
  acquisition.step <- round(10 * exp(s / 5) / 10) * 10
  sampled.path <- sample.path(initial.path = path, acquisition.step = acquisition.step)
  ggplot(sampled.path, aes(x = x, y = y)) +
    geom_path(alpha = 0.6, color = "blue") +
    coord_equal() +
    theme_minimal() +
    labs(title = paste("Sampled Path with Acquisition Step", acquisition.step), x = "X", y = "Y")
}
  
## ---- pathPractical5
s=10
acquisition.step <- round(10*exp(s/5)/10)*10
sampled.path <- sample.path(initial.path = path, acquisition.step=acquisition.step)
ggplot(sampled.path, aes(x = x, y = y)) +
  geom_point(size = 0.4) +
  coord_equal() +
  theme_minimal() +
  labs(title = paste("Sampled Path with Acquisition Step", acquisition.step), x = "X", y = "Y")


## ---- pathPractical6

## ---- pathPractical7
s=10
acquisition.step <- round(10*exp(s/5)/10)*10
sampled.path <- sample.path(initial.path = path, acquisition.step=acquisition.step)
plot(sampled.path$x, sampled.path$y,type = "p" ,lty=1, cex=0.4)

# Observed Data -----------------------------------------------------------
s=60
observed.path <- sample.path(initial.path = path, acquisition.step = s)
traj.ex <- as.ltraj(xy = observed.path[, 1:2], date = observed.path$date, id = rep(1, nrow(observed.path))) 
state.ex <- State[seq(1, length(State), s)]

traj.ex.df <- data.frame(x = traj.ex[[1]]$x, y = traj.ex[[1]]$y, state = state.ex)

ggplot(traj.ex.df, aes(x = x, y = y, color = as.factor(state))) +
  geom_point(size = 0.5) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Observed Path with States", x = "X", y = "Y", color = "State")

save(list = c("traj.ex", "state.ex"), file = "trajEx.Rd")

