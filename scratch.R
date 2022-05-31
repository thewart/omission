.get_theta3 <- function(theta1,theta2) {
  return(1/(1 + (1-theta1)/((1-theta2)*theta1)))
}

get_theta <- function(M) .get_theta3(M$theta1,M$theta2)

rbinombinom <- function(n0,theta1,theta2) {
  y1 <- rbinom(n0, size = 1, prob = theta1)
  y2 <- rbinom(n0, size = 1, prob = theta2) * y1
  return(list(D=data.table(Y1=y1,Y2=y2),n0=n0,theta1=theta1,theta2=theta2))
}

.resample_obs <- function(n0,theta2,y1) rbinom(n0, size = 1, prob = theta2) * Y1

get_n1 <- function(M) sum(M$D$Y1)
get_n2 <- function(M) sum(M$D$Y2)
get_n3 <- function(M) M$D[Y2==0,sum(Y1)]

sample_n3 <- function(n,n0,theta1,theta2) {
  return(sapply(1:n, function(x) rbinombinom(n0,theta1,theta2) |> get_n3()))
}
