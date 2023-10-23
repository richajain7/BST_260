p <- 0.75
B <- 10^5
n <- seq(1, 25, 2)

prob_win <- function(n){
  x <- replicate(B, {
  s <- sample(c(1, 0), size = n, replace = TRUE, prob = c(1-p, p))
  sum(s) >= (n+1)/2
})
mean(x)
}
run <- sapply(n, prob_win)
plot(n, run)

qnorm(0.99, 69, 3)

B <- 1000
iq <- replicate(B, {
  x <- rnorm(10000, 100, 15)
  max(x)
})
hist(iq)

e_x <- (-1*18/19 + 17*1/19)* 1000
e_x

se <- 18 * sqrt(18/19 * 1/19) * sqrt(1000)
se

1-pnorm(0, e_x, se)

p <- seq(0, 1, length = 100)
se <- sqrt((p*(1-p))/1000)
plot(p, se)

se <- 2*sqrt(0.45*0.55/25)
se

library(dslabs)
library(tidyverse)
polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.") 

N <- polls$samplesize[1]
x_hat <- polls$rawpoll_clinton[1]/100

se_hat <- sqrt((x_hat * (1-x_hat))/N)

c(x_hat - 1.96*se_hat, x_hat + 1.96*se_hat)

res <- polls %>% mutate(x_hat = rawpoll_clinton/100, se_hat = sqrt((x_hat * (1-x_hat))/samplesize)) %>% 
  mutate(lower = x_hat - 1.96*se_hat, upper = x_hat + 1.96*se_hat) %>% select(pollster, enddate, 
                                                                              x_hat, lower, upper) %>% 
  mutate(hit = (lower <= 0.482 & upper >= 0.482)) 
mean(res$hit)

polls <- polls_us_election_2016 |> 
  filter(enddate >= "2016-10-31" & state == "U.S.")  |>
  mutate(mu_hat = rawpoll_clinton/100 - rawpoll_trump/100) %>% mutate(p = (mu_hat+1)/2, se = 2*sqrt(p*(1-p)/samplesize)) %>% 
  mutate(lower = mu_hat - 1.96*se, upper = mu_hat + 1.96*se) %>% mutate(hit = (lower <= 0.021 & upper > 0.021))
head(polls)
mean(polls$hit)

polls <- polls %>% mutate(difference = mu_hat - 0.021)

polls %>% group_by(pollster) %>% ggplot(aes(pollster, difference)) + geom_point()

polls %>% group_by(pollster) %>% filter(n()>=5) %>% ggplot(aes(pollster, difference)) + geom_point()

library(dslabs)
x <- heights |> filter(sex == "Male") |>
  pull(height)

mu <- mean(x)
sd(x)

samp <- sample(x, size = 50, replace = TRUE)
mean(samp)
sd(samp)

sigma <- sd(samp)
sigma

c(mean(samp) - 1.96*sigma/sqrt(50), mean(samp) + 1.96*sigma/sqrt(50))

mc <- replicate(10000,{
  samp <- sample(x, size = 50, replace = TRUE)
  interval <- c(mean(samp) - 1.96*sd(samp)/sqrt(50), mean(samp) + 1.96*sd(samp)/sqrt(50))
  between(mu, interval[1], interval[2])
})
mean(mc)

N <- 35
diff <- sapply(2:N, function(n){
  q1 <- qnorm(p = 0.975, mean = 0, sd = 1)
  q2 <- qt(p = 0.975, df = n-1)
  abs(q2-q1)
})
plot(2:N, diff)


library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 |> 
  filter(state == "Florida" & enddate >= "2016-11-04" ) |> 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

results <- polls %>% summarize(avg = mean(spread), se = sd(spread)/sqrt(n()))
results

theta <- 0
tau <- 0.01
sigma <- results$se
mu <- results$avg

B <- sigma^2/(sigma^2 + tau^2)
posterior_mean <- B*theta + (1-theta)*mu
posterior_sd <- sqrt(1/(1/sigma^2 + 1/tau^2))

posterior_mean
posterior_sd


c(posterior_mean - 1.96*posterior_sd, posterior_mean + 1.96*posterior_sd)

pnorm(0, posterior_mean, posterior_sd)

tau <- seq(0.005, 0.05, len=100)
func <- function(t){
  B <- sigma^2/(sigma^2 + t^2)
  posterior_mean <- B*theta + (1-B)*mu
  posterior_sd <- sqrt(1/(1/sigma^2 + 1/t^2))
  pnorm(0, posterior_mean, posterior_sd)
}
ap <- sapply(tau, func)
plot(ap)

n<-100
sum <- n*(n+1)/2
sum

n <- seq(1, 100)
sum(n)

inds <- which(murders$rate < 1)
murders[inds, ]

inds <- which(murders$rate < 1 & murders$region == "West")
murders[inds, c(1,2,4,5,6)]

dat <- murders[murders$rate < 1, ]
dat[which.max(dat$population), ]

dat <- murders[murders$population > 10000000, ]
dat[which.min(dat$rate), ]

dat <- split(1:nrow(murders), murders$region)
sapply(dat, function(n){
  sum(murders$total[n])/sum(murders$population[n])*10^5
})

murders %>% group_by(region) %>% summarize(rate = sum(total)/sum(population)*10^5)

inds <- match(c("New York", "California", "Texas"), murders$state)
murders[inds, ]

qs <- c(10, 30, 50, 70, 90)
heights %>% group_by(sex) %>% reframe(quantile = paste0(qs,"%"), values = quantile(height,qs/100)) %>% 
  pivot_wider(names_from = sex, values_from = values) %>% rename(female_percentile=Female, male_percentile=Male)

x <- heights %>% filter(sex == "Male") %>% pull(height)

mean(x > 69 & x <= 72)


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = as.character(1959:1997))
co2_tidy <- co2_wide %>% pivot_longer(1:12, names_to = "month", values_to = "co2")
co2_tidy

library(Lahman)

top <- Batting |> 
  filter(yearID == 2016) |>
  arrange(desc(HR)) |>
  slice(1:10)

top |> as_tibble()

people <- People |> as_tibble()

top <- top %>%  left_join(people, "playerID") %>% select(playerID, nameFirst, nameLast, HR)

top

salaries <- Salaries %>% as_tibble()
salaries <- salaries %>% filter(yearID == 2016)

top <- top %>% right_join(salaries, "playerID") %>% select(nameFirst, nameLast, teamID, HR, salary)
top


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) |> 
  setNames(1:12) |>
  mutate(year = 1959:1997) |>
  pivot_longer(-year, names_to = "month", values_to = "co2") |>
  mutate(month = as.numeric(month))

yearly_avg <- co2_wide %>% group_by(year) %>% summarize(avg = mean(co2))

co2_wide <- co2_wide %>% left_join(yearly_avg, "year")

co2_wide <- co2_wide %>% mutate(residuals = co2-avg)

co2_wide %>% mutate(year = as.character(year)) %>% ggplot(aes(month, residuals, color = year)) +
  geom_point()









