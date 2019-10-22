# qq plot

# data
set.seed(1)
n = 100
obs = rnorm(n, exp(20), exp(19)) %>% abs() %>% log()
obs %>% hist(30)
#obs %>% density() %>% plot()
teor = rnorm(n)
teor = qnorm(ppoints(length(obs)))[order(order(obs))]

# qq points
teor_obs = cbind(sort(teor), sort(obs))
plot(teor_obs, xlab='theoretical: y ~ N(0, 1)', ylab='observed', type='n')
rug(teor, side=1)
rug(obs, side=2, col='dodgerblue')
points(teor_obs, pch=19, col=scales::alpha('black', .3))

points(median(teor), median(obs), pch=19, col=scales::alpha('dodgerblue', .9))
lines(c(median(teor), median(teor)), c(0, median(obs)), col=scales::alpha('black', .9), lty='dashed')
lines(c(-10, median(teor)), c(median(obs), median(obs)), col=scales::alpha('dodgerblue', .9), lty='dashed')

for (i in 1:nrow(teor_obs)) lines(c(teor_obs[i,1], teor_obs[i,1]), c(0, teor_obs[i,2]), col=scales::alpha('black', .15))
for (i in 1:nrow(teor_obs)) lines(c(-10, teor_obs[i,1]), c(teor_obs[i,2], teor_obs[i,2]), col=scales::alpha('dodgerblue', .15))

# qq line: q 0.25, 0.75
qteor = quantile(teor, probs = c(.25, .75))
qobs = quantile(obs, probs = c(.25, .75))

slope = diff(qobs)/diff(qteor)
intercept = qobs[1] - slope * qteor[1]
abline(intercept, slope, col='red')
