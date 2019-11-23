set.seed(12)
pop = rnorm(3e2, 50, 10)
d_p = data.frame(x=pop)
d_s = data.frame(x=pop[sample(1:length(pop), 20)])
ci = t.test(d_s)$conf.int
eb = data.frame(xmin=t.test(d_s)$conf.int[1], xmax=t.test(d_s)$conf.int[2], y=0.2, y1=0.75)
g = ggplot() +
  geom_dotplot(data=d_p, aes(x=x), alpha=.2, binwidth=1.5) +
  geom_dotplot(data=d_s, aes(x=x), alpha=.8, binwidth=1.5) + 
  geom_vline(xintercept = 50, linetype='dashed', color='red') +
  geom_errorbarh(data=eb, mapping=aes(xmin=xmin, xmax=xmax, y=y), height=.05, size=1) + 
  coord_cartesian(ylim=c(0, 1.2)) +
  theme_classic() + ggtitle('Доверительные интервалы')
g

for (s in 1:100){
  set.seed(s)
  pop = rnorm(3e2, 50, 10)
  d_p = data.frame(x=pop)
  d_s = data.frame(x=pop[sample(1:length(pop), 20)])
  ci = t.test(d_s)$conf.int
  eb = data.frame(xmin=t.test(d_s)$conf.int[1], xmax=t.test(d_s)$conf.int[2], y=0.2, y1=1+runif(1, -.2, .2))
  if(!( 50 > ci[1] & 50 < ci[2])) {color='red';alpha=.9;} else {color='black';alpha=.2}
  g = g + geom_errorbarh(data=eb, mapping=aes(xmin=xmin, xmax=xmax, y=y1), height=.01, alpha=alpha, color=color)
}
g