set.seed(400)

app1 <- BANOVA.Normal(Factor2 ~ user_age_today + gender + income + relationship_status
                      + hispanic + degree + employment_status + parent_health, l1_hyper = c(1,1,0.0001),
                      data = df, id = df$SubNumID, burnin = 5000, sample = 1000, thin = 10)


app4 <- BANOVA.Normal(WTP_log ~ parent_health, l1_hyper = c(1,1,0.0001), data = df, id = df$SubNumID, burnin = 5000, sample = 1000, thin = 10)

app3 <- BANOVA.Poisson(WTP.int ~ 1, ~ parent_health, data = df, id = df$SubNumID, burnin = 5000, sample = 1000, thin = 20)
# parent_health mean    2.5%    97.5%  
# Bad           49.113  41.9973 57.5537
# Good          55.3818 50.3307 61.7705
# Very Bad      49.6013 31.2908 78.2882
# Very Good     37.8015 31.142  46.1298

seg.bf2 <- lmBF(Factor2 ~ user_age_today + gender + income + relationship_status
                + hispanic + degree + employment_status + group, data = df)
seg.bf2.chain <- posterior(seg.bf2, 1, iterations=10000)
seg.bf2.chain.total <- seg.bf2.chain[, 2:47] + seg.bf2.chain[, 1]
seg.bf2.ci <- t(apply(seg.bf2.chain.total, 2,quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf2.ci


seg.bf2.df <- data.frame(seg.bf2.ci)
seg.bf2.df$Segment <- rownames(seg.bf2.df)

p2 <- ggplot(seg.bf2.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
p2 <- p2 + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Factor2")
p2 + ggtitle("95% CI") + coord_flip()



seg.bf1 <- lmBF(Factor2 ~ group, data = df)
seg.bf.chain <- posterior(seg.bf1, 1, iterations=10000)
seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
seg.bf.ci <- t(apply(seg.bf.chain.total, 2,quantile, pr=c(0.025, 0.5, 0.975)))
seg.bf.ci

#                   2.5%      50%    97.5%
# group-Bad       3.969488 4.068372 4.166594
# group-Good      3.925062 3.984394 4.043535
# group-Very Bad  3.958458 4.220980 4.500728
# group-Very Good 3.675071 3.799276 3.925619

library(ggplot2)
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)

p <- ggplot(seg.bf.df, aes(x=Segment, y=X50., ymax=X97.5., ymin=X2.5.))
p <- p + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Factor2")
p + ggtitle("95% CI") + coord_flip()



app2 <- BANOVA.Normal(Factor2 ~ group, l1_hyper = c(1,1,0.0001),
                      data = df, id = df$SubNumID, burnin = 5000, sample = 1000, thin = 10)

df.aov <- aov(Factor2 ~ group, data = df)
df.aov.plot <- data.frame(TukeyHSD(df.aov)$group)
df.aov.plot$Segment <- rownames(df.aov.plot)

p3 <- ggplot(df.aov.plot, aes(x=Segment, y=diff, ymax=upr, ymin=lwr, colour=Segment))
p3 <- p3 + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("Factor 2: Desire to Monitor Parents")
p3 + coord_flip() + geom_hline(yintercept = 0, color = "red", size=1) + 
  ggtitle("95% CI for Differences in Means") + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

app3.table <- app3.pred$'Means for factors at level 2:'
app3.seg <- app3.table[,1]
app3.mean <- as.numeric(app3.table[,2])
app3.lwr <- as.numeric(app3.table[,3])
app3.upr <-  as.numeric(app3.table[,4])
app3.pred.df <- data.frame(segment = app3.seg, mean = app3.mean, lwr = app3.lwr, upr = app3.upr)

app3.pred.df$mean <- round(app3.pred.df$mean, digits=0)
app3.pred.df$upr <- round(app3.pred.df$upr, digits=0)
app3.pred.df$lwr<- round(app3.pred.df$lwr, digits=0)
 
# Segment    mean     lwr     upr
# 1       Bad  49.113 41.9973 57.5537
# 2      Good 55.3818 50.3307 61.7705
# 3  Very Bad 49.6013 31.2908 78.2882
# 4 Very Good 37.8015  31.142 46.1298

p4 <- ggplot(app3.pred.df, aes(x=segment, y=mean, ymax=upr, ymin=lwr, colour=segment))
p4 <- p4 + geom_point(size=4) + geom_errorbar(width=0.2) + ylab("WTP ($)")
p4 + ggtitle("Willingness to Pay ($) by Segment") + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')

