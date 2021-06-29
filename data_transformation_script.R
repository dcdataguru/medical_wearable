
df2 <- data.frame(SubNumID = df$SubNumID, location = paste0(df$LocationLatitude,",",df$LocationLongitude))
df2$mom_alive <- df$Q92
df2$dad_alive <- df$Q93
df2$mom_age <- df$Q95
df2$dad_age <- df$Q97
df2$mom_health <- df$Q100
df2$dad_health <- df$Q101

#correlation analysis
library(corrplot)
library(RColorBrewer)

df7_temp2 <- df[,c('SubNumID', 'Q43_1', 'Q43_2', 'Q43_3', 'Q43_4', 'Q43_5')]
df7 <- merge(x = df7, y = df7_temp2, by = "SubNumID", all.x = TRUE)

df7.cor <- df7[,c("Factor1", 'Factor2', 'Factor3', 'consider', 'favorable',
                  'intend', 'relieved', 'promote')]
df7.cor$consider <- as.numeric(df7.cor$consider)
df7.cor$favorable <- as.numeric(df7.cor$favorable)
df7.cor$intend <- as.numeric(df7.cor$intend)
df7.cor$relieved <- as.numeric(df7.cor$relieved)
df7.cor$promote <- as.numeric(df7.cor$promote)

library(corrgram)
corrplot(corrgram(df7.cor), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



#########################
library(tidyverse)
df2$mom_alive[df2$mom_alive == 1] <- "Yes"
df2$mom_alive[df2$mom_alive == 0] <- "No"

df2$dad_alive[df2$dad_alive == 1] <- "Yes"
df2$dad_alive[df2$dad_alive == 0] <- "No"

df2$mom_health[df2$mom_health == 1] <- "Very Bad"
df2$mom_health[df2$mom_health == 2] <- "Bad"
df2$mom_health[df2$mom_health == 3] <- "Good"
df2$mom_health[df2$mom_health == 4] <- "Very Good"

df2$dad_health[df2$dad_health == 1] <- "Very Bad"
df2$dad_health[df2$dad_health == 2] <- "Bad"
df2$dad_health[df2$dad_health == 3] <- "Good"
df2$dad_health[df2$dad_health == 4] <- "Very Good"

df2$city <- geodata$city
df2$state <- geodata$state
df2$zip <- geodata$zip
df2$country <- geodata$country

library(reshape)
df_q99 <- df[c("SubNumID", "Q99_1", "Q99_2", "Q99_3", "Q99_5", "Q99_4", "Q99_4_TEXT")]
m_df_q99 <- melt(df_q99, id="SubNumID")
m_df_q99_true <- subset(m_df_q99, value == 1)
m_df_q99_true$var_char <- as.character(m_df_q99_true$variable)

m_df_q99_true$var_char[m_df_q99_true$var_char == "Q99_1"] <- "Lives with my father"
m_df_q99_true$var_char[m_df_q99_true$var_char == "Q99_2"] <- "Lives by herself"
m_df_q99_true$var_char[m_df_q99_true$var_char == "Q99_3"] <- "Lives in assisted living facility"
m_df_q99_true$var_char[m_df_q99_true$var_char == "Q99_5"] <- "Lives with me"
m_df_q99_true$var_char[m_df_q99_true$var_char == "Q99_4"] <- "Other"

df3 <- merge(x=df2, y = m_df_q99_true, all.x = TRUE)

df2 <- subset(df3, select = -c(variable, value))
colnames(df2)[which(names(df2) == "var_char")] <- "mom_living"


df_q102 <- df[c("SubNumID", "Q102_1", "Q102_2", "Q102_3", "Q102_5", "Q102_4", "Q102_4_TEXT")]
m_df_q102 <- melt(df_q102, id="SubNumID")
m_df_q102_true <- subset(m_df_q102, value == 1)
m_df_q102_true$var_char <- as.character(m_df_q102_true$variable)

m_df_q102_true$var_char[m_df_q102_true$var_char == "Q102_1"] <- "Lives with my mother"
m_df_q102_true$var_char[m_df_q102_true$var_char == "Q102_2"] <- "Lives by himself"
m_df_q102_true$var_char[m_df_q102_true$var_char == "Q102_3"] <- "Lives in assisted living facility"
m_df_q102_true$var_char[m_df_q102_true$var_char == "Q102_5"] <- "Lives with me"
m_df_q102_true$var_char[m_df_q102_true$var_char == "Q102_4"] <- "Other"

df3 <- merge(x=df2, y = m_df_q102_true, all.x = TRUE)

df2 <- subset(df3, select = -c(variable, value))
colnames(df2)[which(names(df2) == "var_char")] <- "dad_living"

#find duplicates - some respondents chose two options for mom/dad current living situation
which(duplicated(df2$SubNumID))

df3 <- (df2[!duplicated(df2$SubNumID),])

st <- data.frame(state = state.abb, state_name = state.name)
df4 <- merge(x=df3, y = st, all.x = TRUE)


#reverse geocoding
library(revgeo)
dfgeo <- df[c(1,15,16)]
geodata <-  revgeo(dfgeo$LocationLongitude[1], provider = 'bing', dfgeo$LocationLatitude[1], 
                                   API="Al9vHpmS5-V-foqH2t9zrnBsuNkH5cRDPsqlq7vEaauiXYpfzR300HAmQKXUjZJY",
                                  output = 'frame')

for(i in 2:nrow(dfgeo)) {
  geodata <-  rbind(geodata, revgeo(dfgeo$LocationLongitude[i], provider = 'bing', dfgeo$LocationLatitude[i], 
                     API="Al9vHpmS5-V-foqH2t9zrnBsuNkH5cRDPsqlq7vEaauiXYpfzR300HAmQKXUjZJY",
                     output = 'frame'))
}



res_age <- df[,c("SubNumID", "Q115")]
res_age$user_age <- as.character(res_age$Q115)
res_age$user_age_int <- as.numeric(res_age$user_age)
res_age$user_age_today <- 2019 - res_age$user_age_int
res_age <- res_age[,c("SubNumID", "user_age_tody")]
df5 <- merge(x = df4, y = res_age, by = "SubNumID", all.x = TRUE)



fa.questions <- c('SubNumID', "Q11", "Q94", "Q75", "Q95.0", 'Q74', 'Q96', 'Q97.0', "Q35", 'Q78', 'Q79',
                  'Q80', 'Q81', 'Q39', 'Q59', 'Q83', 'Q84', 'Q85', 'Q86', 'Q87', 'Q88')

fa_df <- df[, names(df) %in% fa.questions]
sapply(fa_df, function(x) sum(is.na(x)))

fa_df_no.na <- fa_df[,c("SubNumID", "Q35", 'Q78', 'Q79',
                        'Q80', 'Q81', 'Q39', 'Q59', 'Q83', 'Q84', 'Q85', 'Q86', 'Q87', 'Q88')]

fa_only <- subset(fa_df_no.na, select = -c(SubNumID, Q39, Q81))

library(nFactors)
nScree(fa_only)
# 3 out of 4 methods suggest data set has 3 factors

eigen(cor(fa_only))$values
# first three eigenvalues are greater than 1.0

df.fa.ob <- factanal(fa_only, factors=3)

library(gplots)
library(RColorBrewer)
heatmap.2(df.fa.ob$loadings, col = brewer.pal(9,"Greens"), trace = "none", key=FALSE, dend="none",
          Colv=FALSE, cexCol= 1.2, main="\n\n\n\nFactor loadings")

library(semPlot)
semPaths(df.fa.ob, what="est", residuals=FALSE, cut = 0.3, posCol=c("white", "darkgreen"), 
         negCol=c("white", "red"), edge.label.cex=0.75, nCharNodes=7)

#create manager variables

data.frame(ID=DF[,1], Means=rowMeans(DF[,-1]))
fa_df_no.na$Factor1 <- rowMeans(fa_df_no.na[,c("Q83", 'Q84', 'Q86', 'Q87', 'Q88')])
fa_df_no.na$Factor2 <- rowMeans(fa_df_no.na[,c("Q78", 'Q79', 'Q85')])
fa_df_no.na$Factor3 <- rowMeans(fa_df_no.na[,c("Q35", 'Q59', 'Q80')])

fa_df_no.na$Factor2 <- round(fa_df_no.na$Factor2, digits = 1)
fa_df_no.na$Factor3 <- round(fa_df_no.na$Factor3, digits = 1)

fa_df_factors_only <- fa_df_no.na[,c("SubNumID", "Factor1", "Factor2", "Factor3")]
df6 <- merge(x = df5, y = fa_df_factors_only, by = "SubNumID", all.x = TRUE)

# Factor 1:  Neuroticism
# Q83 I feel guilty about living in another state than parent(s)
# Q84 I have a hard time balancing my own personal and professional demands.
# Q86 I often disagree with my parent(s) about how often I visit.
# Q87 My parent(s) think that I am controlling.
# Q88 My parent(s) are secretive about their state of health and activities.
# 
# Factor 2: Desire to monitor parents
# Q78 I would find it helpful if I can more closely monitor my aging parent(s)' daily activities
# Q79 I worry about my parent(s)' wellbeing when leaving themselves on their own.
# Q85 Knowing that my parent(s) are okay would bring me peace of mind.
# 
# Factor 3:  Inability to look after parents
# Q35 It is not possible for me to constantly monitor my aging parent(s).
# Q59 I don't feel that I visit and call my parent(s) enough.
# Q80 Hiring round the clock help for my aging parent(s) is not feasible for me.
# 
# 
# Strongly disagree (1) 
# Disagree (2) 
# Neither agree nor disagree (3) 
# Agree (4) 
# Strongly agree (5)

df6$parent_health <- df6$mom_health
mom_na <- which(is.na(df6$parent_health))
for (x in mom_na) {
  df6[x,"parent_health"] <- df6[x, "dad_health"]
}

both_parents_dead_index <- which(is.na(df6$parent_health))
df6_removed_no_parents <- df6[-c(both_parents_dead_index),]
which(is.na(df6_removed_no_parents$parent_health))

#MANOVA
df6_removed_no_parents$group <- factor(df6_removed_no_parents$parent_health)
library(reshape2)  # for melt() function
library(ggplot2)
# First we need to restructure the data into long format:
#df6_melt <- melt(df6_removed_no_parents, id=c('group'), measured=c('Factor1', 'Factor2', 'Factor3'))
#names(df6_melt) <- c('Group', 'Outcome_Measure', 'Frequency')
# plot
#df6Boxplot <- ggplot(df6_melt, aes(Group, Frequency, color = Outcome_Measure))
#df6Boxplot + geom_boxplot() + labs(x='Parent Health Status', y='Rating', color='Outcome_Measure') 
outcome <- cbind(df6_removed_no_parents$Factor1,df6_removed_no_parents$Factor2, df6_removed_no_parents$Factor3)
df6_model <- manova(outcome ~ group, data=df6_removed_no_parents)
summary(df6_model, intercept=TRUE)
# Df  Pillai approx F num Df den Df    Pr(>F)    
# (Intercept)   1 0.97648  13369.8      3    966 < 2.2e-16 ***
#   group         3 0.02603      2.8      9   2904  0.002617 ** 
#   Residuals   968                                             
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# > summary(df6_model, intercept=TRUE, test = "Wilks")
# Df   Wilks approx F num Df den Df    Pr(>F)    
# (Intercept)   1 0.02352  13369.8      3  966.0 < 2.2e-16 ***
#   group         3 0.97408      2.8      9 2351.1  0.002551 ** 
#   Residuals   968                                             
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#LDA
#df6.lda <- lda(group ~ Factor1 + Factor2 + Factor3, data = df6_removed_no_parents)

#Bonferroni correction
#0.05/3 (# of comparisions)  = 0.0166667


#TukeyHSD(aov(Factor2 ~ group, data=df6_removed_no_parents))
# Factor 2
#                         diff         lwr         upr     p adj
# Good-Bad           -0.08648601 -0.23949929  0.06652727 0.4656016
# Very Bad-Bad        0.22181651 -0.18629182  0.62992485 0.5003952
# Very Good-Bad      -0.28436259 -0.49652150 -0.07220369 0.0032809
# Very Bad-Good       0.30830252 -0.08628009  0.70288513 0.1846419
# Very Good-Good     -0.19787658 -0.38268573 -0.01306744 0.0303684
# Very Good-Very Bad -0.50617910 -0.92724177 -0.08511644 0.0109302

#significant differences between 
#very good vs bad   -0.28436259
#very good vs very bad  -0.50617910


#library(mvnormtest)
#library(agricolae)

#one-way ANOVA with 4 levels
df6.aov <- aov(Factor2 ~ group, data = df6_removed_no_parents)



