#start up from last step in EditDataframe.R, which was to export a CSV of our cleaned up data. 
#Clear the environment
rm(list=ls(all=TRUE))
#Import the cleaned data as a new df
df.t <- read.csv('data/tempExperiment_v2.csv')
#Get an overview
str(df.t)

#R thinks temp is a nubmer but really it's a low or high temp category, so change temp to a factor
df.t$temp <- factor(df.t$temp)

##want to analyze how temperature treatment, population of origin, and size affect growth rate


#Plot the data - boxplot of variable growth rate as a function of (~) temp and pop. + tells it to combine temp and pop to create 4 distinct groups (ex. for df$A + df$b then it does A&C, B&C, A&D, B&D)
boxplot(df.t$growthRate~df.t$temp+df.t$pop)

#add axis labels

boxplot(df.t$growthRate~df.t$temp+df.t$pop,
        names = c('10', '20', '10', '20'), #temperature label
        at = c(1,2,4,5), #space out boxes according to population - I don't get this
        ylab = 'Growth rate mm/day',
        xlab = ''
)


#use mtext to add text in the margin - not an axis label b/c you want to space it with the boxplot. idk how these parameters change it

mtext('Pop 1', side = 1, at = 1.5, line = 3)
mtext('Pop 2', side = 1, at = 4.5, line = 3)

#Now save it to PDF in results folder

pdf('results/MyBoxplot.pdf', width = 5, height = 5)
##Use code from above
boxplot(df.t$growthRate~df.t$temp+df.t$pop,
        names = c('10', '20', '10', '20'), #temperature label
        at = c(1,2,4,5), #space out boxes according to population - I don't get this
        ylab = 'Growth rate mm/day',
        xlab = ''
)


#use mtext to add text in the margin - not an axis label b/c you want to space it with the boxplot. idk how these parameters change it

mtext('Pop 1', side = 1, at = 1.5, line = 3)
mtext('Pop 2', side = 1, at = 4.5, line = 3)

dev.off()

##Now find out if effect of pop or effect of temp is significant in analysis. Use ANOVA. 
# The '*' symbol is for multiplication and means we want to evaluate an interaction, in this cas evaluate growth rate as a function of the interaction between temp & pop

m1 <- aov(df.t$growthRate ~ df.t$temp * df.t$pop)
m1.summary <- summary(m1)
m1.summary

##save the summary table as an R object so you can call it later
saveRDS(m1.summary, 'results/m1.summary.rds')
