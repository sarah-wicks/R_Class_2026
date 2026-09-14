

##Import data

temp.df <- read.csv("data/tempExperiment-raw.csv", header = T)
temp.df


head(temp.df)

print(temp.df)

#separate temperature treatment from population using strsplit
a <- strsplit(as.character(temp.df$temp), split = '-')
#use unlist and matrix functions to rearranging the output from strsplit
newvar <- matrix(unlist(a), ncol = 2, byrow = TRUE)
head(newvar)
#make a new dataframe with just temp and a new column for pop
df.t2 <- temp.df #set the new dataframe to the original so we can fix it
df.t2$temp <- newvar[,1]
df.t2$pop <- newvar[,2]
head(df.t2)

#use sub to give temp only numeric values and pop only levels pop 1 and pop 2, watch the space at the end of population
df.t2$temp <- sub('ten', '10', df.t2$temp)
df.t2$temp <- sub('twenty', '20', df.t2$temp)
df.t2$pop <- sub('population ', 'pop', df.t2$pop)
str(df.t2)
#Make sure all objects are correct flavor using factor() or as.numeric
df.t2$temp <- factor(df.t2$temp)
df.t2$pop <- factor(df.t2$pop)
str(df.t2)

#Export a new edited csv 
write.csv(df.t2, 'data/tempExperiment_v2.csv')


