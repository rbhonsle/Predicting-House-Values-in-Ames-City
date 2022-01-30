#installing packages

install.packages("vip")
install.packages("pdp")
install.packages("leaps")
install.packages('caret')
install.packages('dplyr')
install.packages("corrr")
install.packages("car")
install.packages("ggthemes")
install.packages("ggcorrplot")
install.packages("ggpubr")
install.packages("randomForest")
install.packages("fastDummies")
install.packages("plotly")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("ggplot2")

#calling libraries

library(ggplot2)
library(plotly)
library(viridis)
library(hrbrthemes)
library(dplyr)
library(tidyverse)
library(leaps)
library(corrr)
library(car)
library(ggthemes)
library(corrplot)
library(ggpubr)
library(randomForest)
library(fastDummies)
library(ISLR)
library(lattice)
library(grid)
library(gridExtra)

#Loading the csv file
data <- read.csv("data.csv",header = TRUE);

# viewing dataframe

View(data)
#number of rows in dataframe

nrow(data)

#number of columns

ncol(data)

# Selecting required variables to build model

req <- c("MSZoning", "LotArea","Neighborhood",
           "BldgType","OverallQual","YearBuilt","YearRemodAdd",
           "CentralAir",
           "GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath",
           "KitchenAbvGr", "TotRmsAbvGrd", "Functional","GarageArea",
           "SalePrice")

filtered_df = data[,req]

## Exploratory Analysis

# Data Distributions

glimpse(filtered_df)

#Sales Price Histogram
# We note that the response variable is not normally distributed with a few outliers notably beyond the 500k mark

price_normal = ggplot(filtered_df,aes(SalePrice/1000))+
  geom_histogram(fill="royalblue4")+
  ggtitle("Distribution of Sale Price")+
  geom_vline(aes(xintercept = mean(SalePrice)/1000,alpha=2),linetype="dotdash",color="salmon",size=1.25,alpha=1)+
  annotate("text", label = "Mean", x = 210,y=250)+
  theme_minimal()+
  theme(plot.title=element_text(vjust=2,hjust=.5,face="bold",size=16),panel.background = element_rect(fill="cornsilk"),
        axis.text=element_text(size=12,color="gray37"),axis.title.y = element_text(size=14))+
  xlab(element_blank())+ylab("Frequency")

price_log = ggplot(filtered_df,aes(log(SalePrice)))+
  geom_histogram(fill="royalblue4")+
  ggtitle("Logarithmic Distribution of Sale Price")+
  geom_vline(aes(xintercept = mean(log(SalePrice))),linetype="dotdash",color="salmon",size=1.25,alpha=1)+
  annotate("text", label = "Mean", x = 12.15,y=175,size=4)+
  theme_minimal()+
  theme(plot.title=element_text(vjust=2,hjust=.5,face="bold",size=16),panel.background = element_rect(fill="cornsilk"),
        axis.text=element_text(size=12,color="gray37"),axis.title.y = element_text(size=14))+
  xlab(element_blank())+ylab("Frequency")


ggarrange(price_normal,price_log,nrow=1)

#zoning
z_freq = table(filtered_df$MSZoning)
z_values = as.numeric(z_freq)
z_names = names(z_freq)

z_data = data.frame(z_values,z_names)

z_plot = ggplot(z_data,aes(x= reorder(z_names,-z_values),z_values))+geom_bar(stat ="identity",fill="tan4")+theme_minimal()+
  xlab(element_blank())+ylab(element_blank())+ylab("count")+ xlab("Zone")+
  ggtitle("Zoning by Count")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title = element_text(vjust=2,hjust=.5,face="bold",size=16),axis.text=element_text(size=12,color="gray37"))
z_plot

#Neighborhood
n_freq = table(filtered_df$Neighborhood)

n_values = as.numeric(n_freq)
n_names = names(n_freq)

n_data = data.frame(n_values,n_names)

n_plot= ggplot(n_data,aes(x= reorder(n_names,n_values),n_values))+geom_bar(stat ="identity",fill="turquoise4")+theme_minimal()+
  xlab(element_blank())+ylab(element_blank())+coord_flip()+xlab("Neighborhood")+ylab("count")+
  ggtitle("Neighborhood by Count")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title = element_text(vjust=2,hjust=.5,size=16,face="bold"),axis.text=element_text(size=12,color="gray37"))
n_plot

# Building type

bldg_freq = table(filtered_df$BldgType)

bldg_values = as.numeric(bldg_freq)
bldg_names = as.factor(names(bldg_freq))

bldg_names = recode_factor(bldg_names,"1Fam"="Single-family detached","2fmCon" = "Two-family Converstion" , "Duplx" ="Duplex" ,"TwnhsE" = "Townhouse End Unit","Twnhs" = "Townhouse Inside Unit")

bldg_data = data.frame(bldg_values,bldg_names)

cols = c("#F6AE2D","#55DDE0", "#33658A", "#2F4858", "#F26419")

bldg_plot = ggplot(bldg_data,aes(x="",y=bldg_values,fill=bldg_names))+
  geom_bar(stat="identity",width=1)+
  ggtitle("Building Types by Frequency")+
  coord_polar("y",start=0)+theme_void()+theme(plot.title=element_text(hjust=.5,face="bold",size=16),legend.title=element_blank(),legend.position="right",
                                              legend.text = element_text(size=10))+
  scale_fill_manual(values = cols)
bldg_plot

ggarrange(z_plot,bldg_plot,n_plot,nrow=1,heights = c(1,1,1))

#interactive bubble chart

p <- n_data %>%
  mutate(text = paste("Neighborhood: ", n_names," Count: ",n_values, sep="")) %>%
  ggplot( aes(x=n_values, y=n_names, size = n_values, color = n_names, text=text)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Neighborhood") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

pp <- ggplotly(p, tooltip="text")
pp

# log transform response variable
filtered_df$SalePriceLog <- log(filtered_df$SalePrice)
filtered_df <- subset(filtered_df,select = -(SalePrice))

# Continuous Variables Plots

f_bath<-ggplot(filtered_df,aes(x = FullBath , y = SalePriceLog, group=FullBath)) +
  geom_boxplot(fill=c("salmon","cornsilk","skyblue","lightgreen"))+theme_minimal()+
  xlab(element_blank())+
  ggtitle("# of Full Baths")+
  ylab("Log of Sale Price")+xlab("Value")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title=element_text(hjust=.5,size=14),axis.text=element_text(size=12,color="gray37"))

h_bath<-ggplot(filtered_df,aes(x = HalfBath , y = SalePriceLog, group=HalfBath)) +
  geom_boxplot(fill=c("salmon","cornsilk","skyblue"))+theme_minimal()+
  xlab(element_blank())+
  ggtitle("# of Half Baths")+
  ylab("Log of Sale Price")+xlab("Value")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title=element_text(hjust=.5,size=14),axis.text=element_text(size=12,color="gray37"))


trab<-ggplot(filtered_df,aes(x = TotRmsAbvGrd, y= SalePriceLog, fill = TotRmsAbvGrd),color="salmon") +
  geom_point()+geom_smooth(color="yellow")+theme_minimal()+
  xlab(element_blank())+
  ggtitle("Total Rooms")+
  ylab("Log of Sale Price")+xlab("Value")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title=element_text(hjust=.5,size=14),axis.text=element_text(size=12,color="gray37"),legend.position="none")

gla<-ggplot(filtered_df,aes(x = GrLivArea , y = SalePriceLog)) +
  geom_point()+
  geom_smooth(method = "lm",color="chocolate1")+theme_minimal()+
  ylab("Log of Sale Price")+xlab("Value")+
  xlab(element_blank())+
  ggtitle("Above Grade Living Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title=element_text(hjust=.5,size=14),axis.text=element_text(size=12,color="gray37"))

ga<-ggplot(filtered_df,aes(x = GarageArea , y = SalePriceLog))+
  geom_point()+
  geom_smooth(method = "lm")+theme_minimal()+
  ylab("Log of Sale Price")+xlab("Value")+
  xlab(element_blank())+
  ggtitle("Garage Area")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.title=element_text(hjust=.5,size=14),axis.text=element_text(size=12,color="gray37"))

layout_matrix = matrix(c(1,1,2,2,3,3,
                         6,4,4,5,5,6),nrow=2,byrow=TRUE)

grid.arrange(trab,gla,ga,f_bath,h_bath, layout_matrix = layout_matrix,top=text_grob("Exploratory Visualizations of Continuous Variables",size=16,face="bold"))


## Analyzing MSZoning
MSZoning1<- filtered_df %>% group_by(MSZoning) %>% summarise(Count=n())
MSZoning1

filtered_df$MSZoning_V <- ifelse(data$MSZoning %in% "RL",1,0)
filtered_df<- subset(filtered_df, select = -c(MSZoning))



## Neighborhood
unique(filtered_df$Neighborhood)
Neighborhood1<- filtered_df %>% group_by(Neighborhood) %>% summarise(Count=n())

filtered_df$Neighborhood_V <- ifelse(data$Neighborhood %in% c("NAmes","CollgCr","OldTown","Edwards"),1,0)
filtered_df<- subset(filtered_df, select = -c(Neighborhood))


## Building Type
filtered_df$BldgType_V <- ifelse(filtered_df$BldgType %in% c('1Fam'),1,0)
filtered_df<- subset(filtered_df, select = -c(BldgType))

## Central Air
filtered_df$CentralAir <- ifelse(filtered_df$CentralAir=='Y',1,0 )

## Functional
Functional1<- filtered_df %>% group_by(Functional) %>% summarise(Count=n())
filtered_df$Functional_V <- ifelse(data$Functional %in% "Typ",1,0)

filtered_df<- subset(filtered_df, select = -c(Functional))

## Total # of Rooms

filtered_df$Rms_adj = log(filtered_df$TotRmsAbvGrd)
filtered_df<- subset(filtered_df, select = -c(TotRmsAbvGrd))

# Plot correlation matrix for continuous variables

int_data = unlist(lapply(filtered_df,is.integer))

continuous_only = filtered_df[,int_data]

# we can see that low density residential areas are typically higher price and has higher variation as indicated by the interquartile range

# correlation matrix

corr = cor(continuous_only)

corrplot(corr,type="lower",order="FPC",col=colorRampPalette(c("darkred", "white", "olivedrab"))(20))

# Subset main dataset into training and testing datasets 

##Removing Outliers
set.seed(1234)

filtered_df <- filtered_df[-c(1299,524),]

sample_data<- sample(nrow(filtered_df), nrow(filtered_df)*.7)
training <- filtered_df[sample_data,]
testing <- filtered_df[-sample_data,]

# Best Subsets regression given all variables and using a backward step-wise method
regsub <- regsubsets(SalePriceLog~.,training, method = "backward", nvmax = 17)
regsubsummary <- summary(regsub)
regsubsummary$rsq ## checking rsq value

# Plotting rsq to select number of variables

rsq_val = regsubsummary$rsq
num = c(1:17)
bs_data = data.frame(rsq_val,num)

rsq_plot = ggplot(bs_data,aes(x=num,y=rsq_val))+
  geom_line()+
  geom_point(color = "darkgreen",size=4)+
  theme_minimal()+
  ggtitle("Best Subsets Regression R-Squared vs # of Variables")+
  theme(plot.title=element_text(vjust=2,hjust=.5,face="bold",size=16),panel.background = element_rect(fill="cornsilk"),
        axis.text=element_text(size=12,color="gray37"),axis.title = element_text(size=14))+
  ylab("R-Squared")+
  xlab("Number of Variables")


rss_val = regsubsummary$rss
num = c(1:17)
bs2_data = data.frame(rss_val,num)

rss_plot = ggplot(bs2_data,aes(x=num,y=rss_val))+
  geom_line()+
  geom_point(color = "darkred",size=4)+
  theme_minimal()+
  ggtitle("Best Subsets Regression RSS vs # of Variables")+
  theme(plot.title=element_text(vjust=2,hjust=.5,face="bold",size=16),panel.background = element_rect(fill="cornsilk"),
        axis.text=element_text(size=12,color="gray37"),axis.title = element_text(size=14))+
  ylab("Residual Sum of Squares")+
  xlab("Number of Variables")

ggarrange(rss_plot,rsq_plot)

mat <- regsubsummary$outmat
rownames(mat) <- 1:nrow(mat)
mat <- mat[1:11,]
matL <- ifelse(mat == " ",FALSE,TRUE)
matLT <- as.data.frame(t(matL))
matLT$names = rownames(matLT)
var_keep = matLT[matLT$`11` == TRUE,12]

var_keep = append(var_keep,"SalePriceLog")

#Model with log transformed response variable

training1 = training[,var_keep]

lm.fitt = lm(SalePriceLog~.,data=training1) ##With Outliers
summary(lm.fitt)

lm.fittt = lm(SalePriceLog~.,data=training1)  ##Without Outliers

vif_df<- data.frame(vif(lm.fittt))
vif_df<-cbind(VariableName = rownames(vif_df), vif_df)
rownames(vif_df)<- 1:nrow(vif_df)
colnames(vif_df) <- c('Variable Name','VIF')
vif_df

summary(lm.fittt)

# Diagnostic plots
#layout(matrix(c(1,2,3,4),2,2))
plot(lm.fitt) #With Outliers

#layout(matrix(c(1,2,3,4),2,2))
plot(lm.fittt)


# undo layout
par(mfrow=c(1,1))

#predict data using subset of train data

testing$predicted = predict(lm.fittt,testing, type='response')
testing$residual = abs(exp(testing$SalePriceLog) - exp(testing$predicted))

# take only needed columns

keep1 = c("SalePriceLog","predicted","residual")

tested_data = testing[,keep1]

# Calculate median absolute deviation
mad_test = median(tested_data$residual)
mad_test

## Random Forest

library(randomForest)
set.seed(12345)

rf <- randomForest(SalePriceLog~.,data = training, mtry=5,importance=TRUE)

rf 
importance(rf)
varImpPlot(rf,main="Random Forest Variable Importance Plot")

testing$test_rf_pred <- predict(rf,testing)

testing$residualrf = abs(exp(testing$SalePriceLog) - exp(testing$test_rf_pred))

# take only needed columns

keeps2 = c("SalePriceLog","test_rf_pred","residualrf")

tested_data_rf = testing[keeps2]

# Calculate mean absolute deviation
mad_test_rf = median(tested_data_rf$residualrf)
mad_test_rf








