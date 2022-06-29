setwd("C:/Users/kenll/Desktop/lecture notes/R")

source('Q2.R')
response_file = "example_response_variable.csv"
data_mat_file = "example_explanatory_variables.csv"


# read the file
data_mat = read.csv(data_mat_file, header = T ) #,colClasses = "factor"
response = read.csv(response_file, header = T)

# make response not an array
if (is.matrix(response) | is.data.frame(response)) {response = response[,1]}

#replace string data into numerical variables
data_mat$OligoDensity[data_mat$OligoDensity == "Low"] <- 0 #0.5 pmol cm^-2
data_mat$OligoDensity[data_mat$OligoDensity == "High"] <- 1 #1 pmol cm^-2
data_mat$OligoDensity[data_mat$OligoDensity == "VHigh"] <- 2 #2 pmol cm^-2
data_mat$OligoDensity<- as.numeric(data_mat$OligoDensity)

data_mat$Lipid[data_mat$Lipid == "80%"] <- 0.8 
data_mat$Lipid[data_mat$Lipid == "100%"] <- 1
data_mat$Lipid<- as.numeric(data_mat$Lipid)

#set values of discrete variables into numerical for multiple linear regression
data_mat$Attachment[data_mat$Attachment == "Chol"] <- 0 
data_mat$Attachment[data_mat$Attachment == "DOPE"] <- 1 
data_mat$Attachment<- as.numeric(data_mat$Attachment)

data_mat$Backbone[data_mat$Backbone == "PO"] <- 0 
data_mat$Backbone[data_mat$Backbone == "PS"] <- 1 
data_mat$Backbone<- as.numeric(data_mat$Backbone)

df_res <- data.frame(Outp = response)#,Size = data_mat$Size
total <- merge(df_res,data_mat)#,by="Size"

# Multiple Linear Regression Example
fity = lm(Outp ~ Size + Lipid + Orientation + Backbone + PeptideDensity + OligoDensity + Attachment + OligoConc,data = total)#Size + Lipid + Orientation + Backbone + PeptideDensity + OligoDensity + Attachment + 
summary(fity) # show results
fity$coefficients
LR_pred = predict(fity,data_mat)

LRQ2 = Q2(LR_pred,df_res,df_res)

plot(data_mat$Size,df_res$Outp)
abline(fity)
plot(data_mat$Size,LR_pred)
plot(fity)

