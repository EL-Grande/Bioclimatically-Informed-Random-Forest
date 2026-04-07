
#My_Exxp<-function(){
  #set.seed(2024)
library(lubridate)
source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get   datasets
source("D:/PhD Material/DataBw/ValOps.R")# Call the function to run the models
source("D:/PhD Material/DataBw/createBoxPlot.R")# Function   for Box Plot    
source("D:/PhD Material/DataBw/createBarChart.R")# Function for BarChart
source("D:/PhD Material/DataBw/myDataset.R")

Lista <- DataOps("2023-01-01", "2023-03-31") 

VIp1 <- c("TS","ST","HI","CP","Twc","TE","AT","Tap","RH",
          "BP","WSX","BV","SR","WD","HD","WB","WS","WSm","LW")
VIp2 <- c("TS","RH","ST","BV","WSX","HI","WB","HD","SH","Tap",
          "Twc","AT","CP","DP","TE","BP","WSm","SR","LW")
VIp3 <- c("TS","RH","ST","HI","WB","DP","AT","Twc",
          "TE","BP","Tap","HD","BV","WSX","WS","WD","SR","WSm","LW")
VIp4<- c("TS","ST","RH","DP","WB","HI","Tap","BP","CP","TE","SR",
        "WSX","SRE","BV","SH","Twc","WD","WSm","LW")

length(VIp4)

################################################################################
#                             WITH ALL PRIORS                                  #
################################################################################
Raw_With_Priors <- Expe1(as.data.frame(Lista$dataset1))
 
D1 <- subset(Lista$dataset1, select = c(-18,-19,-20,-21,-22,-23,-24))
Raw_Without_VI_Priors <- Expe1(as.data.frame(D1))

D1A <- Lista$dataset1[, VIp1, drop = FALSE]

Raw_With_VI_Priors <- Expe1(as.data.frame(D1A))

#*******************************************************************************
Magnitude_S_With_Priors <- Expe1(as.data.frame(Lista$dataset2))

D2 <- subset(Lista$dataset2, select = c(-18,-19,-20,-21,-22,-23,-24))
Magnitude_S_Without_Priors <- Expe1(as.data.frame(D2))

D2A <- Lista$dataset2[, VIp2, drop = FALSE]

Magnitude_S_With_VI_Priors <- Expe1(as.data.frame(D2A))

#*******************************************************************************
Min_Max_S_With_Priors <- Expe1(as.data.frame(Lista$dataset3))

D3 <- subset(Lista$dataset3, select = c(-18,-19,-20,-21,-22,-23,-24))
Min_Max_S_Without_Priors <- Expe1(as.data.frame(D3))

D3A <- Lista$dataset3[, VIp3, drop = FALSE]

Min_Max_S_With_VI_Priors <- Expe1(as.data.frame(D3A))


#*******************************************************************************
Z_Score_With_Priors <- Expe1(as.data.frame(Lista$dataset4))

D4 <- subset(Lista$dataset4, select = c(-18,-19,-20,-21,-22,-23,-24))
Z_Score_Without_Priors <- Expe1(as.data.frame(D4))

D4A <- Lista$dataset4[, VIp4, drop = FALSE]

Z_Score_With_VI_Priors <- Expe1(as.data.frame(D4A))

#*******************************************************************************
#*
################################################################################
#           Team Scatter Plot Training
train_scatter_plot_RawData <- Raw_With_Priors$train_scatter_plot
test_scatter_plot_RawData <- Raw_With_Priors$test_scatter_plot
ScaterFin1 <- grid.arrange(train_scatter_plot_RawData,test_scatter_plot_RawData, ncol = 2)


#Raw_With_Priors (R^2 = 0.8, MAE = 7, CS = 1, SS = 0.93 and KLD = 0 for training)
#Raw_With_Priors (for Validation: R^2 = 0.8, MAE = 4.9, CS = 1, SS = 0.93, KLD = 0)

train_scatter_plot_Magnitude_S_With_Priors <- Magnitude_S_With_Priors$train_scatter_plot
test_scatter_plot_Magnitude_S_With_Priors <- Magnitude_S_With_Priors$test_scatter_plot
ScaterFin2 <- grid.arrange(train_scatter_plot_Magnitude_S_With_Priors,test_scatter_plot_Magnitude_S_With_Priors, ncol = 2)


#Magnitude_S_With_Priors (R^2 = 0.8, MAE = 0, CS = 1, SS = 0.94 and KLD = 0 for training)
#Magnitude_S_With_Priors (R^2 = 0.8, MAE = 0, CS = 1, SS = 0.93, KLD = 0, for Validation)

train_scatter_plot_RawData_With_VI_Priors <- Raw_With_VI_Priors$train_scatter_plot
test_scatter_plot_RawData_With_VI_Priors <- Raw_With_VI_Priors$test_scatter_plot
ScaterFin3 <- grid.arrange(train_scatter_plot_RawData_With_VI_Priors,test_scatter_plot_RawData_With_VI_Priors, ncol = 2)


#Raw Data with VI Priors(R^2 = 0.9, MAE = 3.8, CS = 1, SS = 0.96 and KLD = 0 for training)
#Raw Data with VI Priors (for Validation: R^2 = 0.8, MAE = 4.7, CS = 1, SS = 0.92, KLD = 0)

train_scatter_plot_Magnitude_S_With_VI_Priors <- Magnitude_S_With_VI_Priors$train_scatter_plot
test_scatter_plot_Magnitude_S_With_VI_Priors <-  Magnitude_S_With_VI_Priors$test_scatter_plot
ScaterFin4 <- grid.arrange(train_scatter_plot_Magnitude_S_With_VI_Priors,
                           test_scatter_plot_Magnitude_S_With_VI_Priors, ncol = 2)


#Magnitude_S_With_VI_Priors (R^2 = 0.9, MAE = 0, CS = 1, SS = 0.96 and KLD = 0 for training)
#Magnitude_S_With_VI_Priors (for Validation: R^2 = 0.8, MAE = 0, CS = 1, SS = 0.92, KLD = 0)


train_scatter_plot_RawData_Without_Priors <- Raw_Without_VI_Priors$train_scatter_plot
test_scatter_plot_RawData_Without_Priors <- Raw_Without_VI_Priors$test_scatter_plot
ScaterFin5 <- grid.arrange(train_scatter_plot_RawData_Without_Priors,test_scatter_plot_RawData_Without_Priors, ncol = 2)


#Raw_Without_VI_Priors (R^2 = 0.7, MAE = 7, CS = 1, SS = 0.92 and KLD = 0 for training)
#Raw_Without_VI_Priors (for Validation: R^2 = 0.7, MAE = 7.9, CS = 1, SS = 0.88, KLD = 0)

train_scat_plot_Mag_S_Without_VI_Priors <- Magnitude_S_Without_Priors$train_scatter_plot
test_scat_plot_Mag_S_Without_VI_Priors <- Magnitude_S_Without_Priors$test_scatter_plot
ScaterFin6 <- grid.arrange(train_scat_plot_Mag_S_Without_VI_Priors,
                           test_scat_plot_Mag_S_Without_VI_Priors, ncol = 2)


#Magnitude_S_Without_Priors (R^2 = 0.8, MAE = 0, CS = 1, SS = 0.92 and KLD = 0 for training)
#Magnitude_S_Without_Priors (for Validation: R^2 = 0.7, MAE = 0, CS = 1, SS = 0.88, KLD = 0)

################################################################################



################################################################################
#           Team Loss Functions (Returns a Column Vector)
print(cbind(Raw_With_Priors$loss_dataL,subset(Magnitude_S_With_Priors$loss_dataL, select = c(-1)),
            subset(Min_Max_S_With_Priors$loss_dataL, select = c(-1)),subset(Z_Score_With_Priors$loss_dataL, select = c(-1)),
            Raw_With_VI_Priors$loss_dataL,subset(Magnitude_S_With_VI_Priors$loss_dataL, select = c(-1)),
            subset(Min_Max_S_With_VI_Priors$loss_dataL, select = c(-1)),subset(Z_Score_With_VI_Priors$loss_dataL, select = c(-1)),
            Raw_Without_VI_Priors$loss_dataL,subset(Magnitude_S_Without_Priors$loss_dataL, select = c(-1)),
            subset(Min_Max_S_Without_Priors$loss_dataL, select = c(-1)),subset(Z_Score_Without_Priors$loss_dataL, select = c(-1))
            
))
################################################################################


################################################################################
# Compute Kendall Tau correlation matrix
#df1 <- Lista$dataset1 %>% select(-TS)
# cor_matrix1 <- cor(df1, method = "kendall")
# cor_matrix1[is.na(cor_matrix1)] <- 0 # Replace missing values with zero

# df2 <- Lista$dataset2 %>% select(-TS)
# cor_matrix2 <- cor(df2, method = "kendall")
# cor_matrix2[is.na(cor_matrix2)] <- 0 # Replace missing values with zero
 
#cord1 <- corrplot(cor_matrix1, method = "ellipse", type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black", col = "white")
 
#corrplot(cor_matrix, method = "ellipse", type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black", col = "white")

#cord2 <- corrplot(cor_matrix2, method = "ellipse", type = "full", tl.col = "black", tl.srt = 45, addCoef.col = "black", col = "white")
#cord1
#cord2


ALL_Priors_VIP <- function(){
  ST <- c(15.2,14.0,14.9,16.7)
  sum(ST)/4
  
  
  RH <- c(12.2,14.5,15.0,16.5)
  sum(RH)/4
  
  Tap <- c(12.3,12.2,12.5,14.2)
  sum(Tap)/4
  
  
  HI <- c(13.6,12.7,14.8,15.6)
  sum(HI)/4
  
  Twc <- c(13.0,12.2,13.3,12.4)
  sum(Twc)/4
  
  CP <- c(13.6,12.1,10,13.5)
  sum(CP)/4
  
  TS <- c(11.6,13.2,13.2,14.7)
  sum(TS)/4
  
  
  TE <- c(12.5,12.0,12.8,13.4)
  sum(TE)/4
  
  AT <- c(12.3,12.1,13.4,10)
  sum(AT)/4
  
  
  
  
  BP <- c(12.0,11.5,12.6,14.0)
  sum(BP)/4
  
  WSX <- c(11.9,12.9,11.8,12.8)
  sum(WSX)/4
  
  BV <- c(11.9,13.9,12.0,12.5)
  sum(BV)/4
  
  SR <- c(11.8,11.0,11.4,13.0)
  sum(SR)/4
  
  
  WD <- c(11.6,10,11.7,12.1)
  sum(WD)/4
  
  HD <- c(11.1,12.6,12.2,10)
  sum(HD)/4
  
  WB <- c(11.0,12.7,14.7,15.8)
  sum(WB)/4
  
  WS <- c(10.8,10,11.8,10)
  sum(WS)/4
  
  WSm <- c(10.4,11.4,10.7,12.1)
  sum(WSm)/4
}
ALL_Priors_VIP()

# Omit colors (use black) and slant the labels
#  p0 <- corrplot(cor_matrix, method = c("number"), col = "black", tl.col = "black",
#     bg = "white", number.cex = 0.7, tl.cex = 0.7, tl.srt = 45)


################################################################################
#   CALCULATE THE VIPs
################################################################################
VIP_With_Priors_Times_4 <- grid.arrange(Raw_With_Priors$vip1, Magnitude_S_With_Priors$vip1, 
                                        Min_Max_S_With_Priors$vip1, Z_Score_With_Priors$vip1, 
                                        ncol = 4)
#********************************************************************************************
VIP_With_Out_Priors_Times_4 <- grid.arrange(Raw_Without_VI_Priors$vip1, Magnitude_S_Without_Priors$vip1, 
                                            Min_Max_S_Without_Priors$vip1, Z_Score_Without_Priors$vip1, 
                                                  ncol = 4)
#********************************************************************************************
VIP_With_VIP_Priors_Times_4 <- grid.arrange(Raw_With_VI_Priors$vip1, Magnitude_S_With_VI_Priors$vip1, 
                                            Min_Max_S_With_VI_Priors$vip1, Z_Score_With_VI_Priors$vip1, 
                                            ncol = 4)
################################################################################
 
################################################################################
# Access and display individual objects from the list
#           Team Scatter Plot
print(Raw_With_Priors$p1) # Scatter Plot Training
print(Magnitude_S_With_Priors$p1) # Scatter Plot Training
print(Min_Max_S_With_Priors$p1) # Scatter Plot Training
print(Z_Score_With_Priors$p1) # Scatter Plot Training
################################################################################

################################################################################
#           Team Variable Importance Plot
print(Raw_With_Priors$vip1) # VIP Training
print(Magnitude_S_With_Priors$vip1) # VIP Training
print(Min_Max_S_With_Priors$vip1) # VIP Training
print(Z_Score_With_Priors$vip1) # VIP Training


################################################################################
#############################################################################
#           Team P
###aepenrtial Dependence Plot
print(Raw_With_Priors$pd1) # Partial Dependence Plot Training
print(Magnitude_S_With_Priors$pd1) # Partial Dependence Plot Training
print(Min_Max_S_With_Priors$pd1) # Partial Dependence Plot Training
print(Z_Score_With_Priors$pd1) # Partial Ddence Plot Training
################################################################################


################################################################################
#           Team Dygraph Plot
print(Raw_With_Priors$dyG) # Scatter Plot Training
print(Magnitude_S_With_Priors$dyG) # Scatter Plot Training
print(Min_Max_S_With_Priors$dyG) # Scatter Plot Training
print(Z_Score_With_Priors$dyG) # Scatter Plot Training
################################################################################

################################################################################
#         Team  Residuals 
print(Raw_With_Priors$residuals) # Scatter Plot Training
print(Magnitude_S_With_Priors$residuals) # Scatter Plot Training
print(Min_Max_S_With_Priors$residuals) # Scatter Plot Training
print(Z_Score_With_Priors$residuals) # Scatter Plot Training
################################################################################




################################################################################
#         PRINT LOSS FUNCTION FOR EACH (4) EXPERIMENTS                         #
# 1. With All Data and Including All Priors                                    #
# 2. Without Priors                                                            #
# 3. Using VI Priors Only                                                      #
#-------------------------------------------------------------------------------




################################################################################
#residuals_listB <- list(
#  "Raw Data" =  Raw_With_Priors$residuals,
#  "Magnitude" = Magnitude_S_With_Priors$residuals,
#  "Min-Max" = Min_Max_S_With_Priors$residuals,
#  "Z-Score" = Z_Score_With_Priors$residuals
#)
################################################################################


################################################################################
# Create a box plot with multiple boxes using the function
#BoxPlts <- createBoxPlot(residuals_listB, c("Raw Data", "Magnitude","Min-Max","Z-Score"),
#                         "Residuals Box Plots", "Residuals")
#BoxPlts
################################################################################


################################################################################
# Create a bar chart with multiple bars using the function
#BarChart <- createBarChart(residuals_listB, c("Raw Data", "Z-Score", "Min-Max", "Magnitude"),
#                           "Residuals Bar Chart", "Residuals")
#BarChart
################################################################################

 