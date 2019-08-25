library(lubridate)										
library(padr)										
library(forecast)										
library(tstools)										
library(dplyr)										
library(plyr)										
library(DescTools)										
library(tidyr)										
library(purrr)										
library(reshape)										
library(data.table)										
library(smooth)										
library(thief)										
library(tsintermittent)										


#Import the file										
#Select certain COLUMNS, drop everything else 										
fres_all_rawM1 = subset(fres_all_rawM,select = c(CLNDR_DT,HR_NBR,CLNDR_WK_DAY,SKU_ITEM_NBR,BSNS_UNIT_CD,LNG_DESC,QTY))										
#CONVERT IT INTO A DATAFRAME										
fres_allM = data.frame(fres_all_rawM1)										
#Convert dates into DATE FORMAT										
fres_allM$CLNDR_DT<- dmy(fres_allM$CLNDR_DT)
#Convert all negative sales to zero										
fres_allM$QTY[fres_allM$QTY<0] <- 0										
#Hold_out										
hold_M1 = 20										
									
										
############################## Store split ####################################										
stores = c("46","1","101","133","179","240","328","563")										
store_file_names = c("fres_046","fres_001","fres_101","fres_133","fres_179","fres_240","fres_328","fres_563")										
for (i in 1:8)										
{										
  
  assign(store_file_names[i],subset(fres_allM, BSNS_UNIT_CD==stores[i]))										
  
  
}										
store_list = list(fres_046,fres_001,fres_101,fres_133,fres_179,fres_240,fres_328,fres_563)										
#Count the number of SKUs										
length(unique(fres_001$SKU_ITEM_NBR))										
skus_M = unique(fres_001$SKU_ITEM_NBR)										
write.table(skus_M,file = "skus_M001.csv",sep=",",row.names=FALSE)	

############################## SKU Split FOR STORE1 ##########################										
by_skus = c("108317579","108317616","108317675","108317843","108393002","108413982","108414011","108414053","108428826","108428877","108428906","108428949","108433668","108433692","108433730","108433810","108433895","108433967","108433991","108434054","108434089","108434118","108434142","108434177","108435209","108435233","108446805","108456755","108480392","108502213","108908942","108909056","108909662","108909726","109322417",
            "109322484",	"109322530",	"109331911",	"109389638",	"109515497",	"109515551",	"109515606",	"109547173",	"109547270",	"109789949",	"109798714",	"109811421",	"109811456",	"109811481"
)



sku_file_names = c("fres_240_1","fres_240_2","fres_240_3","fres_240_4","fres_240_5",										
                   "fres_240_6","fres_240_7","fres_240_8","fres_240_9","fres_240_10",										
                   "fres_240_11","fres_240_12","fres_240_13","fres_240_14","fres_240_15",										
                   "fres_240_16","fres_240_17","fres_240_18","fres_240_19","fres_240_20","fres_240_21","fres_240_22","fres_240_23","fres_240_24","fres_240_25",										
                   "fres_240_26","fres_240_27","fres_240_28","fres_240_29","fres_240_30", "fres_240_31","fres_240_32","fres_240_33","fres_240_34","fres_240_35", "fres_240_36", "fres_240_37", "fres_240_38", "fres_240_39",										
                   "fres_240_40", "fres_240_41","fres_240_42","fres_240_43","fres_240_44","fres_240_45", "fres_240_46", "fres_240_47", "fres_240_48", "fres_240_49",
                   "fres_240_50","fres_240_51","fres_240_52","fres_240_53","fres_240_54","fres_240_55")

for (i in 1:100){										
  assign(sku_file_names[i], data.frame(subset(fres_240,SKU_ITEM_NBR== by_skus[i])))										
  
}	


########################### Total Sales #######################################										
sku_file_240 = list (fres_240_1,fres_240_2,fres_240_3,fres_240_4,fres_240_5,										
                     fres_240_6,fres_240_7,fres_240_8,fres_240_9,fres_240_10,										
                     fres_240_11,fres_240_12,fres_240_13,fres_240_14,fres_240_15,										
                     fres_240_16,fres_240_17,fres_240_18,fres_240_19,fres_240_20,fres_240_21,fres_240_22,fres_240_23,fres_240_24,fres_240_25,										
                     fres_240_26,fres_240_27,fres_240_28,fres_240_29,fres_240_30,fres_240_31,fres_240_32,fres_240_33, fres_240_34, fres_240_35,fres_240_36, fres_240_37, fres_240_38,  fres_240_39,  fres_240_40, fres_240_41,fres_240_42,fres_240_43,fres_240_44,fres_240_45, fres_240_46, fres_240_47, fres_240_48,								
                     fres_240_49,	fres_240_50,fres_240_51,fres_240_52,fres_240_53,fres_240_54,fres_240_55)
############################# Change Column Names ############################										
nms <- c("CLNDR_DT","HR_NBR","CLNDR_WK_DAY","SKU_ITEM_NBR","BSNS_UNIT_CD","LNG_DESC", "QTY")										
for (i in 1:100){										
  
  assign(sku_file_names[i],setnames(data.frame(sku_file_240[i]),nms))										
  
}										

sku_file_240 = list (fres_240_1,fres_240_2,fres_240_3,fres_240_4,fres_240_5,										
                     fres_240_6,fres_240_7,fres_240_8,fres_240_9,fres_240_10,										
                     fres_240_11,fres_240_12,fres_240_13,fres_240_14,fres_240_15,										
                     fres_240_16,fres_240_17,fres_240_18,fres_240_19,fres_240_20,fres_240_21,fres_240_22,fres_240_23,fres_240_24,fres_240_25,										
                     fres_240_26,fres_240_27,fres_240_28,fres_240_29,fres_240_30,fres_240_31,fres_240_32,fres_240_33, fres_240_34, fres_240_35,fres_240_36, fres_240_37, fres_240_38,fres_240_39,  fres_240_40, fres_240_41,fres_240_42,fres_240_43,fres_240_44,fres_240_45, fres_240_46, fres_240_47, fres_240_48,								
                     fres_240_49,	fres_240_50,fres_240_51,fres_240_52,fres_240_53,fres_240_54,fres_240_55)

############ Find Gross Sales Daily by Aggregating Hourly sales ##############										
thickg <- function(x) ddply(x, .(CLNDR_DT), summarize, GQTY = sum(QTY))										
##### Apply function to LIST of SKUs #################################################
allthickg <- lapply(sku_file_240,thickg)										
gsales_names = c("g1","g2","g3","g4","g5","g6","g7","g8","g9","g10","g11","g12","g13","g14","g15", "g16","g17","g18","g19","g20", "g21","g22","g23","g24","g25", "g26","g27","g28","g29","g30","g31","g32","g33", "g34","g35","g36","g37","g38","g39", "g40","g41","g42","g43","g44","g45","g46","g47","g48","g49","g50","g51","g52","g53","g54","g55","g56","g57","g58","g59")										

for(i in 1:100)										
{										
  assign(gsales_names[i], data.frame(Reduce(cbind, allthickg[i])))										
}										

glist = list(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,g32,g33,g34,g35,g36,g37,g38,g39, g40,g41,g42,g43,g44,g45,g46,g47,g48,g49,g50,g51,g52,g53,g54,g55)										
for(i in 1:100)										
{										
  assign(gsales_names[i], pad(data.frame(glist[i]),by= "CLNDR_DT")%>% fill_by_value(value=0))										
}										

glist = list(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,g32,g33,g34,g35,g36,g37,g38,g39, g40,g41,g42,g43,g44,g45,g46,g47,g48,g49,g50,g51,g52,g53,g54,g55)										
################################################################################### CUMULATIVE REGRESSION #############################################################										
#################### Merge all BY Forecast files ##################				

by_all = rbind(fsct1,fsct2,fsct3,fsct4)
#################### Parse DATES ##################										
by_all$FILE_DT_KEY = ymd(by_all$FILE_DT_KEY)										
nms = c("CLNDR_DT","DEPT_CD","SKU_ITEM_NBR","BSNS_UNIT_CD","BY")										
by_all = setnames(by_all,nms)										
#################### REMOVE Duplicate Rows ##################										
by_all_new = by_all[!duplicated(by_all[c("CLNDR_DT","DEPT_CD","SKU_ITEM_NBR","BSNS_UNIT_CD")]),]	

#################### Store split ####################										
by_240 = subset(by_all_new,BSNS_UNIT_CD == "1")	


#################### Select 30 SKUs from the Stores file ###############################										
by_sku_file_names = c("by1",	"by2",	"by3",	"by4",	"by5",	"by6",	"by7",	"by8",	"by9",	"by10",	
                      "by11",	"by12",	"by13",	"by14",	"by15",	"by16",	"by17",	"by18",	"by19","by20","by21","by22",	"by23",	"by24","by25",	"by26",	"by27",	"by28",	"by29","by30","by31","by32","by33", "by34","by35","by36","by37","by38","by39", "by40","by41","by42","by43","by44","by45","by46","by47","by48","by49","by50","by51","by52","by53","by54","by55")


for (i in 1:100){										
  assign(by_sku_file_names[i], subset(by_240,SKU_ITEM_NBR== by_skus[i]))										
}								



###################### 16:00 HOUR SUBSET #####################################										
fres_240_h16 = subset(fres_240,HR_NBR<='15:00:00', select = c(CLNDR_DT,HR_NBR,SKU_ITEM_NBR, QTY))										
############ SKU split for H16 #############################################										
sku_file_names_16 = c("fres16_240_1","fres16_240_2","fres16_240_3","fres16_240_4","fres16_240_5",										
                      "fres16_240_6","fres16_240_7","fres16_240_8","fres16_240_9","fres16_240_10",										
                      "fres16_240_11","fres16_240_12","fres16_240_13","fres16_240_14","fres16_240_15",										
                      "fres16_240_16","fres16_240_17","fres16_240_18","fres16_240_19","fres16_240_20","fres16_240_21","fres16_240_22","fres16_240_23",	"fres16_240_24","fres16_240_25",										
                      "fres16_240_26","fres16_240_27","fres16_240_28","fres16_240_29","fres16_240_30","fres16_240_31","fres16_240_32","fres16_240_33","fres16_240_34","fres16_240_35", "fres16_240_36", "fres16_240_37", "fres16_240_38","fres16_240_39","fres16_240_40", "fres16_240_41","fres16_240_42","fres16_240_43","fres16_240_44","fres16_240_45", "fres16_240_46", "fres16_240_47", "fres16_240_48","fres16_240_49",
                      "fres16_240_50","fres16_240_51","fres16_240_52","fres16_240_53","fres16_240_54","fres16_240_55"
)
for (i in 1:100){										
  assign(sku_file_names_16[i], data.frame(subset(fres_240_h16,SKU_ITEM_NBR== by_skus[i])))										
  
}										
sku_file_list_16 = list(fres16_240_1,fres16_240_2,fres16_240_3,fres16_240_4,fres16_240_5,										
                        fres16_240_6,fres16_240_7,fres16_240_8,fres16_240_9,fres16_240_10,										
                        fres16_240_11,fres16_240_12,fres16_240_13,fres16_240_14,fres16_240_15,										
                        fres16_240_16,fres16_240_17,fres16_240_18,fres16_240_19,fres16_240_20,fres16_240_21,fres16_240_22,fres16_240_23,fres16_240_24,fres16_240_25,										
                        fres16_240_26,fres16_240_27,fres16_240_28,fres16_240_29,fres16_240_30,fres16_240_31,fres16_240_32,fres16_240_33,fres16_240_34, fres16_240_35,fres16_240_36, fres16_240_37, fres16_240_38,fres16_240_39,fres16_240_40, fres16_240_41,fres16_240_42,fres16_240_43,fres16_240_44,fres16_240_45, fres16_240_46, fres16_240_47, fres16_240_48, fres16_240_49,
                        fres16_240_50,fres16_240_51,fres16_240_52,fres16_240_53,fres16_240_54,fres16_240_55				
)


############ Cumulative Sales ######################################										
thickc <- function(x) ddply(x, .(CLNDR_DT), summarize, CQTY = sum(QTY))										
allthickc <- lapply(sku_file_list_16,thickc)										
csales_names = c("c1","c2","c3","c4","c5", "c6","c7","c8","c9","c10",										
                 "c11","c12","c13","c14","c15","c16","c17","c18","c19","c20", "c21","c22","c23","c24","c25","c26","c27","c28","c29","c30","c31","c32","c33", "c34","c35","c36","c37","c38","c39","c40","c41","c42","c43","c44","c45","c46","c47","c48","c49","c50","c51","c52","c53","c54","c55","c56","c57","c58")										
for(i in 1:100)										
{										
  assign(csales_names[i], data.frame(Reduce(cbind, allthickc[i])))										
}										

View(c44)
########################################### FIRST Merge and Insert 0s ###############################################										
sku_file_reg = c("reg1","reg2","reg3","reg4","reg5",										
                 "reg6","reg7","reg8","reg9","reg10",										
                 "reg11","reg12","reg13","reg14","reg15",										
                 "reg16","reg17","reg18","reg19","reg20","reg21","reg22","reg23","reg24","reg25","reg26","reg27","reg28","reg29","reg30", "reg31","reg32","reg33","reg34","reg35","reg36","reg37","reg38","reg39","reg40","reg41","reg42","reg43","reg44","reg45","reg46","reg47","reg48","reg49","reg50","reg51","reg52","reg53","reg54","reg55","reg56","reg57","reg58","reg59","reg60","reg61")										
clist = list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,c41,c42,c43,c44,c45,c46,c47,c48,c49,c50,c51,c52,c53)														
glist = list(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30,g31,g32,g33,g34,g35,g36,g37,g38,g39,g40,g41,g42,g43,g44,g45,g46,g47,g48,g49,g50,g51,g52,g53)										
for(i in 1:100)										
{										
  assign(sku_file_reg[i], merge(clist[i],glist[i], by= "CLNDR_DT"))										
}										

############################################### SECOND MERGE ####################										
bylist = list (by1,by2,by3,by4,by5,by6,by7,by8,by9,by10,by11,by12,by13,by14,by15,by16,by17,by18,by19,by20,by21,by22,by23,by24,by25,by26,by27,by28, by29,by30,by31,by32,by33,by34,by35,by36,by37,by38,by39,by40,by41,by42,by43,by44,by45,by46,by47,by48,by49)

reg_list = list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16,reg17,reg18,reg19,reg20,reg21,reg22,reg23,reg24,reg25,reg26,reg27,reg28,reg29,reg30,reg31,reg32,reg33,reg34,reg35,reg36,reg37,reg38,reg39, reg40,reg41,reg42,reg43,reg44,reg45,reg46,reg47,reg48,reg49) 
for(i in 1:100)										
{										
  assign(sku_file_reg[i], pad(merge(reg_list[i],bylist[i], by= "CLNDR_DT"))%>% fill_by_value(value=0))										
}										
reg_list = list(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8,reg9,reg10,reg11,reg12,reg13,reg14,reg15,reg16,reg17,reg18,reg19,reg20,reg21,reg22,reg23,reg24,reg25,reg26,reg27,reg28,reg29,reg30,reg31,reg32,reg33,reg34,reg35,reg36,reg37,reg38,reg39, reg40,reg41,reg42,reg43,reg44,reg45,reg46,reg47,reg48,reg49)



############ CHECK INTERMITTENCY ###############################################
int_M= lapply (reg_list, function (x) {(nrow(subset(x, GQTY=="0"))/length(x$GQTY))*100})
int_M= data.frame(int_M)
write.table(int_M, file = "int_M240.csv", sep=",", row.names = FALSE,col.names = FALSE)

lengthM= lapply (reg_list, function (x) length(x$GQTY))
lengthM = data.frame(lengthM)
write.table(lengthM, file = "length_M240.csv", sep=",", row.names = FALSE,col.names = FALSE)

############ Regression FINAL CODE###############################################										
pred1 = function (x) predict(lm(GQTY~CQTY + BY, data = x[1:(length(x$GQTY)-hold_M1),]), newdata = data.frame(tail(x[,c(2,7)],hold_M1)))										
all_pred1 =lapply(reg_list, pred1)										
all_pred_df1 = data.frame(Reduce(cbind, all_pred1)) %>% fill_by_value(value=0)										
################ TEST files from GROSS SALES file ###############################										
hold = function (x) data.frame(tail(x$GQTY,hold_M1))										
all_test_reg = lapply(reg_list,hold)										
all_test_reg = data.frame(all_test_reg)%>% fill_by_value(value=0)										
############################## Find Naive-common for all models##############################										
naivef <- function(x) naive((x$GQTY[1:(length(x$GQTY)-hold_M1)]),h=hold_M1)$mean										
all_naive_reg <- lapply(reg_list,naivef)										
all_naive_reg <- data.frame(all_naive_reg)										

############################## SystemForecast 30 days##############################										
hold = function (x) data.frame(tail(x$BY,hold_M1))										
all_by_reg = lapply(reg_list,hold)										
all_by_reg = data.frame(all_by_reg)										
############################### NAIVE Accuracy Function for Mean Absolute Error #######################										
reg_acc1 = matrix(nrow = 1500, ncol = 5)										
acc_reg_naive1 = matrix (nrow = 1500, ncol = 7)										
reg_relmae1 = matrix(nrow=1500,ncol=1)										
for (i in 1:1500){										
  reg_acc1[i,] = accuracy (all_test_reg[,i],all_pred_df1[,i])										
  acc_reg_naive1[i,] = accuracy (all_test_reg[,i],all_naive_reg[,i])										
  reg_relmae1[i,] = reg_acc1[i,3]/acc_reg_naive1[i,3]																				
}										
############################### BY Accuracy Function for Mean Absolute Error #######################										
reg_acc1 = matrix(nrow = 1500, ncol = 5)										
acc_reg_by1 = matrix (nrow = 1500, ncol = 5)										
reg_relmae_by1 = matrix(nrow=1500,ncol=1)										
for (i in 1:100){										
  reg_acc1[i,] = accuracy (all_test_reg[,i],all_pred_df1[,i])										
  acc_reg_by1[i,] = accuracy (all_test_reg[,i],all_by_reg[,i])										
  reg_relmae_by1[i,] = reg_acc1[i,3]/acc_reg_by1[i,3]										
}										



############################################################################### PLOT Last 15 Days ###############################################################										
plot_names = c ("plot1","plot2","plot3","plot4","plot5","plot6","plot7","plot8","plot9","plot10","plot11","plot12","plot13","plot14","plot15","plot16","plot17","plot18","plot19","plot20","plot21","plot22")										
for(i in 1:100)										
  
{										
  assign(plot_names[i], data.frame(cbind(all_test_reg[,i], all_by_reg[,i],all_pred_df1[,i])))										
}										

names = c("Actual", "Blue Yonder", "Revised")										
setnames(plot5,names)
sku_plot = paste("Store - 240,", "SKU -", reg5[1,5])
matplot(plot5, type = c("l"),col = c("black","blue","red"),xlab="Days", ylab = "Units", lty = 1:3, main = sku_plot)
legend("topleft",legend = colnames(plot5),col = c("black","blue","red"),lty=1:3)

plot(reg5$GQTY,type="l")

############################################################################### ONE CODE TO BIND THEM ALL #######################################################										
M240_all_models = cbind(reg_relmae_by1,reg_relmae1)										

write.table(M240_all_models,file="M_240_all_models_BY.csv", sep = ",")										

