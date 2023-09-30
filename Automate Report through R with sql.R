rm(list=setdiff(ls(), c("i","status","d.sendmail","filepaths","timings")))
setwd("D:\\dir\\")
library(RMySQL)
library(lubridate)
library(excel.link)
library(BI)
library(dplyr)
library(data.table)
library(tcltk)
library(data.table)
library(dplyr)

Start_date <- Sys.Date()

report_date <- today()-1
start_date_1 <- report_date
end_date_1 <- today()

start_date <- paste("'",Sys.Date()-02, "00:00:00", "'")
end_date <- paste("'",Sys.Date()-02, "23:59:59", "'")

conn <- dbConnect(MySQL(), user = "user",db="db3",host = "3.18.0.12" , port=3306,password = "password@#")


D1 <- dbGetQuery(conn,paste("select * from table 
 ##fulfil your query for fetching data 

where ss.added_on >= ",start_date,"
and ss.added_on < ",end_date,";"))



dbDisconnect(conn)

######after fetch your data is store in D1#####

#manipulation through R code ############

D1$Added_on <- ymd_hms(D1$Added_on)
D1$First_Attempt_Date <- ymd_hms(D1$First_Attempt_Date)

D1$Time_Diff <- (as.numeric(D1$First_Attempt_Date - D1$Added_on))

D1$Neutralised <- ifelse(D1$Time_Diff <=1200 & D1$Shipment_Status == 'Pickup Cancelled by Customer',0,1)

D1$Neutralised[is.na(D1$Neutralised)] <- 1

D1$Cancelled <- ifelse(D1$Shipment_Status == 'Pickup Cancelled by Customer' & (D1$Neutralised !=0) ,1,0)
table(D1$Shipment_Status)
#D1$Neutralised[is.na(D1$Neutralised)] <- 1

D1$Manifest <- ifelse(!is.na(D1$Added_on) , 1,0)
#D1$drs_date <- ifelse(!is.na(D1$drs_closed_on) , 1,0)
D1$Attempt_date <- ifelse(!is.na(D1$First_Attempt_Date) , 1,0)
D1$DC_inscan <- ifelse(!is.na(D1$Inscan_Date), 1,0)
D1$Not_picked <- ifelse(is.na(D1$Inscan_Date),1,0)
D1$QC_Failed <- ifelse(D1$Current_reason == 'Reverse Pickup Failed because of QC checks failed',1,0)
D1$QC_Failed[is.na(D1$QC_Failed)] <- 0

#D1$Neutralised  <- ifelse(D1$`ageing in mins` <=20 & D1$status =="Pickup Cancelled by Customer", 0,1)
table(D1$Current_reason)
table((D1$QC_Failed))
table(D1$Cancelled)
D1$Neutralised = ifelse(is.na(D1$Neutralised),1,D1$Neutralised)
D1$Cancelled = ifelse(is.na(D1$Cancelled),0,D1$Cancelled) 

#colnames(pivot)
#####pivot table 

pivot <- D1 %>% group_by(Region,State,Origin_SC) %>% summarise('Manifest'=sum(Manifest),'Neutralised' =sum(Neutralised),'Cancel'=sum(Cancelled),'QC_Failed'=sum(QC_Failed),'Picked' = sum(DC_inscan),'Not_picked' = sum(Not_picked))




a <- read.csv('data.csv')

pivot$FPS_Outscan <- a$FPS_Outscan[match(pivot$Origin_SC,a$Origin_DC)]

pivot$FPS_Pickup <- a$FPS_Pickup[match(pivot$Origin_SC,a$Origin_DC)]

pivot$FPC_Push <- a$FPC_push[match(pivot$Origin_SC,a$Origin_DC)]

pivot$PFC_FPOC <- a$PFC_FPOC[match(pivot$Origin_SC,a$Origin_DC)]

table(D1$status)
#file read from another portal.
data <- read.csv('data.csv')

data$year <- '-2023'

data$Final_Date <- paste0(data$Closed...Working.Date, data$year)

data$Final_Date <- dmy(data$Final_Date)

data$Origin.SC..12 <- as.character(data$Origin.SC..12)
data$DC <- substr(data$Origin.SC..12, start = regexpr("-", data$Origin.SC..12) + 1, stop = nchar(data$Origin.SC..12))


data1 <- subset(data,data$Final_Date == (today()-3))





data_pivot <- data1 %>% group_by(DC) %>% summarise('Incorrect_QC' =sum(Incorrect.QC.Fail),'Incorrect_Upload' =sum(Incorrect.Upload.By.FE),'Total_QC'=sum(Total.Tickets))



pivot$Incorrect_QC_Fail <- data_pivot$Incorrect_QC[match(pivot$Origin_SC,data_pivot$DC)]

pivot$Incorrect_QC_Fail_Upload <- data_pivot$Incorrect_Upload[match(pivot$Origin_SC,data_pivot$DC)]

pivot$Incorrect_total_qc <- data_pivot$Total_QC[match(pivot$Origin_SC,data_pivot$DC)]






table(data$Closed...Working.Date)

#another data read for qc pass
data2 <- read.csv('Data.csv')

data2$year <- '-2023'

data2$Final_Date <- paste0(data2$Closed...Working.Date, data2$year)

data2$Final_Date <- dmy(data2$Final_Date)

data2$Origin.SC..12 <- as.character(data2$Origin.SC..12)
data2$DC <- substr(data2$Origin.SC..12, start = regexpr("-", data2$Origin.SC..12) + 1, stop = nchar(data2$Origin.SC..12))

data3 <- subset(data2,data2$Final_Date == (today()-3))

################QC pass ##################################

data3_pivot <- data3 %>% group_by(DC) %>% summarise('Total' =sum(Total.Tickets),'incorrect qc pass' =sum(Incorrect.QC.Pass))

#data3_pivot$incorrect_qc_perc_l3 <- paste(round(100*data3_pivot$`incorrect qc pass`/data3_pivot$Total,0),"%",sep = "")

# Now you have the data from the Excel file in the 'data' variable

pivot$L3_Incorrect_Qc_Pass <- data3_pivot$`incorrect qc pass`[match(pivot$Origin_SC,data3_pivot$DC)]
pivot$L3_Total <- data3_pivot$Total[match(pivot$Origin_SC,data3_pivot$DC)]

pivot[is.na(pivot)] <- 0

Region_State <- pivot %>% group_by(Region, State) %>% summarise(across(c(Manifest,Neutralised,Cancel,QC_Failed,Picked,Not_picked,FPS_Outscan,FPS_Pickup,FPC_Push,PFC_FPOC,Incorrect_QC_Fail,Incorrect_QC_Fail_Upload,Incorrect_total_qc,L3_Incorrect_Qc_Pass,L3_Total), sum))

Region_State[is.na(Region_State)] <- 0

Region_State$Total <- 'Total'

Region_State_Total <- Region_State %>% group_by(Total) %>% summarise(across(c(Manifest,Neutralised,Cancel,QC_Failed,Picked,Not_picked,FPS_Outscan,FPS_Pickup,FPC_Push,PFC_FPOC,Incorrect_QC_Fail,Incorrect_QC_Fail_Upload,Incorrect_total_qc,L3_Incorrect_Qc_Pass,L3_Total), sum))


#Region_State_Total$Region <- 'Total'
Region_State_Total$State <- '-'

Region_State_Total <- Region_State_Total[,c(1,17,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
#Region_State <- Region_State[,c(1:17)]

colnames(Region_State)
colnames(Region_State_Total)


Region1<- bind_rows(Region_State,Region_State_Total)

Region1$FPC_Perc <- paste(round(100*Region1$PFC_FPOC/Region1$FPC_Push,0),"%", sep = "")

Region1$FPS_Perc <- paste(round(100*Region1$FPS_Pickup/Region1$FPS_Outscan,0),"%", sep = "")
Region1$Cancel_Perc <- paste(round(100*Region1$Cancel/Region1$Neutralised,0),"%", sep = "")
Region1$Picked_Perc <- paste(round(100*Region1$Picked/Region1$Neutralised,0),"%", sep = "")
Region1$QC_Failed_Perc <- paste(round(100*Region1$QC_Failed/Region1$Neutralised,0),"%", sep = "")
Region1$L3_Audit <- paste(round(100*Region1$L3_Incorrect_Qc_Pass/Region1$L3_Total,0),"%", sep = "")
Region1$Incorrect_Upload <- paste(round(100*Region1$Incorrect_QC_Fail_Upload/Region1$Incorrect_total_qc,0),"%", sep = "")
Region1$Incorrect_QC_Fail_perc <- paste(round(100*Region1$Incorrect_QC_Fail/Region1$Incorrect_total_qc,0),"%", sep = "")
#Final$Incorrect_Upload <- paste(round(100*Final$Incorrect_QC_Fail_Upload/Final$Incorrect_total_qc,0),"%", sep = "")

#colnames(Final)

Region2 <- Region1[,c(1,2,3,4,19,20,21,22,23,24,25,26)]


#colnames(Region2)[1] <- c('Total')
#colnames(Region2)[2] <- c('-')

#pivot[is.na(pivot)] <- 0
pivot$Total = 'Total'
#below total value in data frame

Pivot_Total <- pivot %>% group_by(Total) %>% summarise(across(c(Manifest,Neutralised,Cancel,QC_Failed,Picked,Not_picked,FPS_Outscan,FPS_Pickup,FPC_Push,PFC_FPOC,Incorrect_QC_Fail,Incorrect_QC_Fail_Upload,Incorrect_total_qc,L3_Incorrect_Qc_Pass,L3_Total), sum))
Pivot_Total$State <- 'Total'

Pivot_Total <-Pivot_Total[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

colnames(Pivot_Total)[1] <- c('Region')
#colnames(Pivot_Total)[2] <- c('State')

#columns having only 18 enties
pivot<- pivot[,1:18]




colnames(Pivot_Total)
colnames(pivot)
Final<- bind_rows(pivot,Pivot_Total)

Final$FPC_Perc <- paste(round(100*Final$PFC_FPOC/Final$FPC_Push,0),"%", sep = "")

Final$FPS_Perc <- paste(round(100*Final$FPS_Pickup/Final$FPS_Outscan,0),"%", sep = "")
Final$Cancel_Perc <- paste(round(100*Final$Cancel/Final$Neutralised,0),"%", sep = "")
Final$Picked_Perc <- paste(round(100*Final$Picked/Final$Neutralised,0),"%", sep = "")
Final$QC_Failed_Perc <- paste(round(100*Final$QC_Failed/Final$Neutralised,0),"%", sep = "")
Final$L3_Audit <- paste(round(100*Final$L3_Incorrect_Qc_Pass/Final$L3_Total,0),"%", sep = "")
Final$Incorrect_Upload <- paste(round(100*Final$Incorrect_QC_Fail_Upload/Final$Incorrect_total_qc,0),"%", sep = "")
Final$Incorrect_QC_Fail_perc <- paste(round(100*Final$Incorrect_QC_Fail/Final$Incorrect_total_qc,0),"%", sep = "")
#Final$Incorrect_Upload <- paste(round(100*Final$Incorrect_QC_Fail_Upload/Final$Incorrect_total_qc,0),"%", sep = "")

colnames(Final)

Final1 <- Final[,c(1,2,3,4,5,19,20,21,22,23,24,25,26)]


#data saving in excel
library(openxlsx)

Wb = createWorkbook()
addWorksheet(Wb, "summary")
addWorksheet(Wb, "DC_View")
addWorksheet(Wb, "Raw_Data")
#rm(Wb)
## Write data on Sheet 1 (Summary)-------------------

#write.csv(pivot,'pivot.csv',row.names = F)

writeData(Wb, "summary",Region2,startCol = 3, startRow = 4, colNames = T, rowNames = F)
writeData(Wb,"summary", "Summarize RVP", startCol = 3,startRow = 3,colNames = FALSE, rowNames = TRUE,borderStyle = "medium")
mergeCells(Wb,"summary",cols = 3:14, rows = 3)
writeData(Wb, "Raw_Data",D1, startCol = 1, startRow = 1, colNames = T, rowNames = F)
writeData(Wb, "DC_View",Final1, startCol = 1, startRow = 1, colNames = T, rowNames = F)


headerStyle <- createStyle(fontName = NULL, fontSize = 11, fontColour = '#ffffff', numFmt = "General",
                           border="TopBottomLeftRight",borderColour = "#111112",
                           borderStyle = getOption("openxlsx.borderStyle", "thin"),
                           fgFill = '#006699',   #Cell colour
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           wrapText = FALSE, textRotation = NULL)


subheaderStyle <- createStyle(fontName = NULL, fontSize = 11, fontColour = '#ffffff', numFmt = "TEXT",
                              border="TopBottomLeftRight",borderColour = "#111112",
                              borderStyle = getOption("openxlsx.borderStyle", "thin"),
                              fgFill = '#006699',   #Cell colour
                              halign = "center",
                              valign = "center",
                              textDecoration = "bold",
                              wrapText = FALSE, textRotation = NULL)

bottomStyle <- createStyle(fontName = NULL, fontSize = 11, fontColour = '#ffffff', numFmt = "General",
                           border="TopBottomLeftRight",borderColour = "#111112",
                           borderStyle = getOption("openxlsx.borderStyle", "thin"),
                           bgFill = getOption("openxlsx.borderColour", "white"),
                           fgFill = '#006699',   #Cell colour
                           halign = "center",
                           valign = "center",
                           textDecoration = "bold",
                           wrapText = FALSE, textRotation = NULL)

pct_GrandTotal = createStyle(fontName = NULL, fontSize = 11, fontColour = '#ffffff', numFmt = "0%",
                             border="TopBottomLeftRight",borderColour = "#111112",
                             borderStyle = getOption("openxlsx.borderStyle", "thin"),
                             bgFill = getOption("openxlsx.borderColour", "white"),
                             fgFill = '#006699',   #Cell colour
                             halign = "center",
                             valign = "center",
                             textDecoration = "bold",
                             wrapText = FALSE, textRotation = NULL)

BodyStyle = createStyle(fontName = NULL, fontSize = 11, fontColour = '#000000', numFmt = "TEXT",
                        border="TopBottomLeftRight",borderColour = "#111112",
                        borderStyle = getOption("openxlsx.borderStyle", "thin"),
                        bgFill = getOption("openxlsx.borderColour", "white"),
                        fgFill = '#ffffff',   #getOption("openxlsx.borderColour", "white"),   #Cell colour
                        halign = "center",
                        valign = "center",
                        textDecoration = NULL,
                        wrapText = FALSE, textRotation = NULL)

Body_percent_colm_Style = createStyle(fontName = NULL, fontSize = 11, fontColour = '#000000', numFmt = "0%",
                                      border="TopBottomLeftRight",borderColour = "#111112",
                                      borderStyle = getOption("openxlsx.borderStyle", "thin"),
                                      bgFill = getOption("openxlsx.borderColour", "white"),
                                      fgFill ='#ffffff',      #getOption("openxlsx.borderColour", "white"),   #Cell colour
                                      halign = "center",
                                      valign = "center",
                                      textDecoration = NULL,
                                      wrapText = FALSE, textRotation = NULL)



###########styling header########################
####################style in excel sheet where data save
Region2_1 = sum(c(3)+nrow(Region2))

addStyle(Wb, "summary", style=headerStyle               , cols= 3:14         , rows= 3                , gridExpand=TRUE)
addStyle(Wb, "summary", style=subheaderStyle            , cols= 3:14         , rows= 4                , gridExpand=TRUE)
addStyle(Wb, "summary", style=bottomStyle               , cols= 3:14        , rows= (Region2_1 +1)       , gridExpand=TRUE)
addStyle(Wb, "summary", style=BodyStyle                 , cols= 3:14         , rows= 5:(Region2_1)        , gridExpand=TRUE)
addStyle(Wb, "summary", style=Body_percent_colm_Style   , cols= c(14)        , rows= 5:(Region2_1)        , gridExpand=TRUE)
addStyle(Wb, "summary", style=pct_GrandTotal            , cols= c(14)        , rows= (Region2_1 +1)       , gridExpand=TRUE)



saveWorkbook(Wb,paste("drive where file save",report_date,".xlsx"), overwrite = TRUE)



library(RDCOMClient)
library(tableHTML)
library(cssTools)

#mail body printing through html and CSS
table1 <- Region2 %>%
  tableHTML(rownames = FALSE, second_headers = list((12),('Summary1'))) %>%
  add_css_row(css = list(c('background-color','font-size','font-weight','color','text-align'),c('#006699','15px','Bold','#ffffff','center')),rows = c(1,2,(nrow(Region2)+2))) %>%
  add_css_row(css = list(c('text-align','font-size'),c('center','15px')),rows = 3:(nrow(Region2)+1))



OutApp <- COMCreate("Outlook.Application")
OutMail = OutApp$CreateItem(0)

OutMail$GetInspector()

Signature <- OutMail[["HTMLbody"]] <- paste0("<B>",paste("<p style = color:'#000000'; font-size:'10px'>","Regards","<br>"),"<B>",
                                             (paste("<font size = '3'; color = '#000099'>","Mohit","</font>","<br>")),"<B>",
                                             
Note <- paste0( "Dear All,", paste("<p>","Hi , 

Greetings for the Day...

This is the script which i have worked and provie all to all automation without any.
                                   detail of ", report_date,sep = " ","from the following path:-"),
                paste("<p>","Path >> path for data shared","<P>"))

Note1 <- paste0("<P>","NOTE:- In case of any issue kindly connect with ","<U>","username","</U>")

OutMail[["To"]] = "mohit17@gmail.com"
OutMail[["CC"]] = "abha132@gmail.com"

OutMail[["subject"]] = paste("Automation through R",ymd(report_date),sep = "", "Report")


OutMail[["HTMLbody"]] = paste0(Note,"<P>","<U>","<B>","1. Summary ","</U>","</B>",
                               "<TABLE>","<TR>","<TD>", table1 ,"</TD>","<TD>","</TABLE>",
                               "</TR>","</TABLE>",Note1,Signature)



OutMail$Display()
OutMail$SEND()

#close sql connection
lapply(dbListConnections(MySQL()), dbDisconnect)
#free ram 
rm(list = ls())
gc()


