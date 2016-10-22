#suppressPackageStartupMessages(library(googleVis))
library(googleVis)
library(shiny)
library(datasets)
library(devtools)
#library(xts)
library(reshape2)
library(reshape)
library(plyr)
library(DT)
library(plotrix)
#library(grid)
library(data.table)
#library(car)
#library(MASS)
library(rpivotTable)
library(ggplot2)
#library(plotly)
library(formattable)
library(highcharter)
#library(TileMaker)

shinyServer(function(input, output, session) {
  
  
  Raw<- reactive({
   inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #Raw<-read.csv(inFile$datapath, header=TRUE ,sep=",") 
    #Raw<-read.csv(inFile$datapath,header=TRUE, stringsAsFactors=FALSE, fileEncoding="big5")
    Raw<-read.csv(inFile$datapath,header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="big5")

    Raw<- Raw[-c(1, 2, 3), ]
    Raw$Training<-NULL
    Raw$Project<-NULL
    Raw$Study<-NULL
    Raw$Teach<-NULL
    Raw$ADM<-NULL
    Raw$TC<-NULL
    Raw$Other<-NULL
    Raw$`部門`<-NULL
    Raw$`部門簡稱2`<-NULL
    
    names(Raw)[1]<-"NO"
    names(Raw)[2]<-"Function_team"
    names(Raw)[3]<-"Employee_ID"
    names(Raw)[4]<-"Name"
    names(Raw)[5]<-"Dept_code"
    names(Raw)[6]<-"Department"
    names(Raw)[7]<-"absentHours"
    names(Raw)[8]<-"TotalHours"
    names(Raw)[9]<-"Input_Hours"
    
    Raw$Employee_ID<-NULL
    Raw$Name<-NULL
    
    Raw$NO<-NULL
    Raw$Dept_code<-NULL
    Raw$absentHours<-NULL
    Raw$TotalHours<-NULL
    Raw$Input_Hours<-NULL
    Raw$Input<-NULL
    Raw$Total.1<-NULL
    Raw<-head(Raw,-1)
    colnames(Raw)[grepl("OTHERS", colnames(Raw))]<-"Others"
    colnames(Raw)[grepl("Absent", colnames(Raw))]<-"absent_hours"
    Raw
  })
  
  Department_table<- reactive({

    if(is.null(input$month))
      return()
    Raw<-Raw()
    # Keep standard projects only
    
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:2],Raw2)
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    #Raw3$`Standard Project - JV`<-NULL
    # Factor to numeric
    for (i in 3:ncol(Raw3))
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    
    #----------------new------------------------ 
    names(Raw3)[2]<-"Department"   #2
    #Raw3$`##`<-NULL
    #Raw3$Code<-NULL
    #Raw3$`部門簡稱2`<-NULL
    Raw3$`Standard Project - HP`<-NULL
    Raw3$`Standard Project - JV`<-NULL
    Raw3$`Standard Project - Ali`<-NULL
    Raw3$`Standard Project - Others`<-NULL
    
    #----------------new------------------------ 
    
    dept_agg<-aggregate(Raw3[,3:length(Raw3)], 
                        by=list(
                          Raw3$Department,
                          Raw3$Function
                        ), FUN=sum)
    #dept_agg<-dept_agg[-c(1), ]
    names(dept_agg)[1]<-"department"
    names(dept_agg)[2]<-"function_team"
    dept_Melt<-melt(dept_agg, id=c("department","function_team"))
    colnames(dept_Melt)[grepl("value", colnames(dept_Melt))]<-"man_hour"
    colnames(dept_Melt)[grepl("variable", colnames(dept_Melt))]<-"project_name"
    
    dept_Melt$time<-input$month
    dept_Melt<-subset(dept_Melt, select=c("time","department","function_team","project_name","man_hour"))
    dept_Melt
  })
  
  output$melt_upload<-renderText({
    if(is.null(input$month))
      return(NULL) 
    
    validate(
      need(input$month !="", "The input format: 201204, 201212")
    )
    
    #Get the month from DB
    rm(melt_db)
    melt_db <- dbGetQuery(con, "SELECT * from dms_melt")
    month_db<-unique(melt_db$time)

    
    #Get the month from user
    month_usr<-input$month
    
    # Compare month_db & month_usr
    sum=0
    for (i in 1:length(month_db))
    {
      if (grepl(month_db[i], month_usr)=="TRUE")
      {
        sum=sum+1
      }
    }
      Melt<-Melt()
	    Department_table<-Department_table()
      rownames(Department_table) <- NULL
      colnames(Melt)[grepl("Function_team", colnames(Melt))]<-"function_team"
      colnames(Melt)[grepl("Department", colnames(Melt))]<-"department"
      colnames(Melt)[grepl("Project_type", colnames(Melt))]<-"project_type"
      colnames(Melt)[grepl("Man_hour", colnames(Melt))]<-"man_hour"
      
      Melt$time<-input$month
      Melt$id<-1:nrow(Melt)
      Melt<-subset(Melt, select=c("time", "department","function_team",
                                  "project_type","man_hour","id"))
      rownames(Melt) <- NULL
      Melt$man_hour[is.na(Melt$man_hour)] <- 0
      Melt$man_hour[Melt$man_hour==" "]<-0
      
    if (sum==0) # upload
    {
      #dbWriteTable(con, "dms_melt", value = Melt, overwrite = F, append=T)
	    #dbWriteTable(con, "manhour_by_project", value = Department_table, append = T, row.names = FALSE)
 	    if (all(dbWriteTable(con, "dms_melt", value = Melt, overwrite = F, append=T), 
 	          dbWriteTable(con, "manhour_by_project", value = Department_table, append = T, row.names = FALSE))=="TRUE")
 		   {
 		     print("Uploaded successfully!")
 		   }
    }else{
      if (all(	
	  dbGetQuery(con, "DELETE FROM dms_melt WHERE time = $1 ", list(month_usr)),
	  dbGetQuery(con, "DELETE FROM manhour_by_project WHERE time = $1 ", list(month_usr)),
	  dbWriteTable(con, "dms_melt", value = Melt, overwrite = F, append=T),
	  dbWriteTable(con, "manhour_by_project", value = Department_table, append = T, row.names = FALSE))=="TRUE")
	    {
	      print("Updated successfully!")
	    }
	}
  })
  
  #output$overall_util_table <- renderDataTable({ 
  output$overall_util_table <- renderFormattable({  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(is.null(input$month))
      return(NULL) 
 
    Raw<-Raw()
    Raw1<-Raw[-c(1), ]
    Raw1$absentHours<-NULL
    Raw1$TotalHours<-NULL
    Raw1$NO<-NULL
    
    # Remove the column sum
    Raw1$RFQ.Project...HP<-NULL
    Raw1$RFQ.Project...JV<-NULL
    Raw1$RFQ.Project...Ali<-NULL
    Raw1$RFQ.Project...Others<-NULL
    
    Raw1$Standard.Project...HP<-NULL
    Raw1$Standard.Project...JV<-NULL
    Raw1$Standard.Project...Ali<-NULL
    Raw1$Standard.Project...Others<-NULL
    
    Raw1$Sustaining.Project...HP<-NULL
    Raw1$Sustaining.Project...JV<-NULL
    Raw1$Sustaining.Project...Ali<-NULL
    Raw1$Sustaining.Project...Others<-NULL
    
    Raw1$Function.Project<-NULL
    #Raw1
    
    rm(Raw)
    # Factor to numeric
    for (i in 3:length(Raw1))
    {
      #Raw1[,i]<-as.numeric(levels(Raw1[,i]))[Raw1[,i]]
      Raw1[,i]<-as.numeric(Raw1[,i])
      #Raw1[,i]<-as.numeric(as.character(Raw1[,i]))
    }
    
    #Raw1$absent_hours<-Raw1$Input_Hours-Raw1$Input
    # Aggregate the sum by dept and function team
    agg<-aggregate(Raw1[,3:length(Raw1)], 
                   by=list(#Raw1$NO,
                     Raw1$Department,
                     Raw1$Function_team
                     #Raw1$Dept_code,
                   ), FUN=sum)
    # Remove Raw
    rm(Raw1)
    agg
     
    #agg<-agg[-c(1), ] #Delete the last total sum row
    colnames(agg)[grepl("Group.1", colnames(agg))]<-"Department"
    colnames(agg)[grepl("Group.2", colnames(agg))]<-"Function_team"
     
    # Melt and reshape
  
    Melt<-melt(agg, id=c("Department","Function_team"))
    colnames(Melt)[grepl("value", colnames(Melt))]<-"Man_Hours"
    colnames(Melt)[grepl("variable", colnames(Melt))]<-"Project_type"
    Melt$Man_Hours[is.na(Melt$Man_Hours)]<-0
    # Remove agg
    rm(agg)
    
    # Remove the digit of the project type
    Melt$Project_type<-gsub("\\d+","",Melt$Project_type) 
    
    # Aggregate the sum base on the project type
    Melt<-aggregate(Melt$Man_Hours, 
                    by=list(#Raw1$NO,
                      Melt$Department,
                      Melt$Function_team,
                      Melt$Project_type
                    ), FUN=sum)
    colnames(Melt)[grepl("Group.1", colnames(Melt))]<-"Department"
    colnames(Melt)[grepl("Group.2", colnames(Melt))]<-"Function_team"
    colnames(Melt)[grepl("Group.3", colnames(Melt))]<-"Project_type"
    colnames(Melt)[grepl("x", colnames(Melt))]<-"Man_hour"
    
    # Remove the dot
    Melt$Project_type <- gsub("\\.", "", Melt$Project_type)
 
     # Prepare for the pie chart

    Melt$Department<-NULL
    Melt$Function_team<-NULL
     
     # Subset the project type
#     #Melt<-Melt[(Melt$Project_type != "Input_Hours")&(Melt$Project_type !="Input")
#     #          &(Melt$Project_type !="OTHERS"), ]
#     
    # Percentage compute
    Melt_pie<-aggregate(. ~Project_type, Melt, sum)
   
#     # Remove Melt
#     rm(Melt)
#     
    # Ranking the manhours
    #Melt_pie[,2]<-rev(sort(Melt_pie[,2]))
    
    Melt_pie$Percent<-round(100*Melt_pie$Man_hour/sum(Melt_pie$Man_hour),1)   
    #Melt_pie$Percent<-paste(Melt_pie$Percent, "%",sep="")
    Melt_pie$Percent<-as.numeric(Melt_pie$Percent) 
    Melt_pie<-Melt_pie[ order(-Melt_pie[,2]), ]
    rownames(Melt_pie)<-1:nrow(Melt_pie)
    #Melt_pie
    Melt_pie$NO<-1:nrow(Melt_pie) 
    
    Melt_pie<-subset(Melt_pie, select=c("NO", "Project_type","Man_hour","Percent")) 
    
    formattable(Melt_pie, list( 
      Project_type = formatter("span", style = x ~ ifelse(x == "Others"|x == "absent_hours", 
                                                          style(color = "red", font.weight = "bold"), 
                                                          style(color = "blue", font.weight = "bold"))), 
      Man_hour = color_bar("pink", 0.2), 
      Percent = color_bar("lavender", 0.2)
    ))
  }) 
  
  output$overall_util_pie <-renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(is.null(input$month))
      return(NULL) 
    
    Raw<-Raw()
    Raw1<-Raw[-c(1), ]
    Raw1$absentHours<-NULL
    Raw1$TotalHours<-NULL
    Raw1$NO<-NULL
    
    # Remove the column sum
    Raw1$RFQ.Project...HP<-NULL
    Raw1$RFQ.Project...JV<-NULL
    Raw1$RFQ.Project...Ali<-NULL
    Raw1$RFQ.Project...Others<-NULL
    
    Raw1$Standard.Project...HP<-NULL
    Raw1$Standard.Project...JV<-NULL
    Raw1$Standard.Project...Ali<-NULL
    Raw1$Standard.Project...Others<-NULL
    
    Raw1$Sustaining.Project...HP<-NULL
    Raw1$Sustaining.Project...JV<-NULL
    Raw1$Sustaining.Project...Ali<-NULL
    Raw1$Sustaining.Project...Others<-NULL
    
    Raw1$Function.Project<-NULL
    
    
    rm(Raw)
    # Factor to numeric
    for (i in 3:length(Raw1))
    {
      #Raw1[,i]<-as.numeric(levels(Raw1[,i]))[Raw1[,i]]
      Raw1[,i]<-as.numeric(Raw1[,i])
      #Raw1[,i]<-as.numeric(as.character(Raw1[,i]))
    }
    
    #Raw1$absent_hours<-Raw1$Input_Hours-Raw1$Input
    # Aggregate the sum by dept and function team
    agg<-aggregate(Raw1[,3:length(Raw1)], 
                   by=list(#Raw1$NO,
                     Raw1$Department,
                     Raw1$Function_team
                     #Raw1$Dept_code,
                   ), FUN=sum)
    # Remove Raw
    rm(Raw1)
    
    #agg<-agg[-c(1), ] #Delete the last total sum row
    colnames(agg)[grepl("Group.1", colnames(agg))]<-"Department"
    colnames(agg)[grepl("Group.2", colnames(agg))]<-"Function_team"
    #colnames(agg)[grepl("Group.3", colnames(agg))]<-"Cap."
    #agg[, 1:10]
    
    # Melt and reshape
    library(reshape)
    Melt<-melt(agg, id=c("Department","Function_team"))
    colnames(Melt)[grepl("value", colnames(Melt))]<-"Man_Hours"
    colnames(Melt)[grepl("variable", colnames(Melt))]<-"Project_type"
    Melt$Man_Hours[is.na(Melt$Man_Hours)]<-0
    # Remove agg
    rm(agg)
    
    # Remove the digit of the project type
    Melt$Project_type<-gsub("\\d+","",Melt$Project_type) 
    
    # Aggregate the sum base on the project type
    Melt<-aggregate(Melt$Man_Hours, 
                    by=list(#Raw1$NO,
                      Melt$Department,
                      Melt$Function_team,
                      Melt$Project_type
                    ), FUN=sum)
    colnames(Melt)[grepl("Group.1", colnames(Melt))]<-"Department"
    colnames(Melt)[grepl("Group.2", colnames(Melt))]<-"Function_team"
    colnames(Melt)[grepl("Group.3", colnames(Melt))]<-"Project_type"
    colnames(Melt)[grepl("x", colnames(Melt))]<-"Man_hour"
    
    # Remove the dot
    Melt$Project_type <- gsub("\\.", "", Melt$Project_type)
    
    
    # Prepare for the pie chart
    #Melt<-Melt()
    Melt$Department<-NULL
    Melt$Function_team<-NULL
    
    
    # Subset the project type
    #Melt<-Melt[(Melt$Project_type != "Input_Hours")&(Melt$Project_type !="Input")
    #          &(Melt$Project_type !="OTHERS"), ]
    
    # Percentage compute
    Melt_pie<-aggregate(. ~Project_type, Melt, sum)
    #Melt_pie<-head(Melt_pie,-1)
    
    
    # Remove Melt
    rm(Melt)
    
    # Ranking the manhours
    #Melt_pie[,2]<-rev(sort(Melt_pie[,2]))
    
    Melt_pie$Percent<-round(100*Melt_pie$Man_hour/sum(Melt_pie$Man_hour),1)             
    Melt_pie$lbls<-paste(Melt_pie$Project_type, Melt_pie$Percent)
    Melt_pie$lbls<-paste(Melt_pie$lbls, "%",sep="")
    
    # The 3D pie chart
    #pie3D(Melt_pie$Percent, labels = Melt_pie$Project_type, explode = 0.1)
    pie3D(Melt_pie$Percent, labels = Melt_pie$lbls, radius=1.8, height=0.3, explode=0.15)
  })   
  
  #output$overall_util_pivot= renderDataTable({
  output$overall_util_pivot= renderRpivotTable({
  
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    if(is.null(input$month))
      return(NULL) 
    
    Raw<-Raw()
    Raw1<-Raw[-c(1), ]
    Raw1$absentHours<-NULL
    Raw1$TotalHours<-NULL
    Raw1$NO<-NULL
    
    # Remove the column sum
    Raw1$RFQ.Project...HP<-NULL
    Raw1$RFQ.Project...JV<-NULL
    Raw1$RFQ.Project...Ali<-NULL
    Raw1$RFQ.Project...Others<-NULL
    
    Raw1$Standard.Project...HP<-NULL
    Raw1$Standard.Project...JV<-NULL
    Raw1$Standard.Project...Ali<-NULL
    Raw1$Standard.Project...Others<-NULL
    
    Raw1$Sustaining.Project...HP<-NULL
    Raw1$Sustaining.Project...JV<-NULL
    Raw1$Sustaining.Project...Ali<-NULL
    Raw1$Sustaining.Project...Others<-NULL
    
    Raw1$Function.Project<-NULL
    
    
    rm(Raw)
    # Factor to numeric
    for (i in 3:length(Raw1))
    {
      #Raw1[,i]<-as.numeric(levels(Raw1[,i]))[Raw1[,i]]
      Raw1[,i]<-as.numeric(Raw1[,i])
      #Raw1[,i]<-as.numeric(as.character(Raw1[,i]))
    }
    
    #Raw1$absent_hours<-Raw1$Input_Hours-Raw1$Input
    # Aggregate the sum by dept and function team
    agg<-aggregate(Raw1[,3:length(Raw1)], 
                   by=list(#Raw1$NO,
                     Raw1$Department,
                     Raw1$Function_team
                     #Raw1$Dept_code,
                   ), FUN=sum)
    # Remove Raw
    rm(Raw1)
    
    #agg<-agg[-c(1), ] #Delete the last total sum row
    colnames(agg)[grepl("Group.1", colnames(agg))]<-"Department"
    colnames(agg)[grepl("Group.2", colnames(agg))]<-"Function_team"
    #colnames(agg)[grepl("Group.3", colnames(agg))]<-"Cap."
    #agg[, 1:10]
    
    # Melt and reshape
    library(reshape)
    Melt<-melt(agg, id=c("Department","Function_team"))
    colnames(Melt)[grepl("value", colnames(Melt))]<-"Man_Hours"
    colnames(Melt)[grepl("variable", colnames(Melt))]<-"Project_type"
    Melt$Man_Hours[is.na(Melt$Man_Hours)]<-0
    # Remove agg
    rm(agg)
    
    # Remove the digit of the project type
    Melt$Project_type<-gsub("\\d+","",Melt$Project_type) 
    
    # Aggregate the sum base on the project type
    Melt<-aggregate(Melt$Man_Hours, 
                    by=list(#Raw1$NO,
                      Melt$Department,
                      Melt$Function_team,
                      Melt$Project_type
                    ), FUN=sum)
    colnames(Melt)[grepl("Group.1", colnames(Melt))]<-"Department"
    colnames(Melt)[grepl("Group.2", colnames(Melt))]<-"Function_team"
    colnames(Melt)[grepl("Group.3", colnames(Melt))]<-"Project_type"
    colnames(Melt)[grepl("x", colnames(Melt))]<-"Man_hour"
    
    # Remove the dot
    Melt$Project_type <- gsub("\\.", "", Melt$Project_type)
    Melt
    rpivotTable(Melt)
  })
  
  Melt<-reactive({
    Raw<-Raw()
    Raw1<-Raw[-c(1), ]
    Raw1$absentHours<-NULL
    Raw1$TotalHours<-NULL
    Raw1$NO<-NULL
    
    # Remove the column sum
    Raw1$RFQ.Project...HP<-NULL
    Raw1$RFQ.Project...JV<-NULL
    Raw1$RFQ.Project...Ali<-NULL
    Raw1$RFQ.Project...Others<-NULL
    
    Raw1$Standard.Project...HP<-NULL
    Raw1$Standard.Project...JV<-NULL
    Raw1$Standard.Project...Ali<-NULL
    Raw1$Standard.Project...Others<-NULL
    
    Raw1$Sustaining.Project...HP<-NULL
    Raw1$Sustaining.Project...JV<-NULL
    Raw1$Sustaining.Project...Ali<-NULL
    Raw1$Sustaining.Project...Others<-NULL
    
    Raw1$Function.Project<-NULL
    
    
    rm(Raw)
    # Factor to numeric
    for (i in 3:length(Raw1))
    {
      #Raw1[,i]<-as.numeric(levels(Raw1[,i]))[Raw1[,i]]
      Raw1[,i]<-as.numeric(Raw1[,i])
      #Raw1[,i]<-as.numeric(as.character(Raw1[,i]))
    }
    
    #Raw1$absent_hours<-Raw1$Input_Hours-Raw1$Input
    # Aggregate the sum by dept and function team
    agg<-aggregate(Raw1[,3:length(Raw1)], 
                   by=list(#Raw1$NO,
                     Raw1$Department,
                     Raw1$Function_team
                     #Raw1$Dept_code,
                   ), FUN=sum)
    # Remove Raw
    rm(Raw1)
    
    #agg<-agg[-c(1), ] #Delete the last total sum row
    colnames(agg)[grepl("Group.1", colnames(agg))]<-"Department"
    colnames(agg)[grepl("Group.2", colnames(agg))]<-"Function_team"
    #colnames(agg)[grepl("Group.3", colnames(agg))]<-"Cap."
    #agg[, 1:10]
    
    # Melt and reshape
    library(reshape)
    Melt<-melt(agg, id=c("Department","Function_team"))
    colnames(Melt)[grepl("value", colnames(Melt))]<-"Man_Hours"
    colnames(Melt)[grepl("variable", colnames(Melt))]<-"Project_type"
    Melt$Man_Hours[is.na(Melt$Man_Hours)]<-0
    # Remove agg
    rm(agg)
    
    # Remove the digit of the project type
    Melt$Project_type<-gsub("\\d+","",Melt$Project_type) 
    
    # Aggregate the sum base on the project type
    Melt<-aggregate(Melt$Man_Hours, 
                    by=list(#Raw1$NO,
                      Melt$Department,
                      Melt$Function_team,
                      Melt$Project_type
                    ), FUN=sum)
    colnames(Melt)[grepl("Group.1", colnames(Melt))]<-"Department"
    colnames(Melt)[grepl("Group.2", colnames(Melt))]<-"Function_team"
    colnames(Melt)[grepl("Group.3", colnames(Melt))]<-"Project_type"
    colnames(Melt)[grepl("x", colnames(Melt))]<-"Man_hour"
    
    # Remove the dot
    Melt$Project_type <- gsub("\\.", "", Melt$Project_type)
    Melt
  })
  
  
  
  
#------------------Resources Mapping Analysis (For RD) (Use Raw2)----------------------
  
  output$resource_mapping_table = renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)

    Raw<-Raw()
    # Keep standard projects only
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:5],Raw2)
    
    #remove Raw, Raw
    rm(Raw)
    rm(Raw2)
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    Raw3$`Standard Project - JV`<-NULL
    
    # Factor to numeric
    for (i in 6:ncol(Raw3))
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    # Sum project hours and select Top 10
    rank_all<-rev(sort(colSums(Raw3[,6:length(Raw3)])))
    pro_total<-colSums(Raw3[,6:length(Raw3)])
    
    # Remove Raw3
    rm(Raw3)
    
    pro_total<-as.data.frame(pro_total)
    pro_total$Project<-rownames(pro_total)
    rownames(pro_total)<-1:nrow(pro_total)
    names(pro_total)[1]<-"Man_Hour"
    pro_total<-subset(pro_total, select=c("Project", "Man_Hour"))
    pro_total$Percent<-round(100*(pro_total$Man_Hour)/sum(pro_total$Man_Hour),2)
    pro_total$Percent<-paste(pro_total$Percent, "%",sep="")
    pro_total<-pro_total[ order(-pro_total[,2]), ]
    rownames(pro_total)<-1:nrow(pro_total)
    pro_total
  }) 
  
  output$resource_mapping_pie <-renderPlot({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
#     Raw<-read.csv(inFile$datapath, header=TRUE ,sep=",") 
#     Raw<- Raw[-c(1, 2, 3), ]
#     
#     #Delete unused column
#     Raw$RFQ.Project<-NULL
#     Raw$Standard.Project<-NULL
#     Raw$Sustaining.Project<-NULL
#     Raw$Function.Project<-NULL
#     Raw$Others<-NULL
#     Raw$Absent<-NULL
#     Raw$Total.1<-NULL
#     Raw$Other.1<-NULL
#     Raw$TC.1<-NULL
#     Raw$ADM.1<-NULL
#     Raw$Teach.1<-NULL
#     Raw$Study.1<-NULL
#     Raw$Project.2<-NULL
#     Raw$Training.1<-NULL
#     Raw$Project.1<-NULL
#     
#     names(Raw)[1]<-"NO"
#     names(Raw)[2]<-"Department"
#     names(Raw)[3]<-"Function_team"
#     names(Raw)[4]<-"Employee_ID"
#     names(Raw)[5]<-"Name"
#     names(Raw)[6]<-"Dept_code"
#     names(Raw)[7]<-"Cap"
#     names(Raw)[8]<-"absent_hours"
#     names(Raw)[9]<-"Total_Hours"
#     names(Raw)[10]<-"Input_Hours"
#     Raw$Employee_ID<-NULL
#     Raw$Name<-NULL
#     Raw$Absent<-NULL
#     
#     colnames(Raw)[grepl("Study", colnames(Raw))]<-"Others3"
#     colnames(Raw)[grepl("Teach", colnames(Raw))]<-"Others4"
#     colnames(Raw)[grepl("ADM", colnames(Raw))]<-"Others5"
#     colnames(Raw)[grepl("TC", colnames(Raw))]<-"Others6"
#     colnames(Raw)[grepl("Other", colnames(Raw))]<-"Others7"
#     colnames(Raw)[which(names(Raw) == "Project")] <- "Others"
#     colnames(Raw)[which(names(Raw) == "Training")] <- "Others"
    
    Raw<-Raw()
    # Keep standard projects only
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:5],Raw2)
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Remove Raw, Raw2
    rm(Raw)
    rm(Raw2)
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    Raw3$`Standard Project - JV`<-NULL
    
    # Factor to numeric
    for (i in 6:ncol(Raw3))
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    # Sum project hours and ranking
    rank_all<-rev(sort(colSums(Raw3[,6:length(Raw3)])))
    pro_total<-colSums(Raw3[,6:length(Raw3)])
    
    # Remove Raw3
    rm(Raw3)
    
    pro_total<-as.data.frame(pro_total)
    pro_total$Project<-rownames(pro_total)
    rownames(pro_total)<-1:nrow(pro_total)
    names(pro_total)[1]<-"Man_Hour"
    pro_total<-subset(pro_total, select=c("Project", "Man_Hour"))
    pro_total$Percent<-round(100*(pro_total$Man_Hour)/sum(pro_total$Man_Hour),2)
    pro_total$Percent<-paste(pro_total$Percent, "%",sep="")
    pro_total$Percent<-as.numeric(pro_total$Percent)
    pro_total<-pro_total[ order(-pro_total[,2]), ]
    rownames(pro_total)<-1:nrow(pro_total)
    pro_total<-pro_total[1:10,]
    pie3D(pro_total[1:10,2], labels = pro_total[1:10,1], radius=2.1, height=0.3, explode=0.15)
#     bp <- ggplot(pro_total, aes(x="", y=Man_Hour, fill=Project))+geom_bar(width = 1, stat = "identity")
#     pie <- bp+coord_polar("y", start=0)
#     pie
  })
  
  output$Tilemaker2<-renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    Raw<-Raw()
    # Keep standard projects only
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:5],Raw2)
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Remove Raw, Raw2
    rm(Raw)
    rm(Raw2)
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    #Raw3$`Standard Project - JV`<-NULL
    # Factor to numeric
    for (i in 6:ncol(Raw3))
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    # Sum project hours and rank 
    pro_total<-colSums(Raw3[,6:length(Raw3)])
    pro_total<-as.data.frame(pro_total)
    pro_total$Project<-rownames(pro_total)
    rownames(pro_total)<-1:nrow(pro_total)
    names(pro_total)[1]<-"Man_Hour"
    pro_total<-subset(pro_total, select=c("Project", "Man_Hour"))
    
    # Speicfy JV and runrate
    for (i in 1:nrow(pro_total))
    {
      if (grepl( "Standard Project - JV", pro_total$Project[i])==TRUE){
        break
      }
    }
    index<-i
    
    # Classify JV and runrate
    for (i in 1:(index-1))
    {
      pro_total$biz_model[i]<-"HP_Runrate"
    }
    
    for (j in (index):nrow(pro_total))
    {
      pro_total$biz_model[j]<-"HP_JV"
    }
    
    # Remove the JV total row
    pro_total<-pro_total[!(pro_total$Project=="Standard Project - JV"),]
    pro_total$Percent<-round(100*(pro_total$Man_Hour)/sum(pro_total$Man_Hour),2)
    #pro_total$Percent<-paste(pro_total$Percent, "%",sep="")
    #pro_total$Percent<-as.numeric(pro_total$Percent)
    pro_total<-pro_total[ order(-pro_total[,2]), ]
    rownames(pro_total)<-1:nrow(pro_total)
    pro_total<-subset(pro_total, select=c("Project", "Man_Hour", "Percent", "biz_model"))
    
    biz_percent<-aggregate(pro_total$Percent, by=list(pro_total$biz_model), FUN=sum)
    names(biz_percent)[1]<-"Biz_Model"
    names(biz_percent)[2]<-"Percent"
    biz_percent
    
    
    Button1 <- ButtonMaker(Color = 5,Value = nrow(pro_total),Size = 3,
                           Subtitle = "Number of Standard Project", Icon="glyphicon glyphicon-piggy-bank")
    Button2 <- ButtonMaker(Color = 3,Value = pro_total[1,1], Size = 3,
                           Subtitle = "With Most Manhours", Icon="glyphicon glyphicon-alert")
    Button3 <- ButtonMaker(Color = 1,Value = biz_percent[1,2],Size = 3,
                           Units="%",Icon="glyphicon glyphicon-signal",Subtitle = "HP JV %")
    Button4 <- ButtonMaker(Color = 2,Value = biz_percent[2,2],Size = 3,
                           Units="%",Icon="glyphicon glyphicon-stats",Subtitle = "HP runrate %")
    Div1 <- DivMaker(Buttons = paste(Button1,Button3,Button4,Button2))
    #Div2 <- DivMaker(Buttons = paste(Button2))
    #Div2 <- DivMaker(Title = "Implementation procedures",Buttons = paste(Button3,Button4))
    return(list(HTML(Div1)))
  })
  
  
  
  #output$resource_mapping_orderbar <-renderPlot({
  #output$resource_mapping_orderbar <-renderPlotly({
  output$resource_mapping_orderbar <-renderHighchart({
  #output$order_bar <-renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
#     Raw<-read.csv(inFile$datapath, header=TRUE ,sep=",") 
#     Raw<- Raw[-c(1, 2, 3), ]
#     
#     #Delete unused column
#     Raw$RFQ.Project<-NULL
#     Raw$Standard.Project<-NULL
#     Raw$Sustaining.Project<-NULL
#     Raw$Function.Project<-NULL
#     Raw$Others<-NULL
#     Raw$Absent<-NULL
#     Raw$Total.1<-NULL
#     Raw$Other.1<-NULL
#     Raw$TC.1<-NULL
#     Raw$ADM.1<-NULL
#     Raw$Teach.1<-NULL
#     Raw$Study.1<-NULL
#     Raw$Project.2<-NULL
#     Raw$Training.1<-NULL
#     Raw$Project.1<-NULL
#     
#     names(Raw)[1]<-"NO"
#     names(Raw)[2]<-"Department"
#     names(Raw)[3]<-"Function_team"
#     names(Raw)[4]<-"Employee_ID"
#     names(Raw)[5]<-"Name"
#     names(Raw)[6]<-"Dept_code"
#     names(Raw)[7]<-"Cap"
#     names(Raw)[8]<-"absent_hours"
#     names(Raw)[9]<-"Total_Hours"
#     names(Raw)[10]<-"Input_Hours"
#     Raw$Employee_ID<-NULL
#     Raw$Name<-NULL
#     Raw$Absent<-NULL
#     
#     colnames(Raw)[grepl("Study", colnames(Raw))]<-"Others3"
#     colnames(Raw)[grepl("Teach", colnames(Raw))]<-"Others4"
#     colnames(Raw)[grepl("ADM", colnames(Raw))]<-"Others5"
#     colnames(Raw)[grepl("TC", colnames(Raw))]<-"Others6"
#     colnames(Raw)[grepl("Other", colnames(Raw))]<-"Others7"
#     colnames(Raw)[which(names(Raw) == "Project")] <- "Others"
#     colnames(Raw)[which(names(Raw) == "Training")] <- "Others"
    
     Raw<-Raw()
    # Keep standard projects only
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:2],Raw2)   # 1:2
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    #Raw3$`Standard Project - JV`<-NULL
    # Factor to numeric
    for (i in 3:ncol(Raw3))   #3
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    biz_percent<-Raw3[ , grepl( "Standard Project", names(Raw3))]
    names(biz_percent)<-gsub("^.*?-","-",names(biz_percent))
    names(biz_percent)<-gsub("-", "", names(biz_percent))
    
    
    # Sum biz_percent
    pro_total<-Raw3[ , grep( "Project", names(Raw3), invert=TRUE) ]
    pro_total<-colSums(pro_total[,3:length(pro_total)])  #3
    pro_total<-as.data.frame(pro_total)
    pro_total$Project<-rownames(pro_total)
    rownames(pro_total)<-1:nrow(pro_total)
    names(pro_total)[1]<-"Man_Hour"
    pro_total<-subset(pro_total, select=c("Project", "Man_Hour"))
    
    # Sum pro_total
    biz_percent<-colSums(biz_percent)
    biz_percent<-as.data.frame(biz_percent)
    biz_percent$Project<-rownames(biz_percent)
    rownames(biz_percent)<-1:nrow(biz_percent)
    names(biz_percent)[1]<-"Man_Hour"
    biz_percent<-subset(biz_percent, select=c("Project", "Man_Hour"))
    
    # Compute the percentages
    pro_total$Percent<-round(100*(pro_total$Man_Hour)/sum(pro_total$Man_Hour),2)
    pro_total<-pro_total[ order(-pro_total[,2]), ]
    
    biz_percent$Percent<-round(100*(biz_percent$Man_Hour)/sum(biz_percent$Man_Hour),2)
    
    #---------Build the high chart------------------------------------
    highchart() %>% 
      hc_title(text = "The chart describes both the JV and run rate percentage
               and break down into projects.") %>%
      hc_subtitle(text = "In percentage of business models and projects.") %>% 
      hc_add_series_labels_values(pro_total[1:20,]$Project, pro_total[1:20,]$Percent, name = "Project",
                                  colorByPoint = TRUE, type = "column") %>% 
       hc_add_series_labels_values(biz_percent$Project, biz_percent$Percent,
                                   #colors = substr(terrain.colors(5), 0 , 7), 
                                   type = "pie",
                                   name = "Pie_chart", colorByPoint = TRUE, center = c('50%', '40%'),
                                   size = 330, dataLabels = list(enabled = TRUE)) %>% 
      hc_yAxis(title = list(text = "Man Hour Percentage"),
               labels = list(format = "{value}%"), max = (pro_total[1,]$Percent)*1.1) %>% 
      hc_xAxis(categories = pro_total$Project) %>% 
      hc_legend(enabled = TRUE) %>% 
      hc_tooltip(pointFormat = "{point.y}%")
    
     
      


    #-------With order-----------------------------------------------------------------
#     p <- ggplot(pro_total, aes(x=reorder(Project,Man_Hour), y=Man_Hour, fill=factor(biz_model))) +geom_bar(stat="identity")
#     p <- p + theme(text = element_text(size=15), axis.text.x = element_text(angle=90, vjust=1))  
#     p <- p+coord_flip()
#     p <- p+labs(x="Project name", y="Man hours")
#     p <- p+labs(fill= "Business Model")
#     p
  })
  
  output$choose_Department <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
#     Raw<-read.csv(inFile$datapath, header=TRUE ,sep=",") 
#     Raw<- Raw[-c(1, 2, 3), ]
#     
#     #Delete unused column
#     Raw$RFQ.Project<-NULL
#     Raw$Standard.Project<-NULL
#     Raw$Sustaining.Project<-NULL
#     Raw$Function.Project<-NULL
#     Raw$Others<-NULL
#     Raw$Absent<-NULL
#     Raw$Total.1<-NULL
#     Raw$Other.1<-NULL
#     Raw$TC.1<-NULL
#     Raw$ADM.1<-NULL
#     Raw$Teach.1<-NULL
#     Raw$Study.1<-NULL
#     Raw$Project.2<-NULL
#     Raw$Training.1<-NULL
#     Raw$Project.1<-NULL
#     
#     names(Raw)[1]<-"NO"
#     names(Raw)[2]<-"Department"
    Raw<- Raw()
    Raw<- Raw[-c(1), ]
    
    Department1<-Raw$Department
    Department1<-unique(Department1)
    Department1<-as.character(Department1)
    selectInput("Department", "Choose Department:", choices = Department1, selected = 2, multiple = FALSE)
  })  
  
  #output$Department_bar = renderRpivotTable({
  output$Department_bar = renderPlot({
  #output$Department_bar = renderPlotly({ 
    # If missing input, return to avoid error later in function
    if(is.null(input$Department))
      return()
    
#     inFile <- input$file1
#     if (is.null(inFile))
#       return(NULL)
#     
#     Raw<-read.csv(inFile$datapath, header=TRUE ,sep=",") 
#     Raw<- Raw[-c(1, 2, 3), ]
#     
#     #Delete unused column
#     Raw$RFQ.Project<-NULL
#     Raw$Standard.Project<-NULL
#     Raw$Sustaining.Project<-NULL
#     Raw$Function.Project<-NULL
#     Raw$Others<-NULL
#     Raw$Absent<-NULL
#     Raw$Total.1<-NULL
#     Raw$Other.1<-NULL
#     Raw$TC.1<-NULL
#     Raw$ADM.1<-NULL
#     Raw$Teach.1<-NULL
#     Raw$Study.1<-NULL
#     Raw$Project.2<-NULL
#     Raw$Training.1<-NULL
#     Raw$Project.1<-NULL
#     
#     names(Raw)[1]<-"NO"
#     names(Raw)[2]<-"Department"
#     names(Raw)[3]<-"Function_team"
#     names(Raw)[4]<-"Employee_ID"
#     names(Raw)[5]<-"Name"
#     names(Raw)[6]<-"Dept_code"
#     names(Raw)[7]<-"Cap"
#     names(Raw)[8]<-"absent_hours"
#     names(Raw)[9]<-"Total_Hours"
#     names(Raw)[10]<-"Input_Hours"
#     Raw$Employee_ID<-NULL
#     Raw$Name<-NULL
#     Raw$Absent<-NULL
#     
#     colnames(Raw)[grepl("Study", colnames(Raw))]<-"Others3"
#     colnames(Raw)[grepl("Teach", colnames(Raw))]<-"Others4"
#     colnames(Raw)[grepl("ADM", colnames(Raw))]<-"Others5"
#     colnames(Raw)[grepl("TC", colnames(Raw))]<-"Others6"
#     colnames(Raw)[grepl("Other", colnames(Raw))]<-"Others7"
#     colnames(Raw)[which(names(Raw) == "Project")] <- "Others"
#     colnames(Raw)[which(names(Raw) == "Training")] <- "Others"
    
    Raw<-Raw()
    # Keep standard projects only
    
    Raw2<-Raw[ , grepl( "Standard", names(Raw))]
    Raw3<-cbind(Raw[, 1:2],Raw2)
    
    # Covert to matrix and replace the column names using the 1st row
    Raw3<-as.matrix(Raw3)
    colnames(Raw3)<-Raw3[1,]
    Raw3<- Raw3[-c(1), ]
    
    # Convert to data frame 
    Raw3<-as.data.frame(Raw3)
    #Raw3$`Standard Project - JV`<-NULL
    # Factor to numeric
    for (i in 3:ncol(Raw3))
    {
      Raw3[,i]<-as.numeric(levels(Raw3[,i]))[Raw3[,i]]
    }
    
    
    #----------------new------------------------ 
    names(Raw3)[2]<-"Department"   #2
    #Raw3$`##`<-NULL
    #Raw3$Code<-NULL
    #Raw3$`部門簡稱2`<-NULL
    Raw3$`Standard Project - HP`<-NULL
    Raw3$`Standard Project - JV`<-NULL
    Raw3$`Standard Project - Ali`<-NULL
    Raw3$`Standard Project - Others`<-NULL
    
    #----------------new------------------------ 
    
    dept_agg<-aggregate(Raw3[,3:length(Raw3)], 
                        by=list(
                          Raw3$Department,
                          Raw3$Function
                        ), FUN=sum)
    #dept_agg<-dept_agg[-c(1), ]
    names(dept_agg)[1]<-"Department"
    names(dept_agg)[2]<-"Function_team"
    dept_Melt<-melt(dept_agg, id=c("Department","Function_team"))
    
    colnames(dept_Melt)[grepl("value", colnames(dept_Melt))]<-"Man_Hours"
    colnames(dept_Melt)[grepl("variable", colnames(dept_Melt))]<-"Project"
    #rpivotTable(dept_Melt)
    selected_Department<-dept_Melt[ which(dept_Melt$Department == input$Department), ]
    selected_Department<-selected_Department[ which(selected_Department$Man_Hours>50),] 
  
     
    # adding lables and positions     
    selected_Department<-ddply(selected_Department, .(Function_team), 
                               transform, pos = cumsum(Man_Hours) - (0.5*Man_Hours)
                              )
    
    ggplot(selected_Department, aes(x=Function_team, y=Man_Hours, fill=Project, order=Project, width=0.6))+ 
      geom_bar(stat = "identity", position = "stack")+scale_fill_hue(guide=guide_legend(reverse = TRUE))+
      theme(text = element_text(size=15), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25,size=13))+ 
      geom_text(aes(label=Man_Hours, y=pos), size=5)
    })
  
#------------------Historic Data (From Pstgres)----------------------  
  output$hcontainer <- renderHighchart({
    
#---------------climate demo----------------------    
#     hc <- hc_demo() %>%
#       hc_chart(type = input$type)
#     data(citytemp)
#     hc <- highchart() %>% 
#       hc_chart(type = input$type)%>% 
#       hc_xAxis(categories = citytemp$month) %>% 
#       hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
#       hc_add_series(name = "London", data = citytemp$london) %>% 
#       hc_add_series(name = "Other city",
#                     data = (citytemp$tokyo + citytemp$london)/2)
#     
#     if (input$stacked != FALSE) {
#       hc <- hc %>%
#         hc_plotOptions(series = list(stacking = input$stacked))
#     }
#     
#     if (input$theme != FALSE) {
#       theme <- switch(input$theme,
#                       null = hc_theme_null(),
#                       darkunica = hc_theme_darkunica(),
#                       gridlight = hc_theme_gridlight(),
#                       sandsignika = hc_theme_sandsignika(),
#                       fivethirtyeight = hc_theme_538(),
#                       economist = hc_theme_economist(),
#                       chalk = hc_theme_chalk(),
#                       handdrwran = hc_theme_handdrawn()
#       )
#       hc <- hc %>% hc_add_theme(theme)
#     }
#     hc
   
#------------time series demo--------------------------------------------    
#     library(xts)
#     DATE<-c("20070103","20070104","20070105","20070106","20070107")
#     CLOSE<-c(51,52,58,50,49)
#     OPEN<-c(21,22,20,23,25)
#     PRICE<-data.frame(DATE, CLOSE, OPEN)
#     PRICE$DATE <- as.Date(as.character(PRICE$DATE),format="%Y%m%d")
#     close_ts <- xts(PRICE$CLOSE,PRICE$DATE)
#     open_ts <- xts(PRICE$OPEN,PRICE$DATE)
#   
#       hc<-highchart() %>% 
#       #hc<-highchart(type = "stock") %>% 
#       hc_chart(type = input$type)%>% 
#       #hc_title(text = "SRD Utilization & Others Trend ") %>% 
#       hc_subtitle(text = "Utilization stands for the man hour percentag of RFQ, Standard, Sustaining Projects") %>% 
#       hc_add_series_xts(open_ts, name = "open_ts") %>% 
#       hc_add_series_xts(close_ts, name = "close_ts") %>% 
#       hc_add_theme(hc_theme_smpl())
#       if (input$stacked != FALSE) {
#         hc <- hc %>%
#           hc_plotOptions(series = list(stacking = input$stacked))
#       }
#       
#       if (input$theme != FALSE) {
#         theme <- switch(input$theme,
#                         null = hc_theme_null(),
#                         darkunica = hc_theme_darkunica(),
#                         gridlight = hc_theme_gridlight(),
#                         sandsignika = hc_theme_sandsignika(),
#                         fivethirtyeight = hc_theme_538(),
#                         economist = hc_theme_economist(),
#                         chalk = hc_theme_chalk(),
#                         handdrwran = hc_theme_handdrawn()
#         )
#         hc <- hc %>% hc_add_theme(theme)
#       }
#     hc

    rm(melt_db)
    melt_db <- dbGetQuery(con, "SELECT * from dms_melt")
    melt_db$row.names<-NULL
    melt_db$id<-NULL
    melt_db<-melt_db[(melt_db$project_type != "Input_Hours")
                               &(melt_db$project_type !="Total_Hours"), ]
    
    split <- split(melt_db, melt_db$time)
    #rm(melt_db)
    
    util_group<-list()
    others_group<-list()
    absent_group<-list()
    util_rate<-list()
    Others_rate<-list()
    absent_rate<-list()
    time_month<-list()
    
    for (i in 1:length(split)) {
      # Sum the manhours by project type
      split[[i]]<-aggregate(split[[i]]$man_hour, 
                            by=list(split[[i]]$time, split[[i]]$project_type), FUN=sum)
      
      colnames(split[[i]])[grepl("Group.1", colnames(split[[i]]))]<-"month"
      colnames(split[[i]])[grepl("Group.2", colnames(split[[i]]))]<-"project_type"
      colnames(split[[i]])[grepl("x", colnames(split[[i]]))]<-"manhour"
      
      # Compute percentage
      split[[i]]$percent<-(split[[i]]$manhour)/sum(split[[i]]$manhour)
      
      # subset the group
      util_group[[i]]<- split[[i]][(split[[i]]$project_type != "absent_hours")
                                   &(split[[i]]$project_type != "Others"), ]
      others_group[[i]]<- split[[i]][(split[[i]]$project_type == "Others"), ]
      absent_group[[i]]<- split[[i]][(split[[i]]$project_type == "absent_hours"), ]
      
      #compute the rate
      util_rate[[i]]<-sum(util_group[[i]]$percent)*100
      Others_rate[[i]]<-sum(others_group[[i]]$percent)*100
      absent_rate[[i]]<-sum(absent_group[[i]]$percent)*100
      time_month[[i]]<-unique(split[[i]]$month)
    }
    high_table<-cbind(time_month, util_rate, Others_rate, absent_rate)
    high_table<-as.data.frame(high_table)
    
    # Build the hc chart
    hc <- highchart() %>% 
      hc_title(text = "SRD Manpower Utilization Trend ", style = list(fontSize = "28px")) %>% 
      hc_subtitle(text = "The utilization rates includes standard, RFQ, sustaining and function projects.",
                  style = list(color = "#2b908f",fontSize = "23px" )) %>%
      
      hc_chart(type = input$type)%>%
      hc_xAxis(categories = high_table$time_month, title = list(text = "Month")) %>% 
      hc_yAxis(title = list(text = "Percentage"),
               labels = list(format = "{value}%"), max = 100) %>%
      
      hc_add_series(name = "Utilization Rate", data = high_table$util_rate) %>% 
      hc_add_series(name = "Others Rate", data = high_table$Others_rate) %>% 
      hc_add_series(name = "Absent Rate",data = high_table$absent_rate)
    
    if (input$stacked != FALSE) {
      hc <- hc %>%
        hc_plotOptions(series = list(stacking = input$stacked))
    }
    
    if (input$theme != FALSE) {
      theme <- switch(input$theme,
                      null = hc_theme_null(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      economist = hc_theme_economist(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn()
      )
      hc <- hc %>% hc_add_theme(theme)
    }
    hc
  })
  
  output$historic_pivot= renderRpivotTable({
 
     melt_db$row.names<-NULL
     melt_db$id<-NULL
     rpivotTable(melt_db)
  })

  output$by_project_pivot= renderRpivotTable({
    
	rm(by_project_db)
    by_project_db <- dbGetQuery(con, "SELECT * from manhour_by_project")
    by_project_db$row.names<-NULL
    by_project_db$id<-NULL

    rpivotTable(by_project_db)
  })
  
  
#------------------Dept.& Project Expense (2nd Phase)---------------------------------- 
  #Raw<- reactive({
  #output$expense_table <- renderDataTable({
  expense_script <- reactive({
    inFile <- input$file2
    if (is.null(inFile))
      return(NULL)
    
    expense_script<-read.csv(inFile$datapath,header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="big5")
    #expense_script<-expense_script[,c(2,4,6,7,10,11,12,13,14,15,16,23)]
	expense_script<-expense_script[,c(4,6,7,11,12,13,14,15,16,17,18,19,20)]
	expense_script
  })
  
   #output$melt_upload2<-renderDataTable({
   output$melt_upload2<-renderText({
    if(is.null(input$month2))
      return(NULL)

    validate(
      need(input$month2 !="", "The input format: 201204, 201212")
    )
    #Get the month from DB
    rm(monthly_expense)
    monthly_expense <- dbGetQuery(con, "SELECT * from expense_table2")
    expense_db<-unique(monthly_expense$年月)

    #Get the month from user
    expense_usr<-input$month2

    #Compare month_db & month_usr
    sum=0
    for (i in 1:length(expense_db))
    {
      if (grepl(expense_db[i], expense_usr)=="TRUE")
      {
        sum=sum+1
      }
    }
	expense_script<-expense_script()
  expense_script$年月<-expense_usr
	expense_script$USD<-as.numeric(expense_script$USD)
	expense_script$KNTD<-as.numeric(expense_script$KNTD)
	
    if (sum==0)  # upload
     {
		if (dbWriteTable(con, "expense_table2", value = expense_script, append=T, row.names=FALSE)=="TRUE")
		{
		  print("Uploaded successfully!")
		}		
     }
	else         #delete and upload
	 {     
	    if (all(dbGetQuery(con, "DELETE FROM expense_table2 WHERE 年月 = $1 ", list(expense_usr)), 
	           dbWriteTable(con, "expense_table2", value = expense_script, append=T, row.names=FALSE))=="TRUE")
		{
		  print("Updated successfully!")
		}
	 }   
   })
   
   output$expense_pivot= renderRpivotTable({
    #inFile <- input$file2
    #if (is.null(inFile))
      #return(NULL)
    #expense_script<-read.csv(inFile$datapath,header=TRUE, sep=",", stringsAsFactors=FALSE, fileEncoding="big5")
    #rpivotTable(expense_script)
	rm(monthly_expense)
	monthly_expense <- dbGetQuery(con, "SELECT * from expense_table2")
	#monthly_expense$年月<-NULL
	#monthly_expense$大分類.JV分類.<-NULL
	rpivotTable(monthly_expense)
  })
  
  output$Project_expense <- renderHighchart({
    highchart() %>% 
      hc_chart(polar = TRUE, type = "line") %>% 
      hc_title(text = "Project Budget vs Spending") %>% 
      hc_xAxis(categories = c('Captical Equipment', 'Manpower', 'Cert&lab fees', 'Business Strategy', 
                              'Sampling Cost', 'Personnal Expense'),
               tickmarkPlacement = 'on',
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = 'polygon',
               lineWidth = 0,
               min = 0) %>% 
      hc_series(
        list(
          name = "Allocated Budget",
          data = c(43000, 19000, 60000, 35000, 17000, 10000),
          pointPlacement = 'on'
        ),
        list(
          name = "Actual Spending",
          data = c(50000, 39000, 42000, 31000, 26000, 14000),
          pointPlacement = 'on'
        )
      )
  })
})

