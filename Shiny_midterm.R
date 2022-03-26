library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(DT)
library(caret)
library(randomForest)


dat.test<-read.csv("C:\\ML_midterm\\dat.test_63-2.csv")
dat.test<-dat.test%>%mutate_if(is.numeric, round, digits = 2)
glimpse(dat.test)

model<-readRDS("C:\\ML_midterm\\fit_rf.rds")



predict(model, newdata = dat.test[6,])

#user interface part
ui<-fluidPage(
	navbarPage(strong("Research Education"), id = "tabset",
	theme = shinytheme("united"),
	tabsetPanel(
		tabPanel("ข้อมูลทั่วไป",
			sidebarLayout(
				sidebarPanel(width = 3,
					textInput("ID","กรุณากรอกรหัสนิสิต :"),
					selectInput("charts",
							label = c("เลือกแผนภาพที่ต้องการ"),
							choices = c("histogram", "boxplot")),
					br(),
					#h4("รายงานคะแนนของ"),
					#textOutput("stuName"),
					#textOutput("stuID")
				),
				mainPanel(width = 9,
					h4("แผนภาพแสดงคะแนนสอบกลางภาคของนิสิตทุกคน"),
					plotOutput("plot1", height="250px"),
					br(),
					h4("คะแนนรายบุคคลของนิสิต"),
					DTOutput("table")
				) #end main panel
			),
		),
		tabPanel("คาดการณ์ผลการเรียน",
			fluidRow(h2("การคาดการณ์ผลการเรียนในอนาคตของนิสิต"),
				column(5,
					h2("\n"),
					h5("คาดการณ์จากคะแนนการร่วมกิจกรรม การส่งงานระหว่างเรียน"),
					h5("รวมถึงคะแนนสอบกลางภาคของนิสิต โดยใช้การทำนายจาก"),
					h5("การวิเคราะห์ Machine learning โดยนำข้อมูลในภาคเรียน"),
					h5("และปีการศึกษาก่อนหน้ามาเป็นชุดข้อมูลสำหรับให้เครื่อง"),
					h5("เรียนรู้ (training dataset) สำหรับสร้างโมเดลทำนาย"),
					h5("โดยใช้การวิเคราะห์ Random Forest")
				),
				column(7,
					h3("จากข้อมูลคะแนนของนิสิต"),
					h4(textOutput("stuName")),
					h4(textOutput("stuID")),
					h4(""),
					h4(textOutput("stuMidterm"),
					br(),
					h3("คำแนะนำสำหรับนิสิต"),
					h4(textOutput("stuPred"),
					br(),
					em(h4(textOutput("advs1"), align = "center"),
					em(h4(textOutput("advs2"), align = "center"))
				)
			)
		)
		
	))
))))

#server part
server<-function(input, output){
	
	# general data
	dat.filtered<-reactive({
		dat.test[,2:12]%>%filter(ID==input$ID)
	})

	output$stuName<-renderText({
   		 paste("ชื่อ-สกุล : ", dat.filtered()$name, dat.filtered()$surname)
  	})
  
  	output$stuID<-renderText({
    		paste("รหัสนิสิต : ", input$ID)
  	})
  

 	output$stuMidterm<-renderText({
   		paste("คะแนนสอบกลางภาคของนิสิต : ", dat.filtered()$midterm, " คะแนน จากคะแนนเต็ม 25 คะแนน")
    	})




	# total histogram
	output$plot1<-renderPlot({
		if(input$charts=="histogram"){ggplot(dat.test)+
							geom_histogram(aes(midterm),
								bins=20, binwidth=1.5,
								fill="#FE7E6D")+
								labs(x="คะแนนสอบกลางภาค", y="จำนวนนิสิต")+
							theme_minimal()}
		else{ggplot(dat.test, aes(midterm))+geom_boxplot(fill = "#FE7E6D", 
								colour = "black")+ 
								labs(x="คะแนนสอบกลางภาค")+
								theme_minimal()}
	})
	
	# individual table
	output$table<-renderDT({
			dat.filtered()[,c(2:4,6:10)]%>%datatable(options = list(dom = 't') ) #extensions = list()
	})


	# predict output
	datInput<-reactive({
			data.frame(midterm = dat.filtered()$midterm,
					activity = dat.filtered()$activity*2, 
					stringsAsFactors = FALSE)				
	})	
	
	pred <- reactive({
		as.character(stats::predict(model, newdata=datInput()))
				
	})
  	
	output$stuPred<-renderText({
		if(pred()=="good"){"นิสิตมีแนวโน้มที่จะ...มีผลสัมฤทธิ์ที่ดีในรายวิชานี้"}
		else if(pred()=="pass"){"นิสิตมีแนวโน้มที่จะ...สามารถผ่านเกณฑ์ในรายวิชานี้"}
		else{"นิสิตมีแนวโน้มที่จะ...ไม่ผ่านในรายวิชานี้"}
	})
	
	# advise for student
	output$advs1<-renderText({
		if(dat.filtered()$activity*2 < 50){"พยายามส่งงานให้ครบ"}
		else{"พยายามอ่านหนังสือเพิ่ม"}
	})

	output$advs2<-renderText({
		if(pred()=="good"){"ผลสัมฤทธิ์ที่ดีรออยู่อีกไม่ไกล"}
		else if(pred()=="pass"){"และพยายามให้มากขึ้นอีดนิดจะผ่านรายวิชานี้"}
		else{"ตั้งใจเรียนให้มากขึ้นและทบทวนบทเรียนให้มาก ๆ นะ"}
	})



}

# create Shiny app!!
shinyApp(ui=ui, server=server)






