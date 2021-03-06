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
		tabPanel("�����ŷ����",
			sidebarLayout(
				sidebarPanel(width = 3,
					textInput("ID","��سҡ�͡���ʹ��Ե :"),
					selectInput("charts",
							label = c("���͡Ἱ�Ҿ����ͧ���"),
							choices = c("histogram", "boxplot")),
					br(),
					#h4("��§ҹ��ṹ�ͧ"),
					#textOutput("stuName"),
					#textOutput("stuID")
				),
				mainPanel(width = 9,
					h4("Ἱ�Ҿ�ʴ���ṹ�ͺ��ҧ�Ҥ�ͧ���Ե�ء��"),
					plotOutput("plot1", height="250px"),
					br(),
					h4("��ṹ��ºؤ�Ţͧ���Ե"),
					DTOutput("table")
				) #end main panel
			),
		),
		tabPanel("�Ҵ��ó�š�����¹",
			fluidRow(h2("��äҴ��ó�š�����¹�͹Ҥ��ͧ���Ե"),
				column(5,
					h2("\n"),
					h5("�Ҵ��ó�ҡ��ṹ��������Ԩ���� ����觧ҹ�����ҧ���¹"),
					h5("����֧��ṹ�ͺ��ҧ�Ҥ�ͧ���Ե �����÷ӹ�¨ҡ"),
					h5("����������� Machine learning �¹Ӣ�������Ҥ���¹"),
					h5("��лա���֡�ҡ�͹˹�����繪ش����������Ѻ�������ͧ"),
					h5("���¹��� (training dataset) ����Ѻ���ҧ���ŷӹ��"),
					h5("�������������� Random Forest")
				),
				column(7,
					h3("�ҡ�����Ť�ṹ�ͧ���Ե"),
					h4(textOutput("stuName")),
					h4(textOutput("stuID")),
					h4(""),
					h4(textOutput("stuMidterm"),
					br(),
					h3("���й�����Ѻ���Ե"),
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
   		 paste("����-ʡ�� : ", dat.filtered()$name, dat.filtered()$surname)
  	})
  
  	output$stuID<-renderText({
    		paste("���ʹ��Ե : ", input$ID)
  	})
  

 	output$stuMidterm<-renderText({
   		paste("��ṹ�ͺ��ҧ�Ҥ�ͧ���Ե : ", dat.filtered()$midterm, " ��ṹ �ҡ��ṹ��� 25 ��ṹ")
    	})




	# total histogram
	output$plot1<-renderPlot({
		if(input$charts=="histogram"){ggplot(dat.test)+
							geom_histogram(aes(midterm),
								bins=20, binwidth=1.5,
								fill="#FE7E6D")+
								labs(x="��ṹ�ͺ��ҧ�Ҥ", y="�ӹǹ���Ե")+
							theme_minimal()}
		else{ggplot(dat.test, aes(midterm))+geom_boxplot(fill = "#FE7E6D", 
								colour = "black")+ 
								labs(x="��ṹ�ͺ��ҧ�Ҥ")+
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
		if(pred()=="good"){"���Ե�����������...�ռ����ķ�����������Ԫҹ��"}
		else if(pred()=="pass"){"���Ե�����������...����ö��ҹࡳ�������Ԫҹ��"}
		else{"���Ե�����������...����ҹ�����Ԫҹ��"}
	})
	
	# advise for student
	output$advs1<-renderText({
		if(dat.filtered()$activity*2 < 50){"�������觧ҹ���ú"}
		else{"��������ҹ˹ѧ�������"}
	})

	output$advs2<-renderText({
		if(pred()=="good"){"�����ķ�������������ա�����"}
		else if(pred()=="pass"){"��о���������ҡ����մ�Դ�м�ҹ����Ԫҹ��"}
		else{"�������¹����ҡ�����з��ǹ�����¹����ҡ � ��"}
	})



}

# create Shiny app!!
shinyApp(ui=ui, server=server)






