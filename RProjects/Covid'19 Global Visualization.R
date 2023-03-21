#Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("shiny")
install.packages("plotly")
install.packages("maps")
install.packages("scales")

#Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)
library(plotly)
library(maps)
library(scales)

############################################################Data Fetching#####################################################

###Original Data Format##
confirmed_cases<-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
confirmed_data<-read_csv(url(confirmed_cases))
view(confirmed_data)
deaths_globally<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
deaths<-read_csv(url(deaths_globally))
view(deaths)
recovered_cases <-"https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
recovered<-read_csv(url(recovered_cases))

#########Joined and Mutated data########
URL_used = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

updation_time = function(fileName) {
  
  systemtime=as.numeric(as.POSIXlt(Sys.time()))
  filecreation_time=as.numeric(file.info(fileName)$ctime)
  
  ( systemtime-filecreation_time)/60 #####last updated time
  
}
Data_Call = function(file_saved, columnName) {
  print(paste0("File was last updated at:", updation_time(file_saved)))
  if(!file.exists(file_saved) ||
     
     updation_time(file_saved) > 10) { 
    
    data = read.csv(file.path(URL_used, file_saved),
                    
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      
      select(everything())%>%
      
      pivot_longer( names_to="date", cols=-c(1:4), values_to=columnName )%>%###tidyr
      
      mutate(
        
        date=as.Date(date, format="%m/%d/%y"),
        
        `Province/State`=
          
          ##if_else(`Province/State` == "", "<all>", `Province/State`)
          ifelse(`Province/State` %in% "", `Country/Region`, `Province/State`)### No Province name with corresponding country name
      )
    
    save(data, file=file_saved) 
    
    
  } else {
    
    load(file=file_saved)
    
    
  }
  
  return(data)
  
}

COVID_Data =
  Data_Call(
    "time_series_covid19_confirmed_global.csv", "Confirmed") %>%
  inner_join(Data_Call(
    "time_series_covid19_deaths_global.csv", "Deaths")) %>%
  inner_join(Data_Call(
    "time_series_covid19_recovered_global.csv","Recovered"))#####dplyr
COVID_Data

##################################################Arranging Data###############################################################
colnames(COVID_Data) ###column names
unique(COVID_Data$`Country/Region`) ###177 Countries
unique(COVID_Data$`Province/State`) ###Province
data_arranged<-COVID_Data
data_arranged<-data_arranged %>%
  gather(key="status",
         value="Data",Confirmed, Deaths,
         Recovered) #### wide to long for status
view(data_arranged)

#############################################China Province Wise Confirmed ####################################################
data_china<-filter(data_arranged, data_arranged$`Country/Region` == "China")
view(data_china)
data_china=data_china[-c(2)]
data_china<-filter(data_china, status == "Confirmed")
data_china = data_china %>% 
  group_by(`Province/State`) %>% 
  summarise_if(is.numeric, max, na.rm=TRUE)
fig_china <- plot_ly(data_china,x =data_china$`Province/State`,y=data_china$Data, type = 'scatter',mode="markers")
fig_china <- fig_china %>% layout(xaxis=list(title="Province"),yaxis = list(title="Confirmed Cases Count",type = "log"),title="Logarithmic Plot for Confirmed Cases In China")
fig_china

##################################################US Cases#########################################################
data_US<-filter(data_arranged, data_arranged$`Country/Region` == "US")
view(data_US)
fig_US <- plot_ly(data = data_US, x = data_US$date, y = data_US$Data, color = data_US$status, colors = "Set1",type="scatter",mode="markers")
fig_US <- fig_US %>% layout(xaxis=list(title="Duration",tickangle=30,tickformat = "%d %b %y"),yaxis = list(title="Cases Count"),title="Plot Showing COVID-19 Evolving in US")
fig_US

#############################################6 Countries COVID-19 Evolvement####################################
data2_6countries <- data_arranged[data_arranged$`Country/Region` %in% c("US", "United Kingdom", "Italy","Germany","China","India"), ]
plot_6countries<-ggplot(data2_6countries,aes(x = date, y = Data, color =status)) +geom_line(lwd =0.3) +facet_wrap(~ data2_6countries$`Country/Region`, scales = "free")+
  labs(title = sprintf('Trajectories of the Status of Coronavirus \n')) +
  xlab('Duration') + ylab('Total Numbers') + scale_y_continuous(labels = label_comma())+theme_bw()+
  theme(plot.title = element_text(hjust = 0.55,family="Italica"),
        plot.subtitle = element_text(hjust =1.2),
        axis.text.x = element_text(angle = 0, hjust = 2),
        panel.spacing.x = unit(5, "mm"),
        panel.spacing.y = unit(5, "mm"),
        plot.margin = unit(c(0.3,0.3,0.5,1), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid = element_blank())
ggplotly(plot_6countries)

####################################################Map using ggplot and plotly##############################################
world_confirmed<-COVID_Data
world=map_data("world")
world_confirmed<-world_confirmed %>%
  gather(key="status",
         value="Data",Confirmed, Deaths,
         Recovered)
world_confirmed<-filter(world_confirmed, status == "Confirmed")
world_confirmed_plot<-ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=world_confirmed, aes(x=Long, y=Lat,color=Data),stroke=F, alpha=0.7) +
  labs(title = "Countries with Confirmed COVID-19 Cases Worlwide")+scale_colour_gradient(low = "blue", high = "red", na.value = NA,labels=label_comma())+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(world_confirmed_plot)

################################################Death Percent################################################################
deaths_percent<-filter(data_arranged, (status == "Confirmed"|status=="Deaths"))
deaths_percent = deaths_percent %>% group_by(`Country/Region`,status) %>% summarise_if(is.numeric, max, na.rm=TRUE)
view(deaths_percent)
deaths_percent<-deaths_percent %>% spread(status, Data)
deaths_percent$Percentage<-round((deaths_percent$Deaths/deaths_percent$Confirmed) *100,2)
view(deaths_percent)
deaths_percent_plot<-ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(data=deaths_percent, x=deaths_percent$Long, y=deaths_percent$Lat,aes(size=Percentage,`Country/Region`=`Country/Region`),stroke=F, alpha=0.5,color="red") +
  labs(title = "Countries in Terms of Mortality Rate of COVID-19 Cases Worlwide")+
  theme(plot.title = element_text(hjust = 0.5))
ggplotly(deaths_percent_plot)

################################################### Top 10 Recover Rate Globally######################################################
recovery_percent<-filter(data_arranged, (status == "Confirmed"|status=="Recovered"))
recovery_percent = recovery_percent %>% group_by(`Country/Region`,status) %>% summarise_if(is.numeric, max, na.rm=TRUE)
recovery_percent<-recovery_percent %>% spread(status, Data)
recovery_percent$Percentage<-round((recovery_percent$Recovered/recovery_percent$Confirmed) *100,2)
view(recovery_percent)
recovery_percent<-recovery_percent[c(1,6)]
recovery_percent=as.data.frame(recovery_percent)
ordering_data<-recovery_percent[order(recovery_percent$Percentage),]
top15_recovering<-top_n(ordering_data,10)
barchart <- plot_ly(
  x = top15_recovering$`Country/Region`,
  y = top15_recovering$Percentage,
  type = "bar",
  
)
barchart <- barchart %>% layout(title = '<b>Bar Plot for Recovery Rate Globally</b>',xaxis=list(title = '<b>Countries</b>',tickangle=30),yaxis=list(title='<b>Percentage<b>'))
barchart

##################################################Using New  Count for cases as Data############################################################
new_data<-COVID_Data
new_data$NewConfirmed=new_data$Confirmed - lag(new_data$Confirmed, default=0)
new_data$NewDeaths=new_data$Deaths - lag(new_data$Deaths, default=0)
new_data$NewRecovered=new_data$Recovered - lag(new_data$Recovered, default=0)
view(new_data)
new_data<-new_data %>%
  gather(key="Newstatus",
         value="NewCount",NewConfirmed, NewDeaths,
         NewRecovered)
view(new_data)
data_newdeaths<-filter(new_data, Newstatus == "NewDeaths")
data_newconfirmed<-filter(new_data, Newstatus == "NewConfirmed")
data_newRecovered<-filter(new_data, Newstatus == "NewRecovered")
data_newdeaths_sf<-data_newdeaths%>%select(`Country/Region`,NewCount,Lat,Long)
data_newdeaths<-data_newdeaths%>%select(`Country/Region`,NewCount)
data_newconfirmed<-data_newconfirmed%>%select(`Country/Region`,NewCount)
data_newRecovered<-data_newRecovered%>%select(`Country/Region`,NewCount)
data_newconfirmed=data_newconfirmed%>%
  group_by(`Country/Region`) %>%
  summarise(NewCount = last(NewCount))
data_newdeaths=data_newdeaths%>%
  group_by(`Country/Region`) %>%
  summarise(NewCount = last(NewCount))
data_newRecovered=data_newRecovered%>%
  group_by(`Country/Region`) %>%
  summarise(NewCount = last(NewCount))

###################################################Top 10 countries for New Count###############################################
new_dc<-data_newconfirmed%>%top_n(10)
new_dd<-data_newdeaths%>%top_n(10)
new_dr<-data_newRecovered%>%top_n(10)
unique(new_dc$`Country/Region`)
unique(new_dd$`Country/Region`)
unique(new_dr$`Country/Region`)

###################################################PieChart for Top 10 Countries of New Cases################################
piechart <- plot_ly(width = 1108, height = 750)
piechart <- piechart %>% add_pie(data = new_dc, labels = new_dc$`Country/Region`, values = new_dc$NewCount,
                                 name = "NewConfirmed", domain = list(x = c(-0.02,0.5), y = c(0.4, 1)),insidetextfont = list(size = 12))
piechart <- piechart %>% add_pie(data = new_dd, labels =new_dd$`Country/Region`, values = new_dd$NewCount,
                                 name = "NewDeaths", domain = list(x = c(0.57,3), y = c(0.44,1)),insidetextfont = list(size = 11))
piechart <- piechart %>% add_pie(data = new_dr, labels = new_dr$`Country/Region`, values = new_dr$NewCount,
                                 name = "NewRecovered", domain = list(x = c(0.05,1.7), y = c(0,0.5)),insidetextfont = list(size =10))
piechart <- piechart %>% layout(title = "<b>COVID-19 New Counts for top 10 Countries</b>", showlegend = TRUE)
piechart<-piechart %>% add_annotations(x=0.02,y=0.91,text="<b>New Confirmed")
piechart<-piechart %>% add_annotations(x=0.58,y=0.91,text="<b>New Deaths</b>")
piechart<-piechart %>% add_annotations(x=0.28,y=0.3,text="<b>New Recovered</b>")
piechart

###################################################Shiny:Combined Globally#########################################################
ui = (fluidPage(
  titlePanel("Global Spread of COVID-19"),
  fluidRow(
    column(
      4, 
      selectizeInput("Country", label=h5("Country"), choices=NULL, width="100%")
    ),
    column(
      4, 
      selectizeInput("State", label=h5("State / Province"), choices=NULL, width="100%")
    ),
    column(
      4, 
      checkboxGroupInput(
        "metrics", label=h5("Selected Metrics"), 
        choices=c("Confirmed", "Deaths", "Recovered"), 
        selected=c("Confirmed", "Deaths", "Recovered"), width="100%")
    )
  ),
  fluidRow(
    plotlyOutput("NewCount")
  ),
  fluidRow(
    plotlyOutput("TotalCount")
  )
))

server = function(input, output, session) { 
  globally = reactive({
    globally = COVID_Data %>%
      filter(`Country/Region` == input$Country)
    if(input$State != " ") {
      globally = globally %>% 
        filter(`Province/State` == input$State) 
    } else {
      globally = globally %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm=TRUE)
    }
    
    globally %>%
      mutate(
        date_converted = format(date, format="%b %d"),   
        NewConfirmed=Confirmed - lag(Confirmed, default=0),
        NewRecovered=Recovered - lag(Recovered, default=0),
        NewDeaths=Deaths - lag(Deaths, default=0)
      )
  })
  
  observeEvent(input$Country, {
    States = COVID_Data %>%
      filter(`Country/Region` == input$Country) %>% 
      pull(`Province/State`)
    States = c(sort(unique(States)))
    updateSelectInput(session, "State", choices=States, selected=States[1])
  })
  
  countries = sort(unique(COVID_Data$`Country/Region`))
  
  updateSelectInput(session, "Country", choices=countries, selected="China")
  
  renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle, xaxisTitle) {
    renderPlotly({
      data = globally()
      combinedbarchart = data %>% 
        plot_ly() %>%
        config(displayModeBar=FALSE) %>%
        layout(
          barmode='group', 
          xaxis=list(
            title= xaxisTitle, tickangle=-90, type='category', ticktext=as.list(data$date_converted), 
            tickvals=as.list(data$date), gridwidth=1,size=7), 
          yaxis=list(
            title=yaxisTitle
          ),
          legend=list(x=0.01, y=0.95, font=list(size=7)),
          font=list(face=2,size=9)
        )
      for(metric in input$metrics) 
        combinedbarchart = combinedbarchart %>%
        add_trace(
          x= ~date, y=data[[paste0(varPrefix, metric)]], type='bar', 
          name=paste(legendPrefix, metric, "Cases"),
          marker=list(
            color=switch(metric, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
            line=list(color='rgb(8,48,107)', width=0.5)
          )
        )
      combinedbarchart
    })
  }
  
  output$NewCount = renderBarPlot("New", legendPrefix="New", yaxisTitle="<b>New Cases</b> ", xaxisTitle="<b>Days</b>")
  output$TotalCount = renderBarPlot("",legendPrefix="Cumulated", yaxisTitle="<b>Total Cases</b>", xaxisTitle="<b>Days</b>")
}
# Run the application 
shinyApp(ui = ui, server = server)

###################################################################################################################################################################################