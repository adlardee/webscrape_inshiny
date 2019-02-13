############################################# GLOBAL FILE BELOW ####################################

setwd("C:/Users/adlar/Desktop/webscrape_inshiny")
library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)
library(leaflet)
library(googleVis)
library(corrplot)
library(ggcorrplot)
library(tidyr)
library(tidyverse)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(memoise)

#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes


##MEDIUM DATASET##
#loading medium dataset
medium = read.csv(file = "mediumfinal.csv")
medium
head(medium)
#removing nonclean date column
medium$Date = NULL
head(medium)
#renaming of date.fixed column
colnames(medium)[colnames(medium)=="Date.Fixed"] = "Date"
head(medium)

##TAGS DATASET##
#loading tags dataset
tags = read.csv(file = "tags.csv")
head(tags)

##COMBINED MEDIUM AND TAGS##
#combine of tags and medium dataset
combined = cbind(medium, tags)
head(combined)

str(combined)

#install.packages("tidyverse")
#gather in tidyverse to melt the dataframe
combined2 = gather(combined, Drop, Tag, X0:X7)
combined2

#dropping these two columns
combined2$Drop = NULL
combined2$X = NULL

?gather


combined3 = na.omit(combined2, cols="Tags", )
#removes an empty cells
combined4 = combined3[combined3$Tag != "",]
combined4
#we are ok with some missing tags in combined 4 because we are only looking at tag correlations.

#creating new dataframes so that I can looks at the summary statistics
combined12 = combined[combined$Tags == 'None',]
combined13 = combined[combined$Tags != 'None',]
summary(combined12$Claps)
summary(combined13$Claps)
sd(combined12$Claps)
sd(combined13$Claps)

###############Word Cloud Setup#################
text = readLines("wordcloudtags.txt")
text
docs = Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d2 = head(d, 15)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##################word cloud setup######################

#happiness = read.csv(file = "happiness2017.csv")

#create variable with colnames as choice this is just removing the country name
#choice = colnames(happiness)[-1]
#head(choice)

choice = colnames(medium)

#this is used for the correlation plot (plot 6)
#happiness5 = select(happiness, 'Happiness.Score', 'Economy..GDP.per.Capita.','Family', 'Health..Life.Expectancy.', 'Freedom', 'Generosity', 'Trust..Government.Corruption.', 'Dystopia.Residual')

# category and tag
# 
# combined6 = combined4 %>% group_by(Category, Tag) %>%
#   summarise(., top10)
# 
# combined6 = combined4 %>% group_by(Author, Claps, Title) %>%
#   summarise(., Claps)

combined10 = combined %>%
  group_by(Category, Date) %>%
  summarise(., countcat = n())
head(combined10)



##################################################### UI.R #########################################


library(shinydashboard)

ui <- fluidPage(shinyUI(
  dashboardPage(
    dashboardHeader(title = 'Medium'),
    dashboardSidebar(
      sidebarUserPanel(
        name = 'By Eric Adlard',
        subtitle = '',
        image = 'MEDIUM.png'
      ),
      sidebarMenu(
        menuItem("Data", tabName = "data", icon = icon("database")),
        menuItem("Graphs", tabName = "graphs", icon = icon("bar-chart")),
        menuItem("Traffic", tabName = "traffic", icon = icon("bar-chart")),
        menuItem("Wordcloud", tabName = "wordcloud", icon = icon("bar-chart")),
        menuItem("About", tabName = "about", icon = icon("info"))
      )
      # selectizeInput("selected",
      #                "Select Item to Display",
      #                choice)
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "data",
                # datatable
                fluidRow(box(DT::dataTableOutput("table"), width = 12))),
        tabItem(tabName = "about",
                fluidRow(box(title = "Webscraping of Medium Project",
                             footer = "For my project I decided to scrape data from Medium.com. Currenty they are 
                             one of my favorite reading sources and I usually find the material quite interesting. 
                             In there own words, Medium taps into the brains of the world's most insightful writers, 
                             thinkers, and storytellers to bring you the smartest takes on topics that matter. So 
                             whatever your interest, you can always find fresh thinking and unique perspectives."
                             , width=12)),
                fluidRow(box(title = "What data can I get from Medium?",
                             footer = "I pulled 1026 articles from Medium's six main reading categories (Tech, Startups,
                             Self, Politics, Health, Design). I used selenium infinite scroll as it was required to pull
                             the articles from the website and told it scroll down 7 times for each category. This left 
                             me with article totals of (Tech: 183, Startups: 188, Self: 204, Politics 140, Health: 118, Design 193). 
                             Within these articles I decided to pull article title, author, date, length of article, 
                             claps(thumbs up) and tags. From there I wanted to see what correlations and insight I 
                             could glean from these categories.", width=12))),
        tabItem(tabName = "graphs",
                fluidRow(box(title = "What were some initial findings from the data?",
                             footer = "Article length is important and is consistent across all reading categories. Authors work to optimize their posts to get there point across in a concise manner and it shows in post length. The next point is that article clap averages are fairly low with a lot of content not generating much interest and the majority of interest driven by the top articles."
                             , width=12)),
                fluidRow(column(6, plotOutput("plot7")),
                         column(6, plotOutput("plot8"))
                ),
                fluidRow(column(6, plotOutput("plot9")),
                         column(6, plotOutput("plot10"))
                )
        ),
        tabItem(tabName = "traffic",
                fluidRow(box(title = "Where is the most highly regarded content coming from?",
                             footer = "For websites it is extremely important to understand what type of content there users like and grow that content over time. For Medium I tracked where the claps(thumbs up) were coming from and tagged them to six different article categorization types (Tech, Startups, Self, Politics, Health, Design). What is interesting in this graphic is the diversity of what users like and also the variability of claps among the categorization types."
                             , width=12)),
                fluidRow(column(12, plotOutput("plot13"))
                ),
                fluidRow(column(12, plotOutput("plot14"))
                )
        ),
        tabItem(tabName = "wordcloud",
                fluidRow(box(title = "What are the most commonly tagged words?",
                             footer = "Within Medium you are able to tag keywords to your article to help identify its content. I aggregated all of the keywords used across the articles in my dataset. The graphic on the bottom left gives a count of the top 10 most used keywords in the dataset. On the bottom right you have a word cloud of the most used tags in all articles."
                             , width=12)),
                fluidRow(column(6, plotOutput("plot11")),
                         column(6, plotOutput("plot12"))),
                fluidRow(column(6, h4("Summary of claps with no tags"), verbatimTextOutput("sum1")),
                         column(6, h4("Summary of claps with tags"), verbatimTextOutput("sum2")))
        ) #closes tabItem off.
                )
                ))))


################################### SERVER.R ####################################################


server <- function(input, output, session) {
  
  
  
  #Data output on Data tab
  output$table <- DT::renderDataTable({
    datatable(medium, rownames=FALSE) %>% 
      formatStyle(input$selected,  
                  background="skyblue", fontWeight='bold')
    # Highlight selected column using formatStyle
  })
  output$plot1<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Economy..GDP.per.Capita., colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Economy by regions") + geom_point(size = 4)
  })
  output$plot2<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Health..Life.Expectancy., colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Health / Life Expectancy by regions") + geom_point(size = 4)
  })
  output$plot3<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Freedom, colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Freedom by regions") + geom_point(size = 4)
  })
  output$plot4<-renderPlot({
    ggplot(happiness, aes(x = Happiness.Score, y = Generosity, colour= Region)) + 
      ggtitle("Scatterplot of Happiness vs Generosity by regions") + geom_point(size = 4)
  })
  output$plot5<-renderPlot({
    ggplot(happiness, aes(x = Region, y = Happiness.Score, colour= Region, srt = 45)) + 
      ggtitle("Boxplot of Happiness Score by regions") + geom_boxplot() + theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  output$plot6<-renderPlot({
    corr = round(cor(happiness5), 1)
    ggcorrplot(corr, method = "circle") + ggtitle("Correlation graph among variables")
  })
  output$plot7<-renderPlot({
    ggplot(combined, aes(x = Article_length, y = Claps, colour= Category)) + 
      ggtitle("Scatterplot of article length vs number of claps") + geom_point(size = 4)
  })
  output$plot8<-renderPlot({
    ggplot(combined4, aes(x = Claps, y = Category, colour= Category)) +
      ggtitle("Scatterplot of claps by article category") + geom_point(size = 4)
  })
  output$plot9<-renderPlot({
    ggplot(combined, aes(x = Category, y = Article_length, colour= Category, srt = 45)) + 
      ggtitle("Boxplot of Article length by category") + geom_boxplot() + theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  output$plot10<-renderPlot({
    ggplot(combined, aes(x = Category, y = Claps, colour= Category, srt = 45)) + 
      ggtitle("Boxplot of claps by category") + geom_boxplot() + theme(axis.text.x = element_text(angle = -45, hjust = 0))
  })
  output$plot11<-renderPlot({
    ggplot(d2, aes(x = reorder(word, +freq), y = freq, fill = word)) + geom_bar(stat='identity') + coord_flip()
  })  
  output$plot12<-renderPlot({
    wordcloud(words = d$word, freq = d$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  output$plot13<-renderPlot({
    ggplot(combined4, aes(x = Date, y = Claps, colour= Category, fill = Category)) +
      ggtitle("Traffic of highly regarded articles") + geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = -90, hjust = 0))
  })
  output$plot14<-renderPlot({
    ggplot(combined10, aes(x = Date, y = countcat, colour= Category, fill = Category)) +
      ggtitle("Velocity of article per day by category") + geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = -90, hjust = 0))
  })
  output$sum1<-renderPrint({
    summary(combined12$Claps)
  })
  output$sum2<-renderPrint({
    summary(combined13$Claps)
  })
  #number of articles(y) by length of time(x)
}
shinyApp(ui, server)