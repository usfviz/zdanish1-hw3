library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(GGally)

# setwd('~/Data_Viz/HW3/dataset_diabetes')
# setwd('~/Data_Viz/HW3/Facebook_metrics')

df <- read.csv('dataset_Facebook.csv', sep = ';')
# monthly_agg <- aggregate(cbind(like, comment, Lifetime.Post.Consumptions) ~ Type + Post.Month, data = df, FUN = mean)
monthly_agg <- aggregate(cbind(like, comment, Lifetime.Post.Consumptions) ~ Type + Post.Weekday, data = df, FUN = mean)
type_agg <- aggregate(cbind(like,share,comment) ~ Post.Weekday + Post.Month, data = df, FUN = mean)
type_agg$Post.Weekday <- as.factor(type_agg$Post.Weekday)
type_agg$Post.Month <- as.factor(type_agg$Post.Month)

par_agg <- aggregate(cbind(like, comment, share) ~ Post.Weekday, data = df, FUN = mean)
par_agg$Post.Weekday <- as.factor(par_agg$Post.Weekday)
df$Post.Weekday <- as.factor(df$Post.Weekday)

# levels(df$Post.Weekday) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
ui <- fluidPage(
  headerPanel("Zainab's Dash"),
    mainPanel(
      tabsetPanel(
      tabPanel("Bubble Plot",plotlyOutput("P1", width = "100%"),
      sliderInput("day", "Day of the Week:", min=1, max=7,
                  step=1, value = 1, animate = animationOptions(interval=1000), sep="", width = 550)),
      tabPanel("Small Multiples",plotlyOutput("P2", width = "100%") ,
               sidebarPanel(
                 selectInput('var', 'Y Variable', c('Likes','Comments','Shares')))),
      tabPanel("Parallel Coordinates",plotlyOutput("P3", width = "100%"),
               sidebarPanel(
                 selectInput('yvar', 'Month', c('All','Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))))
    ))
)

server <- function(input, output) {
  
  final<- reactive(subset(df, Post.Weekday == input$day))
  # final_df <- final_df[is.null(final_df)==F,]
  # final_df$id <- 1:nrow(mtc)
  output$P1 <- renderPlotly({
    plt <- ggplot(final(),aes(y=like, x=comment))
    plt <- plt + geom_point(aes(fill = Type, size = Lifetime.Post.Consumptions), alpha = 0.85, shape=21) 
    plt <- plt + scale_size(guide = 'none') + xlab('Number of Comments') 
    plt <- plt + scale_x_continuous(breaks = seq(0,50,10), limits = c(0,50))+ scale_y_continuous(breaks = seq(0,2000,1000), limits = c(0,2000))
    plt <- plt + ylab('Number of Likes') + theme_bw() + theme(legend.title = element_blank())
    plt <- plt + scale_color_manual(values = c('#e7298a','#66a61e','#e6ab02','#a6761d'))
    ggplotly(plt,tooltip = c("text"))
  })
  
  output$P2 <- renderPlotly({
    y_ax <- reactive({
      if ( "Likes" %in% input$var) return(type_agg$like)
      if ( "Shares" %in% input$var) return(type_agg$share)
      if ( "Comments" %in% input$var) return(type_agg$comment)})
    plt <- ggplot(type_agg, aes(y=y_ax(), x=Post.Weekday))
    plt <- plt + geom_bar(stat = 'identity', aes(fill = Post.Weekday))
    plt <- plt + facet_wrap(~Post.Month) 
    plt <- plt + ggtitle("Small Multiples in R")
    plt <- plt + ylab('') + theme_bw() + theme(legend.title = element_blank(), axis.title.x=element_text(vjust=2.1, size = 12)) + xlab('\n \n \n Day of the Week')
    plt <- plt + theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color='#555555')) 
    # plt <- plt + theme(axis.text.x = element_text(angle=90)) 
  })
  d_sub <- reactive({
    if ( "All" %in% input$yvar) return(df)
    if ( "Jan" %in% input$yvar) return(df[df$Post.Month == 1, ])
    if ( "Feb" %in% input$yvar) return(df[df$Post.Month == 2, ])
    if ( "Mar" %in% input$yvar) return(df[df$Post.Month == 3, ])
    if ( "Apr" %in% input$yvar) return(df[df$Post.Month == 4, ])
  if ( "May" %in% input$yvar) return(df[df$Post.Month ==  5, ])
  if ( "Jun" %in% input$yvar) return(df[df$Post.Month == 6, ])
  if ( "Jul" %in% input$yvar) return(df[df$Post.Month == 7, ])
  if ( "Aug" %in% input$yvar) return(df[df$Post.Month == 8, ])
  if ( "Sep" %in% input$yvar) return(df[df$Post.Month == 9, ])
  if ( "Oct" %in% input$yvar) return(df[df$Post.Month == 10, ])
  if ( "Nov" %in% input$yvar) return(df[df$Post.Month == 11, ])
  if ( "Dec" %in% input$yvar) return(df[df$Post.Month == 12, ])})
  output$P3 <- renderPlotly({
    plt <- ggparcoord(d_sub(), columns = 16:18, groupColumn = 'Post.Weekday', scale = 'uniminmax') + theme_bw() + xlab('Interaction Type') 
    plt <- plt + theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey90"), legend.title = element_blank())
    plt <- plt + scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d'),labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    plt <- plt + theme(panel.background = element_rect(fill="white",color = "black", size = 0.5),
                        panel.grid.major = element_blank(), axis.title.y = element_blank())

    ggplotly(plt, tooltip = c('colour'))
    })
  
}


shinyApp(ui = ui, server = server)
