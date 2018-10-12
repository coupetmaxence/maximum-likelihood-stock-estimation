
#######################################
#                                     #
#             Librairies              #
#                                     #
#######################################

library(shiny)
library(shinymaterial)
library(plotly)
library(rjson)




#######################################
#                                     #
#  Load company names from JSON file  #
#                                     #
#######################################

json_data <- rjson::fromJSON(file="companies.json")
labels <- c()
values <- c()
for(dat in json_data)
{
  labels <- c(labels, dat$label)
  values <- c(values, dat$value)
}
names(values) = labels



#######################################
#                                     #
#             Shiny app               #
#                                     #
#######################################

ui <- material_page(
  title = "Stock return parameter estimation",
  nav_bar_color = "blue",
  tags$br(),
  material_row(
    material_column(
      width = 3,
      material_card(
        title = 'Stock informations',
        depth = 4,
        tags$br(),
        material_row(
          material_column(
            width = 12,
            material_dropdown(
              input_id = "stock",
              label = "Stock name",
              choices = values,
              selected = "MSFT"
            )
          )
        ),
        material_row(
          material_column(
            width = 12,
            material_dropdown(
              input_id = "law",
              label = "Type of law",
              choices = c("Log-normal"="log", "Normal"="norm"),
              selected = "log"
            )
          )
        )
      ),
      material_card(
        title = 'Estimations',
        depth = 4,
        tags$br(),
        material_row(
          material_column(
            width = 12,
            textOutput("mean")
          )
        ),
        material_row(
          material_column(
            width = 12,
            textOutput("std")
          )
        )
      )
    ),
    material_column(
      width = 9,
      material_card(
        title = "Returns",
        depth = 4,
        plotlyOutput("distPlot", height = '30em')
    )
  )
)

)

nr.mult <- function(FUN, gradiant, start, TOL = 1E-6,
                    maxiter = 100, echo = FALSE, ...)
{
  if (echo)
    expr <- expression(print(start <- start - adjust))
  else
    expr <- expression(start <- start - adjust)
  i <- 0
  repeat
  {
    adjust <- solve(gradiant(start, ...), FUN(start, ...))
    if (max(abs(adjust)) < TOL)
      break
    if (maxiter < (i <- i + 1))
      stop("Maximum number of iterations reached
           without convergence")
    eval(expr)
  }
  list(roots = start - adjust, nb.iter = i)
}

f.normal <- function(p, x)
{
  mu <- p[1]
  sigma <- p[2]
  n <- length(x)
  c(-sum((x-mu)/(sigma^2)),-n/(2*pi)+sum(((x-mu)^2)/(sigma^3)))
}

fp.normal <- function(p, x)
{
  mu <- p[1]
  sigma <- p[2]
  n <- length(x)
  cbind(c(n/(sigma^2), sum((2*(x-mu))/(sigma^3))), c(sum((2*(x-mu))/(sigma^3)), -sum((3*(x-mu)^2)/(sigma^4))))
}

get_data <- function(symbol)
{
  url <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&outputsize=full&symbol=",symbol)
  url <- paste0(url, "&apikey=KNZX")
  data <- rjson::fromJSON(file=url)
  prices <- data$"Time Series (Daily)"
  prices <- data.frame(do.call("rbind", prices))
  colnames(prices) <- c("Open", "High", "Low", "Close", "Volume")
  close_prices <- data.frame(matrix(unlist(prices$Close), byrow=T),stringsAsFactors=FALSE)
  colnames(close_prices) <- c("Close")
  
  x <- as.numeric(prices$Close[2:length(prices$Close)])/as.numeric(prices$Close[1:length(prices$Close)-1])
  return(x)
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  
   output$distPlot <- renderPlotly({
     X <- get_data("MSFT")
     X <- log(X)
     print(max(X))
     print(min(X))
     print("begin calculation")
     r <- nr.mult(f.normal, fp.normal, start = c(mean(X), var(X)), x=X)
     print("calculation finished")
     print(r$roots)
     
     mu <- r$roots[1]
     sigma <- r$roots[2]**2
     output$mean <- renderText({paste(c("Mean : ", round(mu,4)))})
     output$std <- renderText({paste(c("Std : ", round(sigma,4)))})
     print("DIsp")
     print(max(X))
     print(min(X))
     xfit<-seq(min(X),max(X),length=1000) 
     dens <- dnorm(xfit, mean=mu, sd=sigma)
     plot_ly(x = X, type = "histogram", name = "Histogram") %>% 
       add_trace(x=xfit, y=dens, type="scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
       layout(yaxis2 = list(overlaying = "y", side = "right"))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

