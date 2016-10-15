

###  LOAD THE DATA

source( "https://raw.githubusercontent.com/lecy/us-pop-gradient/master/Data/pop_gradient.R" )
source( "https://raw.githubusercontent.com/lecy/us-pop-gradient/master/Data/create_dmat_w_location_data.R" )



###  SHINY APP

# library( shiny )



## USER INTERFACE 

my.ui <- fluidPage(

  # Application title
  titlePanel("US Population Gradients"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("dist",
                  "Distance From Destination City:",
                  min = 0,
                  max = 2900,
                  step= 10,
                  value = 500),
                  
      selectInput("cities", 
                  label = h3("Select City"), 
                  choices = list( "Chicago" = "17031",
                                  "New York" = "36061",
                                  "Los Angeles" = "06037", 
                                  "Houston" = "48167",
                                  "Rapid City" = "46103"), 
                                  selected = "17031")            
    ),




    mainPanel(
        
        plotOutput("distPlot"),
        
        plotOutput("gradients")
    
    )

  )
)





## SERVER

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  
my.server <- function(input, output) 
{


  
  output$distPlot <- renderPlot({
  
      this.name <- hash.table$name[ hash.table$fips == input$cities ]
      
      mapReach( county.i=input$cities, distance.x=input$dist, map.title=this.name )

  })
  

  output$gradients <- renderPlot({
  

      
      
	plot( pop.gradient$dist, pop.gradient$chicago, type="l", bty="n", col="gray", ylim=c(0,1.1),
	      xlab="Distance from City (miles)", ylab="Percentage of Population Reached")      
	lines( pop.gradient$dist, pop.gradient$los.angeles, type="l", col="gray" )
	lines( pop.gradient$dist, pop.gradient$new.york, type="l", col="gray" )
	lines( pop.gradient$dist, pop.gradient$houston, type="l", col="gray" )
	lines( pop.gradient$dist, pop.gradient$seattle, type="l", col="gray" )
	lines( pop.gradient$dist, pop.gradient$rapid.city, type="l", col="gray" )
	lines( pop.gradient$dist, pop.gradient$maine, type="l", col="gray" )
	
	this.one <- hash.table$city[ hash.table$fips == input$cities ]
	
	this.city <- pop.gradient[ , as.character(this.one) ]
	
	lines( pop.gradient$dist, this.city, type="l", lwd=2, col="red" )
	
	this.point <- this.city[ which( pop.gradient$dist == input$dist ) ]
	
	points( input$dist, this.point, pch=19, col="red", cex=2 )
	
	as.percentage <- paste( round(100*this.point,0), "%", sep="" )
	
	text( input$dist, this.point, as.percentage, pos=3, col="red", cex=1.3, offset=1 )
	
	this.name <- hash.table$name[ hash.table$fips == input$cities ]
	
	text( 50, 0.9, this.name, col="red", cex=1.8, pos=4 )

  })




  
}




# LAUNCH THE APP !

shinyApp( ui = my.ui, server = my.server )
