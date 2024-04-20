# Download packages
if (!require("shiny")) install.packages("shiny", dependencies=TRUE)
if (!require("rgl")) install.packages("rgl", dependencies=TRUE)

library(shiny)
library(rgl)

ui <- fluidPage(
  titlePanel("3D Rotation Visualizer"),
  sidebarLayout(
    sidebarPanel(
      h3("Controls"),
      br(),
      tabsetPanel(
        tabPanel("Rotation",
                 sliderInput("phi", "Phi (X-axis):", min = 0, max = 360, value = 0, step = 5),
                 sliderInput("theta", "Theta (Y-axis):", min = 0, max = 360, value = 0, step = 5),
                 sliderInput("alpha", "Alpha (Z-axis):", min = 0, max = 360, value = 0, step = 5)
        ),
      ),
      br(),
      p("Use the sliders to adjust the angles of rotation around each axis.")
    ),
    mainPanel(
      rglwidgetOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  options(rgl.useNULL = TRUE)
  
  # Function to create a rotation matrix about the X-axis
  rotate_x <- function(phi) {
    phi <- phi * pi / 180  # Convert degrees to radians
    matrix(c(1, 0, 0,
             0, cos(phi), -sin(phi),
             0, sin(phi), cos(phi)), nrow=3, byrow=TRUE)
  }
  
  # Function to create a rotation matrix about the Y-axis
  rotate_y <- function(theta) {
    theta <- theta * pi / 180  # Convert degrees to radians
    matrix(c(cos(theta), 0, sin(theta),
             0, 1, 0,
             -sin(theta), 0, cos(theta)), nrow=3, byrow=TRUE)
  }
  
  # Function to create a rotation matrix about the Z-axis
  rotate_z <- function(alpha) {
    alpha <- alpha * pi / 180  # Convert degrees to radians
    matrix(c(cos(alpha), -sin(alpha), 0,
             sin(alpha), cos(alpha), 0,
             0, 0, 1), nrow=3, byrow=TRUE)
  }
  
  # Function to reset the view
  observeEvent(input$reset, {
    rglwidgetControl("plot", viewer = "reset")
  })
  
  output$plot <- renderRglwidget({
    open3d()
    shape <- cuboctahedron3d()
    
    # Create combined rotation matrix based on slider inputs
    Rx <- rotate_x(input$phi)
    Ry <- rotate_y(input$theta)
    Rz <- rotate_z(input$alpha)
    R_combined <- Rx %*% Ry %*% Rz
    
    clear3d()
    
    # Draw the axes
    axes3d(col = "black", xlim = c(-5, 5), ylim = c(-5, 5), zlim = c(-5, 5))
    segments3d(c(0, 2), c(0, 0), c(0, 0), col = "red", lwd = 5)
    segments3d(c(0, 0), c(0, 2), c(0, 0), col = "gold", lwd = 5)
    segments3d(c(0, 0), c(0, 0), c(0, 2), col = "blue", lwd = 5)
    
    text3d(x = c(2.8, 0, 0), y = c(0, 2.8, 0), z = c(0, 0, 2.8), texts = c("X", "Y", "Z"), 
           adj = c(0.5, 0.5), cex = 1.2, col = "black")
    
    arrow_object <- cbind(
      
      # The tail of the arrow
      c(-1.5, -0.5, 0),
      c(-1.5, 0.5, 0),
      
      # The body of the arrow
      c(0, -0.5, 0),
      c(0, 0.5, 0),
      
      c(0, -1, 0),
      c(0, 1, 0),
      # The head of the arrow
      c(1, 0, 0)
    )
    
    
    # Rotate arrow object using R matrix
    arrow_object <- R_combined%*%arrow_object
    
    
    # Connect points in arrow object to sketch the outline of the object
    segments3d(x = arrow_object[1, c(1, 2)], 
               y = arrow_object[2, c(1, 2)],
               z = arrow_object[3, c(1, 2)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(2, 4)], 
               y = arrow_object[2, c(2, 4)], 
               z = arrow_object[3, c(2, 4)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(4, 6)], 
               y = arrow_object[2, c(4, 6)], 
               z = arrow_object[3, c(4, 6)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(6, 7)], 
               y = arrow_object[2, c(6, 7)], 
               z = arrow_object[3, c(6, 7)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(7, 5)], 
               y = arrow_object[2, c(7, 5)], 
               z = arrow_object[3, c(7, 5)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(5, 3)], 
               y = arrow_object[2, c(5, 3)], 
               z = arrow_object[3, c(5, 3)], 
               col = "red")
    
    segments3d(x = arrow_object[1, c(3, 1)], 
               y = arrow_object[2, c(3, 1)], 
               z = arrow_object[3, c(3, 1)], 
               col = "red")
    
    # Fill in the arrow object by red triangles
    triangles3d(arrow_object[1, c(1, 3, 4)], arrow_object[2, c(1, 3, 4)], arrow_object[3, c(1, 3, 4)], col = "red")
    triangles3d(arrow_object[1, c(1, 2, 4)], arrow_object[2, c(1, 2, 4)], arrow_object[3, c(1, 2, 4)], col = "red")
    triangles3d(arrow_object[1, c(5, 6, 7)], arrow_object[2, c(5, 6, 7)], arrow_object[3, c(5, 6, 7)], col = "red")
    
    # Create widget for 3D graph
    rglwidget()
  })
}


# Create new web window using shiny 
shinyApp(ui = ui, server = server)