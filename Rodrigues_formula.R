# Download packages
if (!require("shiny")) install.packages("shiny", dependencies=TRUE)
if (!require("rgl")) install.packages("rgl", dependencies=TRUE)

library(shiny)
library(rgl)

ui <- fluidPage(
  titlePanel("Rotate 3D object"),
  sidebarLayout(
    sidebarPanel(
      p("Define vector k using polar coordinate by input theta (θ) and phi (φ)"),
      br(),
      sliderInput("theta", "Theta (θ) of vector k in polar coordinate (in degrees):", min = 0, max = 180, value = 45),
      sliderInput("phi", "Phi (φ) of vector k in polar coordinate (in degrees):", min = 0, max = 360, value = 45),
      sliderInput("alpha", "Rotate an angle alpha (α) about the axis represented by the vector k:", min = 0, max = 360, value = 0)
    ),
    mainPanel(
      rglwidgetOutput("plot3d")
    )
  )
)

server <- function(input, output, session) {
  # Set the environment for new window
  open3d(useNULL = TRUE)
  
  output$plot3d <- renderRglwidget({
    theta <- input$theta * pi / 180  # Convert theta from degrees to radians
    phi <- input$phi * pi / 180  # Convert phi from degrees to radians
    alpha <- input$alpha * pi / 180  # Convert alpha from degrees to radians
    
    # Calculate unit vector k coordinate in Oxyz.
    kx <- sin(theta) * cos(phi)
    ky <- sin(theta) * sin(phi)
    kz <- cos(theta)
    
    # Clear the previous graph
    clear3d()
    
    # Draw the axes
    axes3d(col = "black", xlim = c(-5, 5), ylim = c(-5, 5), zlim = c(-5, 5))
    segments3d(c(0, 2.5), c(0, 0), c(0, 0), col = "red", lwd = 5)
    segments3d(c(0, 0), c(0, 2.5), c(0, 0), col = "gold", lwd = 5)
    segments3d(c(0, 0), c(0, 0), c(0, 2.5), col = "blue", lwd = 5)
    
    text3d(x = c(2.8, 0, 0), y = c(0, 2.8, 0), z = c(0, 0, 2.8), texts = c("X", "Y", "Z"), 
           adj = c(0.5, 0.5), cex = 1.2, col = "black")
    
    # Draw unit vector k
    arrow3d(c(0, 0, 0), c(kx, ky, kz), barblen = 0.08, thickness = 1, type = "rotation", col = "green")
    text3d(x = c(kx+0.2, ky + 0.2, kz + 0.2), texts = c("K"), adj = c(0.5, 0.5), cex = 1.2, col = "black")
    
    # Create matrix represent an object having shape of an arrow 
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
    
    # Calculate K matrix
    K <- matrix(
      c(
        0, -kz, ky,
        kz, 0, -kx,
        -ky, kx, 0)
      , nrow = 3,
      ncol =3,
      byrow = TRUE
    )
    
    # Declare the identity matrix
    I <- diag(3)
    
    # Calculate R matrix
    R <- I + sin(alpha)*K + (1-cos(alpha))*K%*%K
    
    # Rotate arrow object using R matrix
    arrow_object <- R%*%arrow_object
    
    # Print R and K matrix on console
    print("Matrix K: ")
    print(K)
    print("Matrix R: ")
    print(R)
    
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