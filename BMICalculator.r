library(shiny)

ui<-fluidPage(
  titlePanel("BMI Calculator: Calculate your BMI and get inferences"),
  sidebarLayout(
    sidebarPanel(
      numericInput("mass_kg", label = strong("Input your weight (in Kg)"),min = 30, max = 250,0), 
      br(),
      numericInput("height_cm", label = strong("Input your height (in cm)"),min = 90, max = 220,0),
      br(),
      actionButton("YourBMI", label = "Calculate BMI!")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Your BMI",
                 p(h3("Your details:")),
                 textOutput("current_weight"),
                 textOutput("current_height"),
                 br(),wellPanel(
                 p(h4("Your BMI is:")),
                 textOutput("BMI_result"),
                 p(h4("Inference from your BMI:")),
                 textOutput("indicator_show"))),
        tabPanel("About BMI",
                 p(h4("Why are BMI measurements important?")),
                 br(),
                 helpText("BMI is used for risk assessment for the general population. Generally speaking, as a person's BMI increases, so does his or her risk of certain diseases. However, BMI does not distinguish between body fat and lean body mass and does not take into account location of body fat. For this reason, it's not an accurate measure of health for certain populations, such as people with higher than average muscle mass or people whose body composition may be skewed for other reasons."),
                 p(h4("What is the classification proposed by the World Health Organization (WHO)?")),
                 helpText(" BMI <18.5 :  Underweight;  BMI [18.5-24.9] :  Normal weight;  BMI [25-29.9] :  Overweight;  BMI >=30 :  Obesity.")),
        tabPanel("BMI Risks",p(h4("What are risks of high BMI?")),
                 HTML('<ol>
                   <li>diabetes</li>
                   <li>cardiovascular disease</li>
                   <li>stroke</li>
                   <li>hypertension</li>
                   <li>gallbladder disease</li>
                   <li>osteoarthritis</li>
                   <li>sleep apnea</li>
                   <li>some cancers</li>
                   </ol>
                 '),
                 
      )))
))
  
  
  
  
server <- function(input,output){
  values <- reactiveValues()
  observe({
    input$YourBMI
    values$bmi <- isolate({
      input$mass_kg/(input$height_cm/100 * input$height_cm/100)
    })
  })
  output$current_weight <- renderText({
    input$YourBMI
    paste("Your Weight(in Kgs): ", isolate(input$mass_kg))
  })
  output$current_height <- renderText({
    input$YourBMI
    paste("Your Height(in cm) :", isolate(input$height_cm))
  })
  output$indicator_show <- renderText({
    if(input$YourBMI == 0) "" else {
      if (values$bmi < 18.5){
        values$indicator_show = "Underweight"
      }
      else if (values$bmi < 24.9){
        values$indicator_show ="Normal weight"
      }
      else if (values$bmi < 29.9){
        values$indicator_show ="Overweight"
      }
      else{
        values$indicator_show ="Obesity"
      }
      paste("", values$indicator_show)
    }
  })
  output$BMI_result <- renderText({
    if(input$YourBMI == 0) "" else
      paste("", values$bmi)
  })
}
shinyApp(ui=ui,server=server)