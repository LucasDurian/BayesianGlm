

library(shiny)
library(shinydashboard)
library(BaysianGlm)
library('Matrix')
library('lme4')
library('mlmRev')
library(lattice)
library(DAAG)



ui <- dashboardPage(
  dashboardHeader(title = "Your options"),
  dashboardSidebar(




    #input the formula 1
                   textInput("formula", "Your formula of model 1:", "use ~ age + I(age^2) + urban + livch"),


    #input the formula 2
                   textInput("formula2", "Your formula of model 2:", "use ~ age + I(age^2) + urban + livch"),



     #compare model or not
    checkboxInput('compare', 'Compare the model 1 with the model 2?', FALSE),

    #input the alpha
                   sliderInput("alpha", "Significance Level:",
                                min = 0, max = 1, value = 0.05, step= 0.01),




    #select family
                   selectInput("family.set", "family:",
                               list("gaussian" = "gaussian",
                                    "binomial" = "binomial",
                                    "Gamma" = "Gamma",
                                    "poisson"="poisson")),
                   submitButton("Update View")),

  dashboardBody(


      # Boxes need to be put in a row (or column)
      tabsetPanel(
      tabPanel("Settings",NULL,
               #input the name of dataset
               textInput("dataset", "Name of data set:", "Contraception"),
               #input the prior mean

               textInput("priormu", "Model 1: the mean of the prior distribution(comma delimited):", "NULL"),


               #input the prior variance

               textInput("priorvar", "Model 1: the variance of the prior distribution(comma delimited):", "NULL"),
               #input the prior mean2

               textInput("priormu2", "Model 2: the mean of the prior distribution(comma delimited):", "NULL"),


               #input the prior variance2

               textInput("priorvar2", "Model 2: the variance of the prior distribution(comma delimited):", "NULL")
),
      tabPanel("Model summary", verbatimTextOutput("summary")),
      tabPanel("View your data set",tableOutput('view')),
      tabPanel("Model comparison",verbatimTextOutput('anova')),
      tabPanel("Model prediction",
               #input the name of dataset
               textInput("newdata", "Name of data set:", "Contraception")
               ,verbatimTextOutput('predict'),
               checkboxInput('pred1', 'Want to predict the data by using model 1?', FALSE),
               checkboxInput('pred2', 'Want to predict the data by using model 2?', FALSE))


    )



  )


)

server <- function(input, output) {

  output$dataset<-renderText({
    input$dataset
  })


  output$formula<-renderText({
    input$formula
  })

  output$formula2<-renderText({
    input$formula2
  })

  output$view <- renderTable({
    get(input$dataset)
  })

  # Generate a summary of the dataset
  output$summary <- renderPrint({

   if(input$family.set=="binomial"){
     family.set<-binomial
   }
    if(input$family.set=="gaussian"){
      family.set<-gaussian
    }
    if(input$family.set=="Gamma"){
      family.set<-Gamma
    }
    if(input$family.set=="poisson"){
      family.set<-poisson
    }


    if(input$priorvar=="NULL" | input$priormu=="NULL"){
    Mod<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
              alpha = as.numeric(input$alpha))


    }
    else{
    Mod<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
              alpha = as.numeric(input$alpha),priormu =as.vector(as.numeric(unlist(strsplit(input$priormu,","))))
              ,priorvar =as.vector(as.numeric(unlist(strsplit(input$priorvar,",")))))

    }


    summary(Mod)




  })


  output$anova <- renderPrint({


    if(input$family.set=="binomial"){
      family.set<-binomial
    }
    if(input$family.set=="gaussian"){
      family.set<-gaussian
    }
    if(input$family.set=="Gamma"){
      family.set<-Gamma
    }
    if(input$family.set=="poisson"){
      family.set<-poisson
    }

    if(input$priorvar=="NULL" | input$priormu=="NULL"){
      Mod1<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha))

      Mod2<-Bglm(formula=as.formula(input$formula2),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha))
    }
    else{
      Mod1<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha),priormu =as.vector(as.numeric(unlist(strsplit(input$priormu,","))))
                 ,priorvar =as.vector(as.numeric(unlist(strsplit(input$priorvar,",")))))

      Mod2<-Bglm(formula=as.formula(input$formula2),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha),priormu =as.vector(as.numeric(unlist(strsplit(input$priormu2,","))))
                 ,priorvar =as.vector(as.numeric(unlist(strsplit(input$priorvar2,",")))))
    }

   if(input$compare){

      anova(Mod1,Mod2)
   }

  if(!input$compare){


      print("There are no models to compare with.")

  }

  })


  output$predict <- renderPrint({
    if(input$family.set=="binomial"){
      family.set<-binomial
    }
    if(input$family.set=="gaussian"){
      family.set<-gaussian
    }
    if(input$family.set=="Gamma"){
      family.set<-Gamma
    }
    if(input$family.set=="poisson"){
      family.set<-poisson
    }

    if(input$priorvar=="NULL" | input$priormu=="NULL"){
      Mod1<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha))

      Mod2<-Bglm(formula=as.formula(input$formula2),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha))
    }
    else{
      Mod1<-Bglm(formula=as.formula(input$formula),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha),priormu =as.vector(as.numeric(unlist(strsplit(input$priormu,","))))
                 ,priorvar =as.vector(as.numeric(unlist(strsplit(input$priorvar,",")))))

      Mod2<-Bglm(formula=as.formula(input$formula2),family =family.set,data=get(input$dataset),
                 alpha = as.numeric(input$alpha),priormu =as.vector(as.numeric(unlist(strsplit(input$priormu2,","))))
                 ,priorvar =as.vector(as.numeric(unlist(strsplit(input$priorvar2,",")))))
    }



   if(input$pred1){

   predict(Mod1,data=get(input$newdata))
   }


    if(input$pred2){

      predict(Mod2,data=get(input$newdata))
    }


  })

}



shinyApp(ui, server)

