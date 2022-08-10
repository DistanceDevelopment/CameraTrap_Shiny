library(activity)
library(shiny)
library(dplyr, warn.conflicts = FALSE)

linebreaks <- function(n){HTML(strrep(br(), n))}

gen.boot.vals <- function(rtime, reps, bandwidth){
  # Create a Progress object
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  # Set up message
  progress$set(message = "Running bootstrap", value = 0)
  
  #Create storage for values
  boot.vals <- rep(NA, reps)
  
  # Number of times we'll go through the loop
  for (i in 1:reps) {
    # Each time through the loop, add another row of data. This is
    # a stand-in for a long-running computation.
    boot.vals[i] <- unname(fitact(rtime, sample="data", reps=1, 
                                  adj=bandwidth, show = FALSE)@act[3])
    
    # Increment the progress bar, and update the detail text.
    progress$inc(1/reps, detail = paste("Calulating for rep ", i))
  }
  return(boot.vals)
}

# PAGE SETUP ~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  #Application Title
  titlePanel("Temporal Availability Calculations for Analysis of Camera Trap Data"),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$']}
                });
                </script>
                ")),
  
  tabsetPanel(type = "tabs",
              tabPanel("Introduction",
                       h4("Introduction"),
                       p("This app provides an interface to calculate the activity multiplier for camera trap distance sampling analyses in Distance for Windows. Please follow the steps below to find the activity multiplier, associated standard error and if required boostrap resamples."),
                       h4("Data Requirements"),
                       p("The data used in this shiny app should constitute independent videos of animal activity. This means that if the recorder was triggered multiple times during a single animal visit, then only one of these should be included in the dataset. Note it is possible to include all the videos in the dataset imported into Distance for Windows."), 
                       h4("App Instructions"),
                       HTML(
                         "<ol>
                           <li><b>Upload your data:</b> Navigate to the 'Data' tab and click on the 'Browse' button. Find your data file and click 'Open'. You should now see a summary of your dataset. You may also choose to view the first few rows by selecting the 'Head' radio button on the left. You may either upload a tab delimited text file (.txt) or a comma delimited file (.csv) containing your data. Your dataset should contain a column with activity times or date times, please see the options on the 'Analysis' tab for suitable formats.</li>
                           <li><b>Fit an activity model:</b> to fit the activity model navigate to the 'Analysis' tab. First, you will need to specify the column in your data set which contains the time or date-time information. The first dropdown menu will have automatically been populated with a list of the column names from your dataset, please select the appropriate column. Next you will need to specify the format of the this column, please select from the available options in the second dropdown menu. You now have the option to specify a bandwith and the number of bootstrap replicate you require. It is sensible to start with only a few as it can take some time to run. Once you have entered these values click on the 'Run Fit Activity' button. On the lower right hand corner of your page you will see a progress counter. Once it completes you will be able to view summary statistics and the fitted model. If you update any of the values you will need to press the 'Run Fit Activity' button again. Please do this before proceeing to the final 'Activity and Bootstrap Values' tab.</li>
                           <li><b>Obtain values for Distance for Windows:</b> You now need to navigate to the 'Activity and Bootstrap Values' tab. To obtain the values required for your distance sampling analysis you will need to supply the average number of hours in any 24 hour period that the cameras were operational and recording. When you fill in this value you will be supplied with the proportion of the day which this constitutes and the activity multiplier, associated standard error and bootstrap values will all be updated accordingly. You may now record the activity multiplier value and standard error for input into the global layer of your Distance project and additionally download the bootstrap resamples for import into your multipler bootstrap layer. The bootstrap resamples will be downloaded as a single column in a tab delimited .txt file. Note that if you require to import more than one column into the boostrap multiplier layer in your Distance project, for example for multiple species, then you will need to first combine these single columns in advance of importing your data.</li>
                         </ol>"
                       )
              ),
              tabPanel("Data",
                       fileInput("file",
                                 label = "Please select your data file.",
                                 multiple = FALSE,
                                 accept = c(".csv", ".txt")),
                       sidebarLayout(
                         sidebarPanel(
                           # Radio buttons to check data
                           radioButtons("display","Display: ",
                                        c(Summary = "summary", Head = "head")),
                           width = 2
                           
                         ),
                         mainPanel(
                           # Display the data
                           conditionalPanel(
                             condition = "(input.display == 'summary' || input.display == 'head')",
                             verbatimTextOutput("data")
                           ),
                           conditionalPanel(
                             condition = "input.display == 'full'",
                             dataTableOutput("data.full")
                           )
                         )
                       )
                       
              ),
              tabPanel("Analysis", 
                       p("Please specify the column in your data containing the time or date-time information and then select the appropriate format describing this column. Note that times must be in 24 hours clock format, i.e. 13:45 rather than 1:45 PM."),
                       selectInput("time.of.day", "Time of day variable name:",
                                   c("Please select data in data tab" = "none")),
                       p("The following formats refer to the values specified as follows: HH - hours, MM - minutes, SS - seconds, dd - day, mm - month, yyyy - year. Note that the number of letters refers to the maximum number of values which should be provided, it is possible to specify in shorter format. For example dd/mm/yyyy HH:MM will accept both 01/12/2020 06:45 and 1/12/20 6:45."),
                       selectInput("time.format", "Date time format",
                                   c("HH:MM" = "%H:%M",
                                     "HH:MM:SS" = "%H:%M:%S",
                                     "HH.MM.SS" = "%H.%M.%S",
                                     "dd/mm/yyyy HH:MM" = "%d/%m/%Y %H:%M",
                                     "dd/mm/yyyy HH:MM:SS" = "%d/%m/%Y %H:%M:%S",
                                     "mm/dd/yyyy HH:MM" = "%m/%d/%Y %H:%M",
                                     "mm/dd/yyyy HH:MM:SS" = "%m/%d/%Y %H:%M:%S",
                                     "yyyy/mm/dd HH:MM" = "%Y/%m/%d %H:%M",
                                     "yyyy/mm/dd HH:MM:SS" = "%Y/%m/%d %H:%M:%S",
                                     "dd-mm-yyyy HH:MM" = "%d-%m-%Y %H:%M",
                                     "dd-mm-yyyy HH:MM:SS" = "%d-%m-%Y %H:%M:%S",
                                     "yyyy-mm-dd HH:MM" = "%Y-%m-%d %H:%M",
                                     "yyyy-mm-dd HH:MM:SS" = "%Y-%m-%d %H:%M:%S",
                                     "dd.mm.yyyy HH:MM" = "%d.%m.%Y %H:%M",
                                     "dd.mm.yyyy HH:MM:SS" = "%d.%m.%Y %H:%M:%S",
                                     "dd/mm/yyyy HH.MM" = "%d/%m/%Y %H.%M")),
                       p("The bandwidth adjustment multiplier is provided to allow exploration of the effect of adjusting the internally calculated bandwidth on accuracy of activity level estimates."),
                       textInput("adj", "Bandwith:", 1),
                       p("The distribution of the triggering events times will now be smoothed, using a kernel smoother. In order to estimate uncertainty in the point estimate of activity proportion, the data will be resampled. Please provide the number of replicates and press 'Run Fit Activity'"),
                       textInput("reps", "Replicates:", 1),
                       actionButton("run", "Run Fit Activity"),
                       h4("Unscaled results"),
                       p("The results below are based on cameras which operate for the entire 24 hours in any given day. When you have finalised your analysis, please proceed to the final tab to scale these values according to hours spent recording. Note, if you update the input values above you should press the 'Run Fit Activity' button again before proceeding."),
                       verbatimTextOutput("resultsParam"),
                       verbatimTextOutput("results"),
                       plotOutput("hist")
                       ),
              tabPanel("Activity and Bootstrap Values",
                       p("We need to perform an adjustment to obtain the activity multipliers we need to include in our distance sampling analyses. We will now scale the activity values in proportion to the number of hours the camera was operating out of a 24 hour period. Please supply the average number of hours for which the camera was recording in a 24 hour interval."),
                       fluidRow(
                         column(3, "Operational camera hours out of 24 hours: "),
                         column(3,textInput("op.hours", "", value = 24, width = '50%'))
                       ),
                       br(),
                       "Proportion of day cameras were recording:",
                       textOutput("prop.op.hours", inline = TRUE),
                       linebreaks(2),
                       h4("Scaled activity rate and SE"),
                       p("The activity rate and standard error displayed below can be recorded and manually entered into the appropriate fields of the global data layer of your Distance project."),
                       "Activity rate multiplier:",
                       textOutput("activity.rate", inline = TRUE),
                       linebreaks(2),
                       "Activity rate standard error (SE):",
                       textOutput("activity.rate.se", inline = TRUE),
                       linebreaks(2),
                       #actionButton("boot.gen", "Generate Bootstrap Replicates"),
                       
                       h4("Scaled bootstrap replicates summary"),
                       p("These are the scaled values ready for import into Distance for Windows. These values can be downloaded, using the button below, to a tab delimited .txt file and imported into the appropriate column of your multiplier bootstrap values layer (default name - 'Multipliers Bootstrap') in your Distance project."),
                       verbatimTextOutput("boot.mult"),
                       downloadButton("boot_mults", "Download Bootstrap Replicates"),
                       linebreaks(2)
              )
  )
)

# SERVER FUNCTION ~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output, session){
  
  # Read in the users dataset
  actdata <- eventReactive(input$file, {
    file <- input$file
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext %in% c("csv", "txt"), "Please upload a csv or txt file"))
    if(ext == "csv"){
      actdata <- read.csv(file$datapath, header = TRUE)
    }
    if(ext == "txt"){
      actdata <- read.table(file$datapath, header = TRUE)
    }
    dnames <- names(actdata)
    names(dnames) <- dnames
    updateSelectInput(inputId = "time.of.day", 
                      choices = dnames)
    return(actdata)
  }) 
  
  # Display data summaries
  output$data <- renderPrint({
    req(file)
    req(actdata())
    switch(input$display,
           "summary" = summary(actdata()),
           "head" = head(actdata()))
  })

  # Perform a check on the bandwidth value. If the user types a . then automatically
  # put a 0 before it. This makes the numeric verification easier!
  observe({
    tmp <- input$adj
    if(substr(tmp, 1, 1) == "."){
      updateTextInput(session, "adj", 
                      value = paste("0", tmp, sep = ""))  
    }
  })

  # Save the analysis inputs so they are not updated in the output, until things are re-run!
  column.name <- eventReactive(input$run, {
    input$time.of.day
  })
  column <- eventReactive(input$run, {
    actdata()[[input$time.of.day]]
  })
  column.format <- eventReactive(input$run, {
    switch(input$time.format,
      "%H:%M" = "HH:MM",
      "%H:%M:%S" = "HH:MM:SS", 
      "%H.%M" = "HH.MM", 
      "%H.%M.%S" = "HH.MM.SS",
      "%d/%m/%Y %H:%M" = "dd/mm/yyyy HH:MM",
      "%d/%m/%Y %H:%M:%S" = "dd/mm/yyyy HH:MM:SS",
      "%m/%d/%Y %H:%M" = "mm/dd/yyyy HH:MM",
      "%m/%d/%Y %H:%M:%S" = "mm/dd/yyyy HH:MM:SS",
      "%Y/%m/%d %H:%M" = "yyyy/mm/dd HH:MM",
      "%Y/%m/%d %H:%M:%S" = "yyyy/mm/dd HH:MM:SS",
      "%d-%m-%Y %H:%M" = "dd-mm-yyyy HH:MM",
      "%d-%m-%Y %H:%M:%S" = "dd-mm-yyyy HH:MM:SS",
      "%Y-%m-%d %H:%M" = "yyyy-mm-dd HH:MM",
      "%Y-%m-%d %H:%M:%S" = "yyyy-mm-dd HH:MM:SS",
      "%d.%m.%Y %H:%M" = "dd.mm.yyyy HH:MM",
      "%d.%m.%Y %H:%M:%S" = "dd.mm.yyyy HH:MM:SS",
      "%d/%m/%Y %H.%M" = "dd/mm/yyyy HH.MM")
  })
  bandwidth <- eventReactive(input$run, {
    input$adj
  })
  reps <- eventReactive(input$run, {
    input$reps
  })
  
  # Run the analysis and save results
  rtime <- eventReactive(input$run, {
    check <- class(column()) == "character"
    req(check)
    tmp <- try(gettime(column(), input$time.format), silent = TRUE)
    if(class(tmp) == "try-error"){
      return(NULL)
    }else if(all(is.na(tmp))){
      validate(need(!all(is.na(tmp)), "Please check your date-time column selection and selected format. There has been a problem in the calculations."))
      return(NULL)
    }else{
      return(tmp)
    }
  })
  act_result <- eventReactive(input$run, {
    check <- class(column()) == "character"
    req(check)
    req(rtime())
    fitact(rtime(), sample="data", reps=1, adj=as.numeric(bandwidth()), show = FALSE)
  })
  # Generate the bootstrap replicates
  boot.mults <- eventReactive(input$run, {
    check <- class(column()) == "character"
    req(check)
    req(rtime())
    gen.boot.vals(rtime = rtime(), reps = as.numeric(reps()), bandwidth = as.numeric(bandwidth()))
  })
  # Calculate the se as the sd of the replicates
  act_se <- eventReactive(input$run, {
    check <- class(column()) == "character"
    req(check)
    req(rtime())
    sd(boot.mults())
  })
  
  # Display activity analysis input 
  output$resultsParam <- renderPrint({
    if (input$run == 0){
      cat("") 
    }else{
      cat("The following results and plot are based on:", fill = TRUE)
      cat("Date time variable: ", column.name(), fill = TRUE)
      cat("   ", paste(column()[1:5], collapse = ", "), " ...", fill = TRUE)
      cat("Date time format:", column.format(), fill = TRUE)
      cat("Bandwith: ", bandwidth(), fill = TRUE)
      cat("Reps: ", reps(), "\n", fill = TRUE)  
    } 
  })
  
  # Display activity results
  output$results <- renderPrint({
    validate(need(is.character(actdata()[[input$time.of.day]]), "The time-date column must be of type character."))
    validate(need(as.character(as.numeric(input$adj)) == input$adj, "Please input a numeric bandwith."))
    validate(need(as.character(as.numeric(input$reps)) == input$reps, "Please input a numeric value for reps."))
    validate(need(input$run, "Not yet run."))
    req(rtime())
    point.result <- act_result()@act
    boot.results <- boot.mults() 
    point.result[2] <- act_se()
    point.result[3] <- quantile(boot.results, prob = 0.025)
    point.result[4] <- quantile(boot.results, prob = 0.975)
    print(as.data.frame(t(point.result)))
  })
  
  # Plot activity results
  output$hist <- renderPlot({
    req(rtime())
    plot(act_result(), cline = list(col = NULL))
  })
  
  # Calculate scaling value - proportion of day
  prop.camera <- reactive(as.numeric(input$op.hours)/24)

  # Output scaling value
  output$prop.op.hours <- renderPrint({
    cat(prop.camera())
  })
  
  # Display scaled activity multiplier
  output$activity.rate <- renderPrint({
    req(rtime())
    act.rate <- act_result()@act[1]/prop.camera()
    cat(act.rate)
  })
  
  # Display scaled activity multiplier standard error
  output$activity.rate.se <- renderPrint({
    req(rtime())
    act.rate.se <- act_se()/prop.camera()
    cat(act.rate.se)
  })
  
  # Display summary of multiplier boostrap replicates
  output$boot.mult <- renderPrint({
    req(rtime())
    boot.vals <- boot.mults()/prop.camera()
    cat("The following results and plot are based on:", fill = TRUE)
    cat("Date time variable: ", column.name(), fill = TRUE)
    cat("   ", paste(column()[1:5], collapse = ", "), " ...", fill = TRUE)
    cat("Date time format:", column.format(), fill = TRUE)
    cat("Bandwith: ", bandwidth(), fill = TRUE)
    cat("Reps: ", reps(), "\n", fill = TRUE)
    print(summary(boot.vals))  
  })
  
  # Divide the bootstrap rep values divided by prop.camera
  download.data <- reactive({
    req(rtime())
    data.frame(boot.reps = boot.mults()/prop.camera())
  })
  
  # Downloadable .txt file of selected dataset
  output$boot_mults <- downloadHandler(
    filename = "BootMultValues.txt",
    content = function(file) {
      eol <- ifelse(.Platform$OS.type == "windows", "\n", "\r\n")
      write.table(download.data(), file, row.names = FALSE, sep = "\t", 
                  eol = eol)
    }
  )
}

shinyApp(ui = ui, server = server)


