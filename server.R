
source("startup.R")

function(input, output, session) {
  

  # read in uploaded dataset
  mydata <- reactive({
    inFile <- input$dat
    
    if(is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  })
  
  # # set defaults for plot Bmin and Bmax depending on chosen scale
  # observeEvent(input$calibrated_scale, {
  #   scale_Bmin_default <- ifelse(input$calibrated_scale=="Log-RR", 0, 1)
  #   scale_Bmax_default <- ifelse(input$calibrated_scale=="Log-RR", log(4), 4)
  #   updateNumericInput(session, "calibrated_Bmin", value=scale_Bmin_default)
  #   updateNumericInput(session, "calibrated_Bmax", value=scale_Bmax_default)
  # })
  
  # collect user's input every time the "Analyze" button is pressed
  calibrated_output <- observeEvent(input$analyze_button, {
    ### isolate on parameters to not update until action button pressed again
    dat = isolate(mydata())
    yi.name = isolate(input$yi_name)
    vi.name = isolate(input$vi_name)
    clustervar.name = isolate(input$clustervar_name)
    q = isolate(input$q)
    model = isolate(input$model)
    favored.direction = isolate(input$favored_direction)
    eta = isolate(input$eta)
    return.worst.meta = isolate(input$return_worst_meta)
    alpha.select = isolate(input$alpha_select)

    # process the input
    if ( favored.direction == "Positive" ) favor.positive = TRUE
    if ( favored.direction == "Negative" ) favor.positive = FALSE
    
    if ( model == "Robust random-effects" ) model = "robust"
    if ( model == "Fixed-effect" ) model = "fixed"
    
    res_corrected_meta <- reactive({
      withProgress(message="Calculating...", value=1,{
        
        withCallingHandlers({
          shinyjs::html("corrected_meta_messages", "")
          
          corrected_meta( yi = dat[[yi.name]],
                          vi = dat[[vi.name]],
                          eta = eta,
                          model = model,
                          selection.tails = 1,  # @could make this a user-specified option
                          favor.positive = favor.positive,
                          alpha.select = alpha.select)
          
        },
        message = function(m){
          shinyjs::html(id="corrected_meta_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="corrected_meta_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )
        
      }) ## closes withProgress
    }) ## closes calibrated_cm output
    
    # text from corrected_meta
    output$text1 = renderText({
      
      res <- req( res_corrected_meta() )
      
      est = round( as.numeric( res$est ), 3 )
      lo = round( as.numeric( res$lo ), 3 )
      hi = round( as.numeric( res$hi ), 3 )
      
      
      ##### Create String for UI #####
      string = paste( est, " (95% CI: ", lo, ", ", hi, ")", sep="" )
      return( string )
      
    }) ## closes calibrated_text1
    
    #bm
    
    # output$calibrated_text2 = renderText({
    #   cm <- req(calibrated_cm())
    #   
    #   p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
    #   Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
    #   Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
    #   Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )
    #   
    #   
    #   ##### Create String for UI #####
    #   string_Tmin = ifelse(p < r, "The proportion of meaningfully strong effects is already less than or equal to r even with no confounding, so this metric does not apply. No confounding at all is required to make the specified shift.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
    #   string_Tmin = ifelse(is.na(string_Tmin), "Cannot compute second two metrics without r. Returning only the proportion.", string_Tmin)
    #   return( string_Tmin )
    #   
    # }) ## closes calibrated_text2
    # 
    # output$calibrated_text3 = renderText({
    #   cm <- req(calibrated_cm())
    #   
    #   p = round( as.numeric(cm$Est[ which(cm$Value=="Prop") ]), 3 )
    #   Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Gmin") ]), 3 )
    #   Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Gmin") ]), 3 )
    #   Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Gmin") ]), 3 )
    #   
    #   
    #   ##### Create String for UI #####
    #   #bm
    #   string_Gmin = ifelse(p < r, "The proportion of meaningfully strong effects is already less than or equal to r even with no confounding, so this metric does not apply. No confounding at all is required to make the specified shift.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
    #   string_Gmin = ifelse(is.na(string_Gmin), "Cannot compute the second two metrics without r. Returning only the proportion.", string_Gmin)
    #   return( string_Gmin )
    #   
    # }) ## closes calibrated_text3
    
    
  }) ## closes calibrated_output
  
  calibrated_plot <- observeEvent(input$calibrated_plot, {
    output$calibrated_plot1 = renderPlot({
      withProgress(message="Generating plot...", value=1,{
        ### isolate on parameters to not update until action button pressed again
        scale = isolate(input$calibrated_scale)
        r = isolate(input$calibrated_r)
        tail = isolate(input$calibrated_tail)
        yi.name = isolate(input$calibrated_yi.name)
        vi.name = isolate(input$calibrated_vi.name)
        method = isolate(input$calibrated_method)
        R = isolate(input$calibrated_R)
        dat = isolate(mydata())
        
        if(scale=="RR"){
          q = isolate(log(input$calibrated_q))
          muB = isolate(log(input$calibrated_muB))
          Bmin = isolate(log(input$calibrated_Bmin))
          Bmax = isolate(log(input$calibrated_Bmax))
          
        } else {
          if(scale=="Log-RR"){
            q = isolate(input$calibrated_q)
            muB = isolate(input$calibrated_muB)
            Bmin = isolate(input$calibrated_Bmin)
            Bmax = isolate(input$calibrated_Bmax)
          }
        }
        
        withCallingHandlers({
          shinyjs::html("calibrated_sens_plot_messages", "")
          sens_plot(method=method, type="line", q=q, yi.name=yi.name, vi.name=vi.name, Bmin=Bmin, Bmax=Bmax, tail=tail, give.CI=TRUE, R=R, dat=dat )
        },
        message = function(m){
          shinyjs::html(id="calibrated_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="calibrated_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )
        
      }) ## closes withProgress
      
      ### output plot warnings:
      ## jl: warnings/messages should now be built into the plot outputs using withCallingHandlers to pull messages/warnings from sens_plot itself
      
    }) ## closes calibrated_plot1
    
    
  }) ## closes calibrated_plot
  
  
  ### results text for calibrated Fixed sensitivity parameters tab
  output$results1 = renderText({
    paste( "Corrected meta-analysis estimate (assuming that significant ",
           tolower( input$favored_direction ),
           " results are ",
           input$eta,
           " times more likely to be published): ",
           sep = "" )
  })
  
  # output$calibrated_results_minbias = renderText({
  #   paste("Minimum bias factor (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_scale, "=", input$calibrated_q, ":")
  # })
  # output$calibrated_results_minconf = renderText({
  #   paste("Minimum confounding strength (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_scale, "=", input$calibrated_q, ":")
  # })
  

  
} ## closes server.R function