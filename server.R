
source("startup.R")

function(input, output, session) {
  
  
  ##### Reactively Read in Uploaded Dataset #####
  mydata <- reactive({
    inFile <- input$dat
    
    if(is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  })
  
  ##### Reactively Analyze User's Input on "Analyze" Click #####
  calibrated_output <- observeEvent(input$analyzeClick, {
    
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
    
    
    ##### Call corrected_meta #####
    res_corrected_meta <- reactive({
      withProgress(message="Calculating corrected estimate...", value=1,{
        
        withCallingHandlers({
          shinyjs::html("corrected_meta_messages", "")
          
          
          ##### Check for required input
          # needs to stay within the withCallingHandlers
          # when the textInput boxes are empty, they default to ""
          if ( is.null(dat) |
               yi.name == "" |
               vi.name == "" | 
               is.na(eta) ) {
            stop( paste("To calculate this metric, must provide at minimum: the dataset, the variable names of estimates and their variances, and the hypothetical publication bias severity (", '\u03b7', ")", sep = "") )
          }
          
          if ( !yi.name %in% names(dat) ) stop( paste("There is no variable called ", yi.name, " in the dataset", sep = "" ) )
          
          if ( !vi.name %in% names(dat) ) stop( paste("There is no variable called ", vi.name, " in the dataset", sep = "" ) )
          
          if ( clustervar.name == "" ) {
            warning("These analyses assume the estimates are not clustered because you did not provide a name for a cluster variable.")
            cluster = 1:nrow(dat)
          } else {
            
            if ( !clustervar.name %in% names(dat) ) stop( paste("There is no variable called ", clustervar.name, " in the dataset", sep = "" ) )
            
            cluster = dat[[clustervar.name]]
          }
          
          #@haven't tested with clusters
          
          corrected_meta( yi = dat[[yi.name]],
                          vi = dat[[vi.name]],
                          eta = eta,
                          model = model,
                          clustervar = cluster,
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
    }) ## closes res_corrected_meta output
    
    
    
    ##### Call svalue #####
    res_svalue <- reactive({
      withProgress(message="Calculating bias required to make shift...", value=1,{
        
        withCallingHandlers({
          shinyjs::html("svalue_messages", "")
          
          ##### Check for required input
          # needs to stay within the withCallingHandlers
          # when the textInput boxes are empty, they default to ""
          if ( is.null(dat) |
               yi.name == "" |
               vi.name == "" | 
               is.na(q) ) {
            stop( "To calculate this metric, must provide at minimum: the dataset, the variable names of estimates and their variances, and the threshold (q)" )
          }
          
          if ( clustervar.name == "" ) {
            warning("These analyses assume the estimates are not clustered because you did not provide a name for a cluster variable.")
            cluster = 1:nrow(dat)
          } else {
            
            if ( !clustervar.name %in% names(dat) ) stop( paste("There is no variable called ", clustervar.name, " in the dataset", sep = "" ) )
            
            cluster = dat[[clustervar.name]]
          }
          
          svalue( yi = dat[[yi.name]],
                  vi = dat[[vi.name]],
                  q = q,
                  clustervar = cluster,
                  model = model,
                  favor.positive = favor.positive,
                  alpha.select = alpha.select,
                  return.worst.meta = TRUE )
          
        },
        message = function(m){
          shinyjs::html(id="svalue_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="svalue_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )
        
      }) ## closes withProgress
    }) ## closes calibrated_cm output
    
    
    
    ##### Organize corrected_meta output into string #####
    output$num.results.cm = renderText({
      
      res <- req( res_corrected_meta() )
      
      est = round( as.numeric( res$est ), 3 )
      lo = round( as.numeric( res$lo ), 3 )
      hi = round( as.numeric( res$hi ), 3 )
      
      string = paste( est, " (95% CI: ", lo, ", ", hi, ")", sep="" )
      return( string )
      
    }) ## closes text.cm
    
    
    ##### Organize svalue outputs into strings #####
    
    output$num.results.worst = renderText({
      
      res <- req( res_svalue() )
      
      # this is a string because could be "Not possible"
      worst = res$meta.worst
      
      # worst could be NULL if there were too few nonaffirmative studies
      if ( !is.null(worst) ) {
        # meta.worst has a different structure depending on the model chosen
        if ( model == "fixed" ) {
          return( paste( round( worst$b, 2 ),
                         " [",
                         round( worst$ci.lb, 2 ),
                         ", ",
                         round( worst$ci.ub, 2 ), 
                         "]",
                         sep = "" ) )
        } else if ( model == "robust" ) {
          return( paste( round( worst$b, 2 ),
                         " [",
                         round( worst$reg_table$CI.L, 2 ),
                         ", ",
                         round( worst$reg_table$CI.U, 2 ), 
                         "]",
                         sep = "" ) )
        }
      }
      
      if ( is.null(worst) ) {
        return("--")
      }
      
      
    }) ## closes text.sval.est
    
    output$num.results.sval.est = renderText({
      
      res <- req( res_svalue() )
      
      # this is a string because could be "Not possible"
      sval.est = res$stats$sval.est
      
      if ( sval.est == "Not possible" ){
        
        string = paste( "No amount of publication bias favoring significant ",
                        tolower(input$favored_direction),
                        " results could attenuate the point estimate to q = ",
                        input$q,
                        ". \n\nIn that sense, the meta-analysis is highly robust to publication bias.",
                        sep = "")
        
      } else {
        return( round( as.numeric(sval.est), 2) )
      }
      
    }) ## closes text.sval.est
    
    
    output$num.results.sval.ci = renderText({
      
      
      res <- req( res_svalue() )
      
      # this is a string because could be "Not possible"
      sval.ci = res$stats$sval.ci
      
      if ( sval.ci == "Not possible" ){
        
        string = paste( "No amount of publication bias favoring significant ",
                        tolower(input$favored_direction),
                        " results could attenuate the confidence interval to include q = ",
                        input$q,
                        ". \n\nIn that sense, the meta-analysis is highly robust to publication bias.",
                        sep="")
        
      } else if ( sval.ci == "--" ) {
        
        string = paste( "The uncorrected confidence interval already contains ",
                        input$q,
                        ", so this metric does not apply.",
                        sep = "" )
      } else {
        return( round( as.numeric(sval.ci), 2) )
      }
      
    }) ## closes text.sval.ci
    
    
    
  }) ## closes calibrated_output
  
  calibrated_plot <- observeEvent(input$plotClick, {
    
    # if using ggplotly, change to "renderPlotly"
    output$calibrated_plot1 = renderPlot({
      withProgress(message="Generating plot...", value=1,{
        # ### isolate on parameters to not update until action button pressed again
        # scale = isolate(input$calibrated_scale)
        # r = isolate(input$calibrated_r)
        # tail = isolate(input$calibrated_tail)
        # yi.name = isolate(input$calibrated_yi.name)
        # vi.name = isolate(input$calibrated_vi.name)
        # method = isolate(input$calibrated_method)
        # R = isolate(input$calibrated_R)
        # dat = isolate(mydata())
        
        
        ### isolate on parameters to not update until action button pressed again
        dat = isolate(mydata())
        yi.name = isolate(input$yi_name)
        vi.name = isolate(input$vi_name)
        clustervar.name = isolate(input$clustervar_name)
        q = isolate(input$q)
        model = isolate(input$model)
        favored.direction = isolate(input$favored_direction)
        alpha.select = isolate(input$alpha_select)
        etaMin = isolate(input$etaMin)
        etaMax = isolate(input$etaMax)
        
        # process the input
        if ( favored.direction == "Positive" ) favor.positive = TRUE
        if ( favored.direction == "Negative" ) favor.positive = FALSE
        
        if ( model == "Robust random-effects" ) model = "robust"
        if ( model == "Fixed-effect" ) model = "fixed"
        
        
        withCallingHandlers({
          shinyjs::html("calibrated_sens_plot_messages", "")
          
          
          ##### Check for Required Input #####
          # when the textInput boxes are empty, they default to ""
          if ( is.null(dat) |
               yi.name == "" |
               vi.name == "" ) {
            stop( paste("To make the plot, must provide at minimum: the dataset, the variable names of estimates and their variances", sep = "") )
          }
          
          if ( !yi.name %in% names(dat) ) stop( paste("There is no variable called ", yi.name, " in the dataset", sep = "" ) )
          
          if ( !vi.name %in% names(dat) ) stop( paste("There is no variable called ", vi.name, " in the dataset", sep = "" ) )
          
          if ( clustervar.name == "" ) {
            warning("The estimates and confidence interval in the plot assume the studies' estimates are not clustered because you did not provide a name for a cluster variable.")
            cluster = 1:nrow(dat)
          } else {
            
            if ( !clustervar.name %in% names(dat) ) stop( paste("There is no variable called ", clustervar.name, " in the dataset", sep = "" ) )
            
            cluster = dat[[clustervar.name]]
          }
          
          
          if ( etaMax < etaMin ) stop("Lower limit must be less than upper limit")
          el = as.list( seq( etaMin, etaMax, .5 ) )
          
          
          # make the plot
          # get estimates at each value
          res.list = lapply( el, 
                             function(x) {
                               
                               cat("\n Working on eta = ", x)
                               
                               return( corrected_meta( yi = dat[[yi.name]], 
                                                       vi = dat[[vi.name]],
                                                       eta = x,
                                                       clustervar = cluster,
                                                       model = model,
                                                       favor.positive = favor.positive,
                                                       alpha.select = alpha.select ) )
                             } )
          
          
          re.rob.ests = as.data.frame( do.call( "rbind", res.list ) )
          
          # save results because lapply above is slow
          # because each column is secretly a list, impeding write.csv
          re.rob.ests = as.data.frame( apply(re.rob.ests, 2, unlist) )
          
          ##### Make Plot #####
          
          # # simplify breaks a little compared to eta
          # breaks = c(200, 150, 100, 50, 40, 30, 20, 10, 5)
          # axis.font.size = 16
          # axis.label.size = 20
          # 
          # if (short.name == "Li") ylabel = "Corrected estimate (HR)"
          # if (short.name == "Ali") ylabel = "Corrected estimate (BMD % change)"
          
          # if removing ggplotly, need to also change renderPlotly above to renderPlot
          ggplot( ) +
            
            # null
            geom_hline( yintercept = 0, color = "black", lty = 2 ) +
            
            geom_ribbon( data = re.rob.ests, aes( x = eta, ymin = lo, ymax = hi ), fill = "black", alpha = .1) +
            
            geom_line( data = re.rob.ests, aes( x = eta, y = est ), color = "black", lwd = 1.2) +
            
            # having eta in the bquote breaks ggplotly but is fine with regular ggplot:
            xlab( bquote( "Severity of hypothetical publication bias" ~ (eta) ) ) +
            ylab( "Corrected estimate" ) + 
            
            
            theme_classic()
          
          # theme(axis.title = element_text(size=axis.label.size),
          #       axis.text = element_text(size=axis.font.size) ) 
          
          
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
  
  
  
  ##### Reactive Interpretation Strings #####
  # wait until Analyze button is clicked, then update the text string
  # this is to avoid updating the string while the previous numerical results are still displayed,
  #  before the new analysis has been performed
  getPipedInterpretation1 = eventReactive(input$analyzeClick, {
    # avoid piping in "NA" if eta hasn't been filled in
    etaString = ifelse( is.na(input$eta), "eta", input$eta )
    
    paste( "Corrected meta-analysis estimate (assuming that significant ",
           tolower( input$favored_direction ),
           " results are ",
           etaString,
           " times more likely to be published): ",
           sep = "" )
    
  })
  output$pipedInterpretation1 = renderText({
    getPipedInterpretation1()
  })
  
  
  getPipedInterpretation2 = eventReactive(input$analyzeClick, {
    # avoid piping in "NA" if eta hasn't been filled in
    qString = ifelse( is.na(input$q), "q", input$q )
    
    
    paste( "Factor by which publication bias would need to favor significant ",
           tolower(input$favored_direction),
           " results in order to ",
           " reduce the meta-analysis estimate to ",
           qString,
           ":", 
           sep = "" )
    
  })
  output$pipedInterpretation2 = renderText({
    getPipedInterpretation2()
  })
  
  getPipedInterpretation3 = eventReactive(input$analyzeClick, {
    # avoid piping in "NA" if eta hasn't been filled in
    qString = ifelse( is.na(input$q), "q", input$q )
    
    paste( "Factor by which publication bias would need to favor significant ",
           tolower(input$favored_direction),
           " results in order to ",
           " shift the meta-analysis confidence interval to include ",
           qString,
           ":", 
           sep = "" )
    
  })
  output$pipedInterpretation3 = renderText({
    getPipedInterpretation3()
  })
  
  
  getPipedInterpretation4 = eventReactive(input$analyzeClick, {
    "Estimate under worst-case publication bias: "
    
  })
  output$pipedInterpretation4 = renderText({
    getPipedInterpretation4()
  })
  
  
} ## closes server.R function