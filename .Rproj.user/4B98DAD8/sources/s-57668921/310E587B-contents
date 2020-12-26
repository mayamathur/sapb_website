

# Need from user: 
# - yi, vi, q, clustervar, model, alpha.select, favor.positive, CI.level

# Output:
# - corrected meta for specific eta (or worst-case from svalue function)
# - svalue
# - significance funnel


source("startup.R")

### CHANGE THIS ###
# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
amount of unmeasured confounding needed to move the estimate and confidence interval
to your specified true value rather than to the null value.'

### CHANGE THIS ###
# message to display for OLS
OLS.mess = 'Note: Using the standard deviation of the outcome yields a conservative approximation
of the standardized mean difference. For a non-conservative estimate, you could instead use the estimated residual standard deviation from your linear
regression model. Regardless, the reported E-value for the confidence interval treats the 
standard deviation as known, not estimated.'


navbarPage( "Sensitivity analysis for publication bias in meta-analyses", id = "navbar",
            
            
            theme = shinytheme("flatly"),
            
            
            tabPanel("Conduct sensitivity analysis",
                     shinyjs::useShinyjs(),
                     
                     checkboxInput( 'show_instructions_2', 'Show instructions', FALSE ),
                     
                     
                     conditionalPanel(  condition = "input.show_instructions_2 == true",
                                        HTML(paste(
                                          
                                          'This website implements the sensitivity analyses described in <a href="https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598">Mathur & VanderWeele (2020a)</a> and <a href="https://annals.org/aim/fullarticle/2643434/sensitivity-analysis-observational-research-introducing-e-value">VanderWeele & Ding (2017)</a>.
                       
                      <br><br><b>Sensitivity analysis for the proportion of meaningfully strong causal effects</b>
                       
                       <br><br>Here, you can choose a fixed set of sensitivity parameters (the mean of the bias factor distribution and the proportion of the estimated heterogeneity that is due to confounding)
                       and estimate:
                       
                       <ul>
                       <li> The proportion of meaningfully strong causal effect sizes (i.e., those stronger than a chosen threshold q)
                       <li> The minimum bias factor that would be required to "explain away" the effect in the sense of reducing to less than r the proportion of meaningfully strong effects
                       <li> The minimum confounding strength (i.e., strength of association on the relative risk scale between the unmeasured confounder(s) and the exposure, as well as between the unmeasured confounder(s) and the outcome) that would be required to "explain away" the effect
                       </ul>
                       
                       Below these three metrics appears a plot of the estimated proportion of meaningfully strong causal effects as a function of the severity of bias.,
                       
            
                       <br><br><b>Choosing robust versus parametric estimation</b>
                       
   
                       <br><br>There are two statistical methods to estimate the metrics described above. The "Robust" tab below uses a nonparametric method (<a href="https://pubmed.ncbi.nlm.nih.gov/32141922/">Mathur & VanderWeele (2020b)</a>; <a href="https://onlinelibrary.wiley.com/doi/abs/10.1002/jrsm.1345">Wang & Lee (2019)</a>) that makes no assumptions about the distribution of population effects and performs well in meta-analyses with as few as 10 studies, and performs well even when the proportion being estimated is close to 0 or 1. However, it only accommodates bias whose strength is the same in all studies (homogeneous bias). When using the robust method, you will need to upload a dataset of study-level point estimates and variance estimates because inference is conducted by bootstrapping.
                       
                       <br><br>The "Parametric" tab uses a method that assumes that the population causal effects are approximately normal across studies and that the number of studies is large. Parametric confidence intervals should only be used when the proportion estimate is between 0.15 and 0.85, and you will get a warning message otherwise. Unlike the calibrated method, the parametric method can accommodate bias that is heterogeneous across studies, specifically bias factors that are log-normal across studies. When using the parametric method, you will specify summary estimates from your confounded meta-analysis rather than uploading a dataset.


                       
                       <br><br><b>Effect size measures other than log-relative risks</b>
                       
                       <br><br>If your meta-analysis uses effect sizes other than log-relative risks, you should first approximately convert them to log-relative risks, for example using the function <code>convert_measures</code> in the R package <a href="https://cran.r-project.org/web/packages/EValue/index.html">EValue</a>. 

                       <br><br><b>When these methods should be used</b>
                       
                       <br><br>These methods perform well only in meta-analyses with at least 10 studies; we do not recommend reporting them in smaller meta-analyses. Additionally, it only makes sense to consider proportions of effects stronger than a threshold when the heterogeneity estimate is greater than 0. For meta-analyses with fewer than 10 studies or with a heterogeneity estimate of 0, you can simply report E-values for the point estimate using the tab "Sensitivity analysis for the point estimate".\n\n'
                                          
                                        ) )
                     ),
                     
                     width = 6,
                     
                     hr(),
                     
                     tabsetPanel(
                       
                       tabPanel("Robust estimation (homogeneous bias across studies)",
                                fluidRow(
                                  tags$style(type = "text/css",
                                             "label { font-size: 12px; }"
                                  ),
                                  ### hidden method input used in server.R
                                  column(width=12, shinyjs::hidden(selectInput('calibrated_method', 'Method (calibrated or parametric)', choices = c('calibrated'), selected = 'calibrated'))
                                  ),
                                  
                                  shinydashboard::box(width=4,
                                                      title= h4(strong("Upload meta-analysis dataset")),
                                                      column(width=10,
                                                             fileInput('dat', label = 'Upload meta-analysis dataset (csv)', accept=c('text/csv',
                                                                                                                                     'text/comma-separated-values,text/plain',
                                                                                                                                     '.csv')) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'A csv file containing study-level point estimates and variance estimates')),
                                                             
                                                             textInput('yi_name', "Name of variable in data containing studies' point estimates", placeholder = 'yi.name') %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = "Point estimates and their variances should be on a scale that is suitable for meta-analysis and on which 0 represents no effect (e.g., log-ratios rather than ratios, Fisher's z rather than Pearson's r, etc.)")),
                                                             
                                                             textInput('vi_name', "Name of variable in data containing studies' variance estimates", placeholder = 'vi.name') %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = "Name of variable in data containing studies' variance estimates")), 
                                                             
                                                             textInput('clustervar_name', "Name of variable in data containing cluster indicator (optional)", placeholder = 'clustervar') %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = "A string or numeric variable with one entry per row. This variable's unique values should indicate unique clusters in the point estimates (e.g., representing journal articles that each contribute multiple estimates). If left blank, the analysis assumes all point estimates are independent."))
                                                             
                                                      ),
                                                      column(width=10,
                                                             actionButton(inputId = 'calibrated_calculate', label='Analyze')
                                                      ) 
                                  ),
                                  shinydashboard::box(width=6,
                                                      title= h4(strong("Specify sensitivity parameters and thresholds")),
                                                      
                                                      
                                                      column( width=6,
                                                              
                                                              
                                                              numericInput('q', 'Threshold (q) to which to shift \nthe point estimate', 0, min = -Inf, max = Inf, step = 0.01) %>%
                                                                
                                                                shinyInput_label_embed(
                                                                  shiny_iconlink() %>%
                                                                    bs_embed_popover(title = 'The attenuated value to which to shift the point estimate and its confidence interval. Should be specified on the same scale as the point estimates (e.g., if the estimates are on the log-relative risk scale, then so should q).')),
                                                              
                                                              
                                                              selectInput('favored_direction', 'Direction of effects favored by \npublication bias', choices = c('Positive', 'Negative'), selected = 'Positive') %>%
                                                                shinyInput_label_embed(
                                                                  shiny_iconlink() %>%
                                                                    bs_embed_popover(title = '"Positive" if you would like to assume that publication bias favors significant positive-signed estimates; "negative" if you would like to assume that publication bias favors significant negative-signed estimates.')
                                                                ),
                                                              
                                                              
                                                              
                                                              numericInput('eta', 'Hypothetical publication bias severity', NA, min = 1, max = 200, step = 0.01) %>%
                                                                shinyInput_label_embed(
                                                                  shiny_iconlink() %>%
                                                                    bs_embed_popover(title = 'The number of times more likely an affirmative study is to be published than a nonaffirmative study. Used to adjust point estimate for specified amount of publication bias. \nWorst-case publication bias is when affirmative studies are essentially infinitely more likely to be published than nonaffirmative studies.')),
                                                              
                                                              
                                                              
                                                              checkboxInput( 'return_worst_meta', 'Also show worst-case publication bias', TRUE ),                                                           
                                                      ), # closes column
                                                      
                                                      column( width=6,
                                                              
                                                              numericInput('alpha_select', 'Two-sided p-value at which publication probability is assumed to change', 0.05, min = 0, max = 1, step = 0.01) %>%
                                                                
                                                                shinyInput_label_embed(
                                                                  shiny_iconlink() %>%
                                                                    bs_embed_popover(title = 'For most scientific disciplines, this will be 0.05.'))
                                                              
                                                              
                                                      )
                                                      
                                                      
                                  )
                                ),  ##closes fluidRow
                                
                                hr(),
                                
                                ### results text ###
                                
                                wellPanel( textOutput("calibrated_results_prop"), span( textOutput("calibrated_text1") )
                                           # for "i" information icon, not currently in use
                                           #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                                ),
                                wellPanel( textOutput("calibrated_results_minbias"), span( textOutput("calibrated_text2") )
                                           # for "i" information icon, not currently in use
                                           #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                                ),
                                wellPanel( textOutput("calibrated_results_minconf"), span( textOutput("calibrated_text3") )
                                           # for "i" information icon, not currently in use
                                           #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                                ),
                                
                                mainPanel(
                                  span( htmlOutput("calibrated_cm_messages"), style="color:red"), width = 8
                                ),
                                
                                hr(),
                                
                                ### used for plot only:
                                shinydashboard::box(width=6,
                                                    title=h4(strong("Range of bias factors to show on plot")),
                                                    numericInput('calibrated_Bmin', 'Lower limit of bias factor (on scale you specified)', 1, min=0, max=Inf, step=0.1) %>%
                                                      shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                          bs_embed_popover(title = 'used for plot only')),
                                                    numericInput('calibrated_Bmax', 'Upper limit of bias factor (on scale you specified)', 4, min=0, max=Inf, step=0.1) %>%
                                                      shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                          bs_embed_popover(title = 'used for plot only')),
                                                    actionButton(inputId = 'calibrated_plot', label='Generate plot')
                                ),
                                
                                mainPanel(
                                  plotOutput('calibrated_plot1')
                                ),
                                ### plot warnings:
                                mainPanel(
                                  span( htmlOutput("calibrated_sens_plot_messages"), style="color:red"), width = 8
                                )
                                
                       ) ### closes tabPanel "Calibrated"
                       
                       # tabPanel("Parametric estimation (allows heterogeneous bias)",
                       #          dashboardBody(
                       #            tags$style(HTML('
                       #                              
                       #                              .box {margin: 0px;}
                       #                              
                       #                              ')),
                       #            
                       #            fluidRow(
                       #              tags$style(type = "text/css",
                       #                         "label { font-size: 12px; }"
                       #              ),
                       #              ### hidden method input used in server.R
                       #              column(width=12, shinyjs::hidden(selectInput('parametric_method', 'Method (calibrated or parametric)', choices = c('parametric'), selected = 'parametric'))
                       #              ),
                       #              shinydashboard::box(width=4,
                       #                                  
                       #                                  title= h4(strong("Input estimates from confounded meta-analysis")),
                       #                                  
                       #                                  column(width=10,
                       #                                         selectInput('parametric_scale', 'Scale (RR or log-RR)', choices = c('RR', 'Log-RR'), selected = 'RR') %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'The scale (relative risk [RR] or log-relative risk [log-RR] on which you will input the meta-analytic pooled estimate, threshold, and mean bias factor below.'))
                       #                                         ,
                       #                                         
                       #                                         numericInput('parametric_yr', 'Pooled effect size', NA, min = 0, max = Inf, step = 0.1) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'The usual estimate of the average effect size in the meta-analysis of the potentially confounded studies, prior to any correction for unmeasured confounding')),
                       #                                         numericInput('parametric_vyr', 'Estimated variance of pooled point estimate (optional)', NA, min = 0, max = Inf, step = 0.01) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'The estimated variance of the pooled point estimate from confounded meta-analysis. Since the meta-analysis should be conducted with the point estimates on the log scale, you should input the variance as it is reported by your meta-analysis software without taking the log again. If not provided, you will not get confidence intervals for the sensitivity analyses.')),
                       #                                         numericInput('parametric_t2', paste0('Estimated heterogeneity (', '\u03c4\u00b2', ')'), NA, min = 0, max = Inf, step = 0.1) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = paste0('The estimated heterogeneity (', '\u03c4\u00b2', ') from the confounded meta-analysis. Since the meta-analysis should be conducted with the point estimates on the log scale, you should input ', '\u03c4\u00b2', ' as it is reported by your meta-analysis software without taking the log again.'))),
                       #                                         numericInput('parametric_vt2', paste0('Estimated variance of ', '\u03c4\u00b2', '  (optional)'), NA, min = 0, max = Inf, step = 0.01) %>%
                       #                                           
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = paste0('The estimated variance of (', '\u03c4\u00b2', ') from the confounded meta-analysis. Since the meta-analysis should be conducted with the point estimates on the log scale, you should input ', '\u03c4\u00b2', ' as it is reported by your meta-analysis software without taking the log again.')))
                       #                                  )
                       #              ),
                       #              shinydashboard::box(width=6,
                       #                                  title= h4(strong("Specify sensitivity parameters and thresholds")),
                       #                                  column(width=6,
                       #                                         
                       #                                         numericInput('parametric_muB', 'Mean bias factor across studies (on scale you specified)', NA, min = 0, max = Inf, step = 0.01) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'Mean bias factor on the chosen scale (RR or log) across studies. To estimate the proportion of effects stronger than q without correction for unmeasured confounding, set to 1.')),
                       #                                         numericInput('parametric_prop_t2', paste0('Proportion of heterogeneity (', '\u03c4\u00b2', ') due to variation in confounding bias '), NA, min = 0, max = 1, step = .1) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = paste0('The proportion of the confounded heterogeneity estimate', '\u03c4\u00b2', ' that is thought to be due to variation across studies in confounding bias rather than to genuine effect heterogeneity. This proportion allows to you to specify how variable you think confounding bias is across studies.'))),
                       #                                         numericInput('parametric_r', 'Proportion below which strong effects are to be reduced (r)', NA, min = 0, max = 1, step = 0.1) %>%
                       #                                           
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'For the second two metrics, the value to which the proportion of meaningfully strong effects is to be reduced'))
                       #                                  ),
                       #                                  
                       #                                  column(width=6,       
                       #                                         
                       #                                         numericInput('parametric_q', 'Threshold (q) for meaningfully strong effect size (on scale you specified)', NA, min = 0, max = Inf, step = 0.01) %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'Effect size that represents the minimum threshold for a meaningfully strong effect
                       #                                                              size (on scale you specified)')),
                       #                                         
                       #                                         selectInput('parametric_tail', 'Tail', choices = c('above', 'below'), selectize = FALSE, size = 2, selected = 'above') %>%
                       #                                           shinyInput_label_embed(
                       #                                             shiny_iconlink() %>%
                       #                                               bs_embed_popover(title = 'above for the proportion of effects above q; below for the proportion of effects below q')),
                       #                                         actionButton(inputId = 'parametric_calculate', label='Analyze')
                       #                                         
                       #                                  )
                       #                                  
                       #              )
                       #              
                       #              
                       #            )), ## closes fluidRow and dashboardBody
                       #          
                       #          hr(),
                       #          
                       #          ### results text ###
                       #          
                       #          wellPanel( textOutput("parametric_results_prop"), span( textOutput("parametric_text1") ),
                       #                     # for "i" information icon, not currently in use
                       #                     #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                       #                     span( htmlOutput("parametric_phatwarn"), style="color:red")),
                       #          wellPanel( textOutput("parametric_results_minbias"), span( textOutput("parametric_text2") )
                       #                     # for "i" information icon, not currently in use
                       #                     #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                       #          ),
                       #          wellPanel( textOutput("parametric_results_minconf"), span( textOutput("parametric_text3") )
                       #                     # for "i" information icon, not currently in use
                       #                     #, shiny_iconlink() %>% bs_embed_popover(title = "PLACEHOLDER INFORMATION ICON")
                       #          ),
                       #          mainPanel(
                       #            span( htmlOutput("parametric_cm_messages"), style="color:red"), width = 8
                       #          ),
                       #          
                       #          hr(),
                       #          
                       #          shinydashboard::box(width=6,
                       #                              title=h4(strong("Range of bias factors to show on plot")),
                       #                              numericInput('parametric_Bmin', 'Lower limit of bias factor (Bmin)', 1, min=0.1, max=Inf, step=0.1) %>%
                       #                                shinyInput_label_embed(
                       #                                  shiny_iconlink() %>%
                       #                                    bs_embed_popover(title = 'used for plot only')),
                       #                              numericInput('parametric_Bmax', 'Upper limit of bias factor (Bmax)', 4, min=0, max=Inf, step=0.1) %>%
                       #                                shinyInput_label_embed(
                       #                                  shiny_iconlink() %>%
                       #                                    bs_embed_popover(title = 'used for plot only')),
                       #                              actionButton(inputId = 'parametric_plot', label='Generate plot')
                       #          ),
                       #          
                       #          
                       #          mainPanel(
                       #            plotOutput('parametric_plot1')
                       #          ),
                       #          
                       #          ### plot warnings:
                       #          ## jl: all warnings from sens_plot should be output with the below now:
                       #          mainPanel(
                       #            span( htmlOutput("parametric_sens_plot_messages"), style="color:red"), width = 8
                       #          )
                       #          
                       # ) ### closes tabPanel "Parametric"
                     ) ### closes tabsetPanel
                     
            ), ### closes tabPanel "Fixed sensitivity parameters"
            
            tabPanel("More resources",
                     
                     mainPanel(      HTML(paste( 
                       
                       
                       "<b>More resources for these sensitivity analyses</b>",
                       
                       "<br><br>In addition to using this website, you can alternatively conduct these sensitivity analyses

                       using the functions <code>confounded_meta</code> and <code>sens_plot</code> in the R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a> (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066405/'>Mathur et al., 2018</a>).",
                       
                       "<br><br>For more information on the interpretation of these sensitivity analyses and guidance on choosing the sensitivity parameters, see <a href='https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598'>Mathur & VanderWeele (2020a)</a>,
                       and for a review of methods to choose a threshold representing a meaningfully strong effect size, see the Supplement of <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8057'>Mathur & VanderWeele (2019).</a>
                       For more on the robust estimation methods, see <a href='https://journals.lww.com/epidem/Fulltext/2020/05000/Robust_Metrics_and_Sensitivity_Analyses_for.7.aspx?casa_token=PELPmhG6P3wAAAAA:D2bYC3kXCtRmncY-ELNt4I8ub1ZUhwTQjsFq8vh05h_EhV4kOJQuR3L97TsSLtun4zQ26Ys26ayF3aleMKj-93Q'>Mathur & VanderWeele (2020b).</a></a>",
                       
                       
                       
                       "<br><br><b>More resources for other biases and study designs</b>",
                       
                       "<br><br>Similar methods and tools are also available to conduct analogous sensitivity analyses for other types of biases as follows. </br></br>

                       
                       To assess other biases in meta-analyses:   
                       
                       <ul>
                       <li>Publication bias in meta-analyses (<a href='https://osf.io/s9dp6/'>Mathur & VanderWeele, 2020c</a>;
                       R package <a href='https://cran.r-project.org/web/packages/PublicationBias/index.html'>PublicationBias</a>)</li>
                       
                       </ul>
                       
                       To assess biases in individual studies:
                       
                       <ul>
                       <li>Unmeasured confounding (the E-value) (<a href='https://annals.org/aim/fullarticle/2643434/sensitivity-analysis-observational-research-introducing-e-value'>VanderWeele & Ding, 2017</a>; <a href='http://www.evalue-calculator.com'>website</a>, R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>, or Stata package <a href='https://journals.sagepub.com/doi/abs/10.1177/1536867X20909696'>evalue</a>)</li>
                       <li>Selection bias (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6553568/'>Smith & VanderWeele, 2019a</a>; <a href='https://selection-bias.herokuapp.com/'>website</a> or R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>)</li>
                       
                       <li>Measurement error (<a href='https://academic.oup.com/aje/article/188/10/1823/5506602?casa_token=5ZyiVJp9_5UAAAAA:40rpOH1mRz0IDeRJ35atRRk9x6MJgIHMNOxLCcsnfouzN3qWXrght0XVWNIHQcRwWP1Bhgl8vY9B'>VanderWeele & Li, 2019</a>; R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>)</li>
                       
                       <li>A combination of unmeasured confounding, selection bias, and measurement error simultaneously (<a href='http://arxiv.org/abs/2005.02908'>Smith et al, 2020</a>;
                       R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>) </li>
                       
                       </ul>
                       
                       An analog of the E-value is also available to address unmeasured mediator-outcome confounding when carrying out mediation analysis for direct and indirect effects (<a href='https://journals.lww.com/epidem/Fulltext/2019/11000/Mediational_E_values__Approximate_Sensitivity.9.aspx'>Smith & VanderWeele, 2019b</a>).
                       
                       
                       </br></br>
                       ",
                       
                       "<b>Developers</b>",
                       
                       
                       "<br><br>This website was created by <a href='http://www.mayamathur.com'>Maya Mathur</a>, <a href='https://med.stanford.edu/qsu/current-members/Justin_Lee.html'>Justin Lee</a>, and <a href='https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/'>Tyler VanderWeele</a>.",
                       
                       
                       
                       "<br><br><b>References</b><br>",
                       
                       
                       "<ul>

                       <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4820664/'>Ding P & VanderWeele TJ (2016)</a>. Sensitivity analysis without assumptions. <i>Epidemiology</i>, 27(3), 368–377.</li>
                       
                       <li><a href='https://jamanetwork.com/journals/jama/fullarticle/2723079?casa_token=vP0UXdEX4HAAAAAA:y0GoUYecb4QvGnn23FNxpnOsBu5Z70-DW1apD84XqPWNL0kXYDXlT5hQVweAUZVh6zJe9BU_sA'>Haneuse S, VanderWeele TJ, & Arterburn D (2019)</a>. Using the E-value to assess the potential effect of unmeasured confounding in observational studies. <i>Journal of the American Medical Association</i>, 321(6), 602-603.</li>
                       
                       
                       <li> <a href='https://journals.sagepub.com/doi/abs/10.1177/1536867X20909696'>Linden A, Mathur MB, & VanderWeele TJ (2020)</a>. Conducting sensitivity analysis for unmeasured confounding in observational studies using E-values: The evalue package. <i>The Stata Journal</i> (in press).</li>
                       
                       <li> <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066405/'>Mathur MB, Ding P, Riddell CA, & VanderWeele TJ (2018).</a> Website and R package for computing E-values. <i>Epidemiology</i> 29(5), e45.</li>
                       
                       <li> <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8057'>Mathur MB & VanderWeele TJ (2019).</a> New statistical metrics for meta-analyses of heterogeneous effects. <i>Statistics in Medicine</i> 38(8), 1336-1342.</li>
                       
                       
                       <li><a href='https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598'>Mathur MB & VanderWeele TJ (2020a)</a>. Sensitivity analysis for unmeasured confounding in meta-analyses. <i>Journal of the American Statistical Association</i> 115(529), 163-170.</li>
                       
                       <li><a href='https://pubmed.ncbi.nlm.nih.gov/32141922/'>Mathur MB & VanderWeele TJ (2020b)</a>. Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. <i>Epidemiology</i> 31(3), 356-358.</li>
                       
                       <li><a href='https://osf.io/s9dp6/'>Mathur MB & VanderWeele TJ (2020c)</a>. Sensitivity analysis for publication bias in meta-analyses. <i>Journal of the Royal Statistical Society: Series C</i> 69(5), 1091-1119.</li>
                       <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6553568/'>Smith LH & VanderWeele TJ (2019a)</a>. Bounding bias due to selection. <i>Epidemiology</i> 30(4), 509.</li>
                       <li><a href='https://journals.lww.com/epidem/Fulltext/2019/11000/Mediational_E_values__Approximate_Sensitivity.9.aspx'>Smith LH & VanderWeele TJ (2019b)</a>. Mediational E-values: Approximate sensitivity analysis for mediator-outcome confounding. <i>Epidemiology</i> 30(6), 835-837.</li>
                       
                       <li><a href='https://annals.org/aim/fullarticle/2643434/sensitivity-analysis-observational-research-introducing-e-value'>VanderWeele TJ & Ding P (2017)</a>. Sensitivity analysis in observational research: Introducing the E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.</li>
                       
                       <li><a href='https://www.degruyter.com/view/journals/jci/7/2/article-20180007.xml'>VanderWeele TJ, Ding P, & Mathur MB (2019a)</a>. Technical considerations in the use of the E-value. <i>Journal of Causal Inference</i>, 7(2).</li>
                       <li><a href='https://annals.org/aim/article-abstract/2719984/correcting-misinterpretations-e-value'>VanderWeele TJ, Mathur MB, & Ding P (2019b)</a>. Correcting misinterpretations of the E-value. <i>Annals of Internal Medicine</i> 170(2), 131-132.</li>
                       
                       <li><a href='https://academic.oup.com/aje/article/188/10/1823/5506602?casa_token=5ZyiVJp9_5UAAAAA:40rpOH1mRz0IDeRJ35atRRk9x6MJgIHMNOxLCcsnfouzN3qWXrght0XVWNIHQcRwWP1Bhgl8vY9B'>VanderWeele TJ & Li Y (2019)</a>. Simple sensitivity analysis for differential measurement error. <i>American Journal of Epidemiology</i>, 188(10), 1823-1829.</li>
                       </ul>"
                       
                       
                       
                     ) ) )
            )
            
) ## closes navbarPage


