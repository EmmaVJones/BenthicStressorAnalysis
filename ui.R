shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Stressor Analysis Tool",
                             #tabPanel("About",fluidRow(column(10,
                            #                                  h4("This app was created to assist in the identification of benthic stressors 
                            #                                     to aquatic communities."),
                            #                                  h5("By uploading field parameters and chemistry data from a given sample site, 
                            #                                     users may compare their dataset to 16 years of Virginia Probabilistic Monitoring
                            #                                     data by major river basin, stream order, and ecoregion."),
                            #                                  br(),
                            #                                  h5(strong("Authors:")),
                            #                                  h5("Jason Hill, Water Quality Assessment Team Leader"),h5("Chip Sparks, Regional Biologist"),
                            #                                  h5("Mary Dail, Water Quality Assessment Scientist"),h5("Emma Jones, Water Quality Assessment Data Analyst"),
                            #                                  h5("Lucy Baker, Regional TMDL Coordinator"),
                            #                                  br(),
                            #                                  h5(strong("Background:")),
                            #                                  p("The DEQ assesses aquatic invertebrate communities in order to evaluate whether or 
                            #                                    not Virginia’s aquatic life use standard is being met. To meet both state and federal 
                            #                                    requirements, DEQ assesses water quality data, including chemical and biological data, 
                            #                                    every two years and subsequently lists those waterbodies not meeting Virginia’s water 
                            #                                    quality standards on the 303(d) list. The root cause of an aquatic community shift is 
                            #                                    rarely obvious even though changes in the composition and/or abundance of the benthic 
                            #                                    macroinvertebrate community are distinct. DEQ utilizes EPA’s recommended method, stressor 
                            #                                    analysis, to systematically characterize the cause of an aquatic community shift. The goal
                            #                                    of the stressor analysis process is to apply a weight-of-evidence approach to define a/the 
                            #                                    most probable stressor(s) that explain(s) the shift in the benthic macroinvertebrate community. 
                            #                                    Aquatic community stressors encompass a wide array of parameters that have varying degrees of 
                            #                                    synergistic interactions, further complicating the stressor analysis process. Recognizing these
                            #                                    challenges, stressor thresholds were developed utilizing ten years of data collected through 
                            #                                    DEQ’s Freshwater Probabilistic Monitoring Program.  Stressor thresholds are concentration/measured 
                            #                                    ranges linked to varying levels of stress to aquatic life that present context for stressor 
                            #                                    analyses reviewers and developers to evaluate water quality datasets and relate them to aquatic 
                            #                                    community outcomes. Statewide, ecoregion-, basin-, and stream order-specific context is presented 
                            #                                    in relation to the following common aquatic stressors: dissolved oxygen, pH, total phosphorus, 
                            #                                    total nitrogen, ionic strength (specific conductivity, TDS, and dissolved sulfate, chloride, 
                            #                                    sodium, and potassium), dissolved metals cumulative criterion unit, total habitat and relative 
                            #                                    bed stability.  Specifically, thresholds ranging from no stress to aquatic life to high probability 
                            #                                    of stress aquatic life were developed and integrated into a web-based application for better 
                            #                                    understanding stressors."),
                            #                                  hr(),
                            #                                  h4(strong("To begin analysis, simply navigate to the 'Upload Data' tab and follow the on screen 
                            #                                            instructions.")),
                            #                                  h4(strong("See the 'How To' tab for additional instructions on pulling data
                            #                                            necessary to run the tool and report."))))),
                             tabPanel("How To",
                                      #htmlOutput("TMDLBenthicStressorToolHowTo")),
                                      h3('Requires updating')),
                             tabPanel("Data Acquisition",
                                      h3('Requires updating')),
                             tabPanel("Data Upload",
                                      h3('Requires updating')),
                             tabPanel("Data Summary",
                                      h3('Requires updating')),
                             tabPanel("Statewide Map",
                                      h3('Requires updating')),
                             tabPanel("Dissolved Metals",
                                      h3('Requires updating')),
                             tabPanel('Report',
                                      h3('Requires updating'))
                  )))
                  
                             