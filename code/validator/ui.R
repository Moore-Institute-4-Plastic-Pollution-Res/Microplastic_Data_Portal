
dashboardPage(
    fullscreen = T,
    help = T,
    #options = list(skin = "blue"),
    dashboardHeader(title = "Data Validator"),
    dashboardSidebar(
        sidebarUserPanel(
            #image = "https://drive.google.com/file/d/13iCjC10dV3giFhCCoir_8mnbwtHM1rMA/view?usp=sharing",
            name = "Welcome!"
        ),
        sidebarMenu(
            id = "sidebarmenu",
            #sidebarHeader("Header 1"),
            menuItem(
                "About",
                tabName = "item1",
                icon = icon("sliders-h")
            ),
            menuItem(
                "Validator",
                tabName = "item2",
                icon = icon("check")
            ),
            menuItem(
                "Help",
                tabName = "item3",
                icon = icon("question")
            )
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tabItems(
            tabItem(
                tabName = "item1",
                box(
                    title = "Overview",
                    p("Welcome to the Data Validator webpage. This tool allows you to validate data interactively by uploading a dataset and rules file. To get started, go to the validator tab on the left."),
                    HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/GKsoNega7CY" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>'),
                    width = 12
                ),
                box(
                    title = "Instructions and Examples",
                    collapsed = T,
                    p("Both the data and rules files must be in .csv format. Examples for how to structure and query the data and rules can be found below:"),
                        popover(
                            downloadButton("download_rules", "Download Sample Rules", style = "background-color: #2a9fd6;"),
                            title = "Download rules file",
                            content = "This is an example file that can be used in tandem with the valid or invalid data files to test out the tool."
                        ),
                        popover(
                            downloadButton("download_rules_excel", "Rules Template", style = "background-color: #28a745;"),
                            title = "Download rules template file",
                            content = "This is an file that can be used as a template when collecting data so that it conforms to most of the rules tested in this portal."
                        ),    
                        popover(
                            downloadButton("download_good_sample", "Download Valid Sample Data", style = "background-color: #28a745;"),
                            title = "Download valid example data",
                            content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that is 100% validated."
                        ),
                        popover(
                            downloadButton("download_sample", "Download Invalid Sample Data", style = "background-color: #dc3545;"), #%>%

                            title = "Download invalid example data",
                            content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that isn't 100% validated."
                        ),
                    tags$ol(
                        tags$li("Uploaded the data and rules file on the validator tab."),
                        tags$li("You will either recieve a certificate that your data is valid (which you may download), or notification of any issue(s) found."),
                        tags$li("In the event of invalid data, the description of the issue(s) to be resolved and severity will be displayed in the 'Issues Raised' panel."),
                        tags$li("You can click on any of the descriptions to display the rows where the issue was found in the 'Issues Selected' panel."),
                        tags$li("The 'Issues Raised' and 'Issue Selected' data sheets may be copied, or downloaded as CSV, Excel, or PDF.")
                    ),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = F,
                    p("Join our team to build this tool!"),
                    HTML(paste0('<a class="btn btn-info" href = "', config$github,'" role = "button" >Github</a>')),
                    boxLayout(
                        type = "columns",
                        #title = "The Team",
                        #collapsed = T,
                        lapply(config$contributors, function(x){x})
                    ),
                    width = 12
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(2,
                           popover(
                               fileInput("file", NULL,
                                         placeholder = ".csv",
                                         buttonLabel = "Data...",
                                         multiple = T,
                                         accept=c("text/csv",
                                                  "text/comma-separated-values,text/plain")), #%>%
                               title = "Upload CSV to validate",
                               content = "This can only be uploaded after the rules file. This is where you upload the csv file that you want to validate using the rules file.")
                           #      size = "medium", rounded = TRUE
                    ),
                    column(2,
                           uiOutput(outputId = "rules_upload")
                    ),
                    column(8, uiOutput("certificate"), uiOutput("alert"))),
                    uiOutput("error_query"),
                fluidRow(
                    popover(
                        box(
                            title = "Rules File",
                            collapsed = T,
                            width = 12,
                            DT::dataTableOutput("rules_dt"),
                            style = 'overflow-x: scroll'
                        ),
                        title = "Rules File",
                        placement = "bottom",
                        content = "Backend file of rules currently in use.")
                ),
                fluidRow(
                    popover(
                      box(
                        title = "Diagnose",
                        collapsed = T,
                        width = 12,
                        jsoneditOutput("remote_out"),
                        jsoneditOutput("validation_out")#,
                    ),
                    title = "Diagnose",
                    placement = "bottom",
                    content = "For Developmental & Debugging Purposes")
                )
            ),
            tabItem(
                tabName = "item3",
                box(
                    title = "FAQs",
                    strong("Where is my data going?"),
                    p("Data uploaded to the validator does not get saved unless you have a specific agreement with our group to save it."),
                    strong("Is this open source web tool secure?"),
                    p("The validator app is https encrypted. Source code is available on GitHub for security review."),
                    width = 12
                ),
                box(
                    title = "Contact Us",
                    p("Have any additional questions or concerns? Email us using the link below:"),
                    HTML(paste0('<a class="btn btn-info" href = "mailto:', config$contact, '" role = "button" >Contact Us</a>')),
                    p("Please include in your email:"),
                    p ("(1) What should the app be doing?"),
                    p ("(2) What is the app doing instead?"),
                    width = 12
                )
            )

        )

    ),

    footer = dashboardFooter(left = fluidRow(column(1,a(href = config$twitter, icon('twitter'))),
                                             column(1,a(href = config$github, icon('github'))),
                                             column(1,a(href = config$license, img(src= "CC.png", width= 18, height= 18))))
                            
    )
)
