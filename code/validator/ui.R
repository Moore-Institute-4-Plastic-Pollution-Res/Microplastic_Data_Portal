
dashboardPage(
    fullscreen = T,
    help = T,
    dashboardHeader(title = config$portal_name,
                    includeCSS("www/datatable.css")),
    dashboardSidebar(
        sidebarUserPanel(
            name = config$portal_funder_name,
            image = config$portal_funder_link
        ),
        sidebarMenu(
            id = "sidebarmenu",
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
                    HTML(paste0('<iframe width="560" height="315" src="',config$overview,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = F,
                    p("Join our team to build this tool!"),
                    HTML(paste0('<a class="btn btn-info" href = "', config$github,'" role = "button" >Github</a>')),
                    boxLayout(
                        type = "columns",
                        lapply(config$contributors, function(x){x})
                    ),
                    width = 12
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(4,
                           popover(
                               fileInput("file", NULL,
                                         placeholder = "Start Here",
                                         buttonLabel = "Upload Data",
                                         width = "100%",
                                         multiple = T,
                                         accept=c("text/csv",
                                                  "text/comma-separated-values,text/plain")), #%>%
                               title = "Upload CSV to validate",
                               content = "This can only be uploaded after the rules file. This is where you upload the csv file that you want to validate using the rules file.")
                    ),
                    column(4,
                           if(!isTruthy(config$rules_to_use)){
                               popover(
                                   fileInput("file_rules", NULL,
                                             placeholder = ".csv",
                                             buttonLabel = "Rules...",
                                             width = "100%",
                                             accept=c("text/csv",
                                                      "text/comma-separated-values,text/plain")),
                                   title = "Upload rules",
                                   content = "Upload the rules csv to use to validate the data csv"
                               ) 
                           }
                    ), 
                    column(4, 
                           uiOutput("alert"))),
                    uiOutput("error_query"),
                    uiOutput("dev_options")
            ),
            tabItem(
                tabName = "item3",
                box(
                    title = "Tutorial",
                    p("Welcome to the Data Validator webpage. This tool allows you to validate data interactively by uploading a dataset and rules file. To get started, go to the validator tab on the left."),
                    HTML(paste0('<iframe width="560" height="315" src="',config$tutorial,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>')),
                    width = 12
                ),
                box(
                    title = "Instructions and Examples",
                    p("Both the data and rules files must be in .csv or .xlsx format. Examples for how to structure and query the data and rules can be found below:"),
                    popover(
                        downloadButton("download_rules", "Download Sample Rules", style = "background-color: #2a9fd6;"),
                        title = "Download rules file",
                        content = "This is an example file that can be used in tandem with the valid or invalid data files to test out the tool."
                    ),
                    popover(
                        downloadButton("download_rules_excel", "Rules Template", style = "background-color: #e83e8c;"),
                        title = "Download rules template file",
                        content = "This is an file that can be used as a template when collecting data so that it conforms to most of the rules tested in this portal."
                    ),    
                    popover(
                        downloadButton("download_good_sample", "Download Valid Sample Data", style = "background-color: #28a745;"),
                        title = "Download valid example data",
                        content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that is 100% validated."
                    ),
                    popover(
                        downloadButton("download_sample", "Download Invalid Sample Data", style = "background-color: #dc3545;"), 
                        title = "Download invalid example data",
                        content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that isn't 100% validated."
                    ),
                    br(),
                    tags$ol(
                        tags$li("Uploaded the data and rules file on the validator tab."),
                        tags$image(src = "upload.png", width = "50%"),
                        tags$li("If your data is valid a popup will appear that allows you to upload to a remote repository if you have a key."),
                        tags$image(src = "popup.png", width = "50%"),
                        tags$li("If your data is valid you may download a certificate, this is saved on our end too to prove the event occured."),
                        tags$image(src = "download.png", width = "50%"),
                        tags$li("In the event of invalid data, the description of the issue(s) to be resolved and severity will be displayed in the 'Issues Raised' panel."),
                        tags$image(src = "error.png", width = "50%"),
                        tags$li("You can click on any of the descriptions to display the rows where the issue was found in the 'Issues Selected' panel."),
                        tags$li("The 'Issues Raised' and 'Issue Selected' data sheets may be copied, or downloaded as CSV, Excel, or PDF.")
                    ),
                    width = 12
                ),
                box(
                    title = "FAQs",
                    strong("Where is my data going?"),
                    p("Links shared in this tool for upload will be added to an S3 bucket to test that they are able to be downloaded. Data will not be shared externally unless you specify the data can be shared by inputting the security key."),
                    strong("Is this open source web tool secure?"),
                    p("The validator app is https encrypted and the source code is available on GitHub for security review."),
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
