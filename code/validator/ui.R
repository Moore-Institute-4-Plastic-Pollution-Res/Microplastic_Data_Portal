dashboardPage(
    fullscreen = T,
    help = T,
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
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = T,
                    p("You can help us build this tool!"),
                    HTML('<a class="btn btn-info" href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal" role = "button" >Github</a>'),
                    width = 12
                    
                )
            ),
            tabItem(
                tabName = "item2",
                fluidRow(
                    column(1,
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
                    ),
                    column(1, 
                           popover(
                               downloadButton("download_rules", "", style = "background-color: #2a9fd6;"), #%>%
                               title = "Download Rules Example",
                               content = "This is an example rules file, follow the format of this rules file to create your own."
                           )
                    ),
                    column(1,
                           popover(
                                   fileInput("file", NULL,
                                             placeholder = ".csv",
                                             buttonLabel = "Data...",
                                             multiple = T,
                                             accept=c("text/csv",
                                                      "text/comma-separated-values,text/plain")), #%>%
                               
                               title = "Upload CSV to validate",
                               content = "This can only be uploaded after the rules file. This is where you upload the csv file that you want to validate using the rules file."), 
                           #      size = "medium", rounded = TRUE
                    ),
                    column(1,
                           popover(
                               downloadButton("download_sample", "", style = "background-color: #dc3545;"), #%>%
                               
                               title = "Download invalid example data",
                               content = "This is an example file that can be used in tandem with the example rules file to test out the tool for its performance with a dataset that isn't 100% validated."
                           ),
                           popover(
                               downloadButton("download_good_sample", "", style = "background-color: #28a745;"), #%>%
                               
                               title = "Download validated example data",
                               content = "This is an example file that can be used in tandem with the example rules file to test out the tool with a dataset that is 100% validated."
                           )
                           #     type = "info", 
                           #     size = "medium", rounded = TRUE
                           # )
                    ),
                    column(8, uiOutput("certificate"), uiOutput("alert"))),
                fluidRow(
                    popover(
                        box(title = "Issues Raised", 
                            id = "issues_raised",
                            dropdownMenu = boxDropdown(
                                boxDropdownItem(
                                    prettySwitch("show_decision",
                                                 label = "Errors only?",
                                                 inline = T,
                                                 value = T,
                                                 status = "success",
                                                 fill = T))
                            ),
                            DT::dataTableOutput("show_report"),
                            style = 'overflow-x: scroll',
                            maximizable = T,
                            width = 4
                        ),
                        title = "Issues Raised",
                        placement = "left",
                        content = "This is where the rules that are violated (or all rules if the advanced tool is turned on) show up. The table appears after data upload and is selectable which will query the issue selected box."),
                    popover(
                        box(title = "Issue Selected",
                            id = "issue_selected",
                            DT::dataTableOutput("report_selected"),
                            style = 'overflow-x: scroll',
                            maximizable = T,
                            width = 8
                        ),
                        title = "Issue Selected", 
                        placement = "left",
                        content = "This is where the selection in the issues raised box will show up. Whatever rule is selected will query the dataset and show any rows that violate the rule and show any problematic columns in red."
                    )
                ),
                fluidRow(
                    box(
                        title = "Diagnose",
                        width = 12, 
                        reactjsonOutput("rules_out"),
                        reactjsonOutput("validation_out")#, 
                       # reactjsonOutput("remote_out") 
                    )
                )
            )
        )
    )
)