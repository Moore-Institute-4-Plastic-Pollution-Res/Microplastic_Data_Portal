
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
                        tags$li("The 'Issues Raised' and 'Issue Selected' data sheets may be copied, or downloaded as CSV, Excel, or PDF."),
                    ),
                    width = 12
                ),
                box(
                    title = "Contribute",
                    collapsed = F,
                    p("Join our team to build this tool!"),
                    HTML('<a class="btn btn-info" href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal" role = "button" >Github</a>'),
                    boxLayout(
                        type = "columns",
                        #title = "The Team",
                        #collapsed = T,
                        boxProfile(image = "https://i0.wp.com/mooreplasticresearch.org/wp-content/uploads/2022/03/20220321_104726_2.jpg?resize=1200%2C1600&ssl=1", title = "Win Cowger", subtitle = "Moore Institute for Plastic Pollution Research", bordered = FALSE),
                        boxProfile(image = "https://i0.wp.com/mooreplasticresearch.org/wp-content/uploads/2022/03/20220307_110138-1.jpg?resize=1188%2C1536&ssl=1", title = "Shelly Moore", subtitle = "Moore Institute for Plastic Pollution Research", bordered = FALSE),
                        boxProfile(image = "https://envisci.ucr.edu/sites/default/files/styles/profile_card_118_x_118/public/Hannah%20Hapich.jpg?h=cbdac610&itok=fjGpJ7uI", title = "Hannah Hapich", subtitle = "Moore Institute for Plastic Pollution Research", bordered = FALSE),
                        boxProfile(image = "https://media.licdn.com/dms/image/C5603AQEiefNJhW36QQ/profile-displayphoto-shrink_400_400/0/1517424516334?e=1678924800&v=beta&t=92DMrsU_rWgFFwxy9YFOqVY1NuJGTC5FKN3rmS7EU3g", title = "Gabriel Daiess", subtitle = NULL, bordered = FALSE),
                        boxProfile(image = "https://www.sccwrp.org/wp/wp-content/uploads/2020/03/Leah-Thomas-Hampton-SCCWRP-bkg-WebAR.jpg", title = "Leah Thornton Hampton", subtitle = "Southern California Coastal Water Research Project", bordered = FALSE),
                        boxProfile(image = "", title = "Richard Nelson", subtitle = "California State Water Resources Control Board", bordered = FALSE),
                        boxProfile(image = "https://media.licdn.com/dms/image/C5603AQHVTKQR_Xmm8g/profile-displayphoto-shrink_400_400/0/1654033306784?e=1678924800&v=beta&t=6LRkyHUOIo7MkUlmrRmDR5Ft55rSyC0KMnLec4qW3Jk", title = "Haig Jack Minasian", subtitle = "CSU Long Beach", bordered = FALSE),
                        boxProfile(image = "https://media.licdn.com/dms/image/C5603AQHLq0Iv-e9SGg/profile-displayphoto-shrink_400_400/0/1588630989428?e=1678924800&v=beta&t=suBzVjL4nwHZzOEUSHDRHwlfGZiccL4aqAKEYudN4jc", title = "Holden Ford", subtitle = "CSU Long Beach", bordered = FALSE),
                        boxProfile(image = "", title = "Anja Oca", subtitle = "CSU Long Beach", bordered = FALSE),
                        boxProfile(image = "", title = "Libby Heeren", subtitle = "Moore Institute for Plastic Pollution Research", bordered = FALSE)
                    ),
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
                    column(1
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
                    column(1
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
                    popover(
                      box(
                        title = "Diagnose",
                        collapsed = T,
                        width = 12,
                        #jsoneditOutput("remote_out"),
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
                    HTML('<a class="btn btn-info" href = "mailto:win@mooreplasticresearch.org" role = "button" >Contact Us</a>'),
                    p("Please include in your email:"),
                    p ("(1) What should the app be doing?"),
                    p ("(2) What is the app doing instead?"),
                    width = 12
                )
            )

        )

    ),

    footer = dashboardFooter(left = fluidRow(column(1,a(href = "https://twitter.com/Win_OpenData", icon('twitter'))),
                                             column(1,a(href = "https://github.com/Moore-Institute-4-Plastic-Pollution-Res/Microplastic_Data_Portal/tree/main/code/validator", icon('github'))),
                                             column(1,a(href = "https://creativecommons.org/licenses/by/4.0/", img(src= "CC.png", width= 18, height= 18)))),
                            right = fluidRow(
                                column(6, actionButton(
                                inputId = "login",
                                label = "Login",
                                #icon = "icon-signout",
                                width = NULL,
                                status = NULL,
                                gradient = FALSE,
                                outline = FALSE,
                                size = NULL,
                                flat = FALSE
                                        )
                                    ),
                            column(6,
                                   uiOutput("verified")
                                   )
                            )
    )


)
