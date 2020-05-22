
server <- function(input,output,session) {
  addClass(selector = "body",class = "sidebar-collapse")
  observe({
    if(input$left_sidebar == "DMX") {
      shinyjs::addClass(selector = "aside.control-sidebar", class = "control-sidebar-open")
    }else{
      shinyjs::removeClass(selector = "aside.control-sidebar",class = "control-sidebar-open")
    }
  })
  
  output$xx <- renderUI(
    fluidRow(
      column(
        width = 9,
        align ="center",
        flipBox(
          id=1,
          tags$head(tags$style(HTML('.card{
                                     box-shadow: 0px 0px 20px 6px rgba(0,0,0,0.5);
            }'))),
          
          main_img = "HashingLogo.jpg",
          header_img = "",
          front_title = "",
          back_title =  tagList(tags$img(src = "/Hashimg.jpg",
                                         width="45px", height="45px")),  #########image 
          front_btn_text = "Flip to Execute Masking",
          back_btn_text = "Flip to Steps",
          fluidRow(
            box(title = "Initiate DMX Masking",
                
                #closable = TRUE,
                enable_label = TRUE,
                label_status = "danger",
                status = "primary",
                solidHeader = TRUE,
                #collapsible = TRUE,
                width = 10,
                #background = "navy",
                htmlOutput("status" )
                
            )
          ),
          
          back_content = tagList(
            fluidRow(box(
              title = "Click on Execute Masking to run the process",
              #closable = TRUE,
              enable_label = TRUE,
              label_status = "primary",
              status = "primary",
              solidHeader = TRUE,
              #collapsible = TRUE,
              width = 10,
              #background = "primary",
              #h3("\t One stop solution for Hashing"),
              #actionButton("action","Execute Masking",class="btn-primary"),
              
              #submitButton("")
            )),
            fluidRow(actionButton("action","Execute Masking",class="btn-primary")),
            
          )
        )
      )
    )
  )
  
  ########################
  
  output$DMX <- renderUI(
    fluidRow( 
      column(
        width = 9,
        align ="center",
        flipBox(
          id=2,
          width = 12,
          tags$head(tags$style(HTML('.card{
                                     box-shadow: 0px 0px 20px 6px rgba(0,0,0,0.5);}'))),
          align ="center",
          main_img =  "HashingLogo.jpg",
          header_img = "",
          front_title = "DMX: Data Masking",
          #back_title = "aaa",
          back_title = tagList(tags$img(src = "/Hashimg.jpg",
                                        width = "45px" ,height = "45px" )),  
          front_btn_text = "Flip to User Help",
          back_btn_text = "Flip to About DMX",
          fluidRow(
            box(title = "About DMX",
                #closable = TRUE,
                enable_label = TRUE,
                label_status = "danger",
                status = "primary",
                solidHeader = TRUE,
                #collapsible = TRUE,
                width = 10,
                #background = "navy",
                htmlOutput("statusbox1")
            )
          ),
          
          back_content = tagList(
            fluidRow(box(
              title = "Troubleshooting",
              #closable = TRUE,
              enable_label = TRUE,
              label_status = "primary",
              status = "primary",
              solidHeader = TRUE,
              #collapsible = TRUE,
              #background = "navy",
              width = 10,
              htmlOutput("statusbox2")
            )),
            
            
          )
        )
      )
    )
  )
  ###########################
  
  output$FT <- renderUI(
    fluidRow(
      column( 
        width = 9,
        align ="center",
        flipBox(
          id=3,
          tags$head(tags$style(HTML('.card{
                                     box-shadow: 0px 0px 20px 6px rgba(0,0,0,0.5);}'))),
          
          main_img = "HashingLogo.jpg",
          header_img = "",
          front_title = "",
          back_title =  tagList(tags$img(src="/Hashimg.jpg",
                                         width = "45px", height = "45px")),  ###############image 
          front_btn_text = "Driver Example",
          back_btn_text = "FileType",
          fluidRow(
            box(title = "FileType Defination",
                #closable = TRUE,
                enable_label = TRUE,
                label_status = "danger",
                status = "primary",
                solidHeader = TRUE,
                #collapsible = TRUE,
                width = 10,
                #background = "navy",
                tableOutput("FileType")
            )
          ),
          
          back_content = tagList(
            fluidRow(box(
              title = "Driver Example",
              #closable = TRUE,
              enable_label = TRUE,
              label_status = "primary",
              status = "primary",
              solidHeader = TRUE,
              #collapsible = TRUE,
              width = 10,
              #background = "navy",
              #tagList(tags$img(src = "Driver.JPG"))
              tagList(tags$img(src = "/Driver.JPG", width= "800px", height= "160px"))
            ))
            
          )
        )
      )
    )
  )
  #####################################
  
  output$Scheduler <- renderUI(
    fluidRow(
      column(
        width = 9,
        align ="center",
        flipBox(
          id=4,
          tags$head(tags$style(HTML('.card{
                                     box-shadow: 0px 0px 20px 6px rgba(0,0,0,0.5);
            }'))),
          
          main_img = "HashingLogo.jpg",
          header_img = "",
          front_title = "Scheduler",
          back_title =  tagList(tags$img(src = "/Hashimg.jpg",
                                         width="45px", height="45px")),  #########image 
          front_btn_text = "Scheduling instructions",
          back_btn_text = "Scheduler",
          
          fluidRow(
            column(width=2,actionButton("actionSch","Create Job",class="btn-primary")),column(width = 2,actionButton("actionCal","Calender",class="btn-primary")),
            column(width = 2, actionButton("actionPause","Pause Job",class="btn-primary")),column(width = 2, actionButton("actionPause","Delete Job",class="btn-primary")),column(width = 2, actionButton("actionPause","Execute Job",class="btn-primary"))),
          
          #fluidRow(column(width=3,actionButton("actionSch","Schedule Job",class="btn-primary")),column(width = 3,actionButton("actionCal","Calender",class="btn-primary"))),
          #fluidRow(actionButton("actionCal","Calender",class="btn-primary")),
          #fluidRow(actionButton("actionPause","PauseJob",class="btn-primary")),
          
          
          
          back_content = tagList(
            fluidRow(box(
              title = "Scheduling instructions",
              #closable = TRUE,
              enable_label = TRUE,
              label_status = "primary",
              status = "primary",
              solidHeader = TRUE,
              #collapsible = TRUE,
              width = 10,
              htmlOutput("SchInstruction")
              #background = "primary",
              #h3("\t One stop solution for Hashing"),
              #actionButton("action","Execute Masking",class="btn-primary"),
              
              #submitButton("")
            )),
            
            
          )
        )
      )
    )
  )
  ########################################################
  output$DriverUI <- renderUI(
    fluidPage(
      column(
        width = 10,
        align ="center",
        flipBox(
          id=5,
          tags$head(tags$style(HTML('.card{
                                     box-shadow: 0px 0px 20px 6px rgba(0,0,0,0.5);
            }'))),
          
          main_img = "HashingLogo.jpg",
          header_img = "",
          front_title = "Driver UI",
          back_title =  tagList(tags$img(src = "/Hashimg.jpg",
                                         width="45px", height="45px")),  #########image 
          front_btn_text = "Driver selection instructions",
          back_btn_text = "Driver UI",
          br(),
          
          tags$b("Choose drop down option to see file headers"),
          br(),
          br(),
          
          fluidRow( 
            
            column(
              width = 2, selectInput("headerlines", "Header",
                                     choices = c(
                                       One = 0,
                                       Two = 1,
                                       Three =2,
                                       Four=3,
                                       Five=4),
                                     selected = 0
                                     
              )
            ),
            
            #radioButtons("headerlines","HeaderLines",
            #choices = c(
            # One = 0,
            #Two = 1,
            #Three =2,
            #Four=3),inline=TRUE,
            #selected = 0),
            column(width = 2,
                   selectInput("sep", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t",
                                           PipeSeparated = "|"),
                               selected = ","
                               
                   )),
            
            column(width = 2,selectInput("quote", "Quote",
                                         choices = c(None = "",
                                                     "Double" = '"',
                                                     "Single" = "'"),
                                         selected = '"'
                                         
            )),
            
            
            # Horizontal line ----
            
            
            
            # Input: Select number of rows to display ----
            #radioButtons("disp", "Display",
            #choices = c(Head = "head",
            #All = "all"),inline=TRUE,#selected = "head")
            #div(style = "font-size: 14px; 'padding-left:200px; padding-right:1px; padding-top:-200px; padding-bottom:-100px', margin-top:-20em" , 
                
            fluidRow(#div(style = "font-size: 14px; 'padding-left:200px; padding-right:1px; padding-top:-200px; padding-bottom:-100px', margin-bottom:20em",
                  column(width = 6,fileInput("file1", "Upload File to be masked",
                                             multiple = TRUE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv"),buttonLabel = "Upload File",placeholder = "No file selected"))),
            
          ),
          
          #column(width = 3,h4("Field Names in File"),
          # Main panel for displaying outputs ----
          #mainPanel(
          # Output: Data file ----
          #tableOutput("contents"),
          
          
          #))
          
          
          #fluidRow(column (width=6)),
          #div(style = "font-size: 14px; padding: 0px 0px; margin-top:-400em", 
          
          fluidRow(
            column(width=6,uiOutput("dnd_values")),div(style = "font-size: 14px; padding: 0px 0px; margin-top:-100em", 
                   #br(),
                   column(width=4,div(style = "font-size: 14px; padding: 0px 0px; margin-top:-9em",
                          tags$b("File Specification to be filled by User"),
                          tags$head(tags$style(HTML('.h4, .h5, .h6, h4, h5, h6 {
    margin-top: 10px;
    margin-bottom: 10px;
    margin-left: -50px;
}'))),
                          ####              
                          #selectInput(
                          #inputId = 'select',
                          #label = 'Specifications',
                          #choices = LETTERS[1:2],
                          #selected = 'FileSpecification'
                          # ),div(style = "font-size: 14px; padding: 0px 0px; margin-top:-400em"
                          column(width=4,div(style = "font-size: 14px; padding: 0px 0px; margin-top:1em",
                                 rHandsontableOutput('rTable'),
                                 #verbatimTextOutput('textOuput')
                                 
                          ))))
                   
                   
                   
                   
                   
                   #fileInput("csvFile", "Upload file to be masked to select headers!"),
                   #tableOutput("rawData")
          )),
          
          #fluidRow(column (width=8))
          fluidRow(column(width = 7),
                   column(width=2,div(style = "font-size: 14px; padding: 0px 0px; margin-top:-16em; margin-left:-7em",
                          actionButton("actionSave","Add to Memory",class="btn-primary"),
                          br(),br(),fluidRow(width=2,
                                   downloadButton("actionExport", "Create Process",class="btn-primary"))
                          
                          ))),
         
          
          back_content = tagList(
            fluidRow(box(
              title = "Instructions to select Driver components",
              #closable = TRUE,
              enable_label = TRUE,
              label_status = "primary",
              status = "primary",
              solidHeader = TRUE,
              #collapsible = TRUE,
              width = 10,
              #background = "primary",
              htmlOutput("UIInstruction")
              #h3("\t One stop solution for Hashing"),
              #actionButton("action","Execute Masking",class="btn-primary"),
              
              #submitButton("")
            )),
            
            
          )
        )
      )
    )
  )
  
  
  ######################################
  observeEvent(input$action,
               {
                 shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
                 showModal(modalDialog("Please wait !! Hashing Process is in progress...", footer = "This dialog box will disappear on completion"))
                 system("cmd.exe",input = paste('"C://Program Files/R/R-3.6.3/bin/Rscript.exe" D://DMX_APP/DMX.R'))
                 removeModal()
               })
  #####################
  DF <- data.frame("FileType" = c('TypeA','TypeB','TypeC','TypeD'),"HeaderCount" = c('=<1','>1','>1','0'),"FooterCount"= c('0','>=1','0','>=1'))
  output$FileType <- renderTable(
    {DF}
  )
  
  ############################
  output$contents <- renderTable({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = TRUE)
  })
  #######################
  reactOnFile <- function(){
    if(!is.null(input$file1)){
      if(length(input$file1$datapath > 0)){
        print(input$file1$datapath)
        df <- read.csv(input$file1$datapath, header = TRUE, stringsAsFactors = FALSE, skip = input$headerlines, sep = input$sep, quote = input$quote)
        return(colnames(df))
      } else {
        return(list(c("")))
      }
    } else {
      return(list(c("")))
    }
  }
  
  output$dnd_values <- renderUI({
    fluidRow(div(style = "font-size: 14px; padding: 0px 0px; margin-top:-10em", 
      column(
        #tags$b("Field names in file"),
        width = 12,
        bucket_list(
          header = tags$b("Drag the column names to box which needs to be masked"),
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "Header display",
            labels = reactOnFile(),
            input_id = "rank_list_1"
          ),
          add_rank_list(
            text = "Masked Columns",
            labels = NULL,
            input_id = "rank_list_2"
          )
        ))))
  })
  
  #output$contents <- renderTable({
  #req(reactOnFile())
  #reactOnFile()
  #})
  
  output$status <- renderText({ 
    paste0("Step 1: Create the DMX Driver by following steps <br/>
              Step 2: Complete the following fields in the UI (Check fileType for an example)    <br/>
                   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- FileType                                                                        <br/>
                  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Number of Headers                                                               <br/>
                   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Footer                                                                          <br/>
                   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Line Separator                                                                  <br/>
                   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Input File Header Names (Pipe Separated)                                        <br/>
                   &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- Column names to be masked (Pipe Separated )                                     <br/>
  Step 3: Place the original files to be masked in the same location as the DMX driver sheet        <br/>
  Step 4: Ensure Driver fields are filled properly and correct file generated (Check driver sheet example under FileType)                              <br/>
  Step 5: Click on the Execute Masking button                                                              <br/>
  Step 6: Retrieve the masked files from the same location as the DMX Driver Sheet on your computer ")
    
    
  })
  
  #######################
  output$statusbox1 <- renderText(
    {
      paste0("DMX is a business oriented tool designed to mask sensitive data in a fast, cost effective manner. The resulting sanitised data can then be safely used for analysis tasks within business or IT functions and locations that are outside the normal scope of where the sensitive data would otherwise be allowed.<br/></br/>
  
  DMX uses a custom algorithmic process which keeps the integrity of any required linkages between data, but anonymises the sensitive data fields. This means the DMX masked data set can be analysed and utilised as if it was the original, only without the risk of exposing the original sensitive data to unauthorised users.")
    }
  )
  
  ###########################
  
  output$statusbox2 <- renderText({
    paste0("Issue: Masked files are not processed <br/>
  Solution: <br/> 
  1. Check DMX Driver sheet has been correctly filled out (check on FileType tab for example) <br/>
  2. Check DMX Driver File and original files are all saved in the same specified location on your computer<br/>
  3. Remove old masked files or old original files from file location before executiing the masking process 
    
    ")
  })
  
  ###############################
  
  rTable_content <- reactive(
    {
      #if (input$select == 'A')#{
      DF <- data.frame(
        #'Type' = paste0('Row', 1:5),
        'Type' =c('fileType','colSeparator','header','footer','lineSeparator','fileExtension','fileName'),
        'Value' = rep('', 7),
        stringsAsFactors = FALSE
      )
      
      # get row and colnames in order
      #colnames(t_DF) <- rownames(DF)
      #rownames(t_DF) <- colnames(DF)
      #DF = data.frame(matrix(NA, nrow = ncol(DF), ncol = nrow(DF)))
      #} #else {
      #DF <- data.frame(
      #'Type' =c('fileType','ColSeparator','header','footer','lineSeparator','fileExtension','fileName'),
      #'Value' = rep('', 7),
      #stringsAsFactors = FALSE
      #)
      # }
      
      # Try to keep previously entered custom value for match Type's
      if (length(input$rTable) > 0){
        oDF <- hot_to_r(input$rTable)
        #oDF <- transpose(oDF)
        DF <- merge(DF, oDF, by = 'Type', all.x = TRUE)
        DF$Value <- ifelse(is.na(DF[,3]), DF[,2], DF[,3])
        DF <- DF[, c(1, ncol(DF))]
        
      }
      
      DF
    }
  )
  
  output$rTable <- renderRHandsontable({
    rhandsontable(
      data = rTable_content(),
      rowHeaders = NULL,
      contextMenu = FALSE,
      width = 1200,
      height = 500
      
      
    )
  })
  output$textOuput <- renderText({
    paste(
      unlist(hot_to_r(input$rTable)),
      collapse = '; '
    )
  }) 
  #####################################
  
  
  observeEvent(input$actionSave,
               {
                 shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")
                 showModal(modalDialog("Please wait !! Data saving is in progress...", footer = "This dialog box will disappear on completion"))
                 TBContent= rTable_content()
                 TB_Content = data.frame(matrix(NA, nrow = 1, ncol = nrow(TBContent)))
                 colnames(TB_Content) <- TBContent$Type
                 TB_Content[1,] <- TBContent$Value
                 TB_Content$cols <- c(paste(reactOnFile(), collapse="|"))
                 TB_Content$Hash <- c(paste(input$rank_list_2, collapse="|"))
                 #  NOTE: This will only work for a maximum of 2 rows
                 # TODO: Use rbind to extend feature to allow more rows
                 #TB_Content[complete.cases(TB_Content),]
                 existingDataframe <<- TB_Content
                 
                 removeModal()
               }
  )
  
  
  output$actionExport <- downloadHandler(
    filename = function() {
      paste("Driver", ".csv", sep = "") 
    }, 
    content = function(file){
      TBContent= rTable_content()
      TB_Content = data.frame(matrix(NA, nrow = 1, ncol = nrow(TBContent)))
      colnames(TB_Content) <- TBContent$Type
      TB_Content[1,] <- TBContent$Value
      TB_Content$cols <- c(paste(reactOnFile(), collapse="|"))
      TB_Content$Hash <- c(paste(input$rank_list_2, collapse="|"))
      if( nrow(existingDataframe) > 0){
        TB_Content <- rbind(TB_Content, existingDataframe);
      }
      existingDataframe <- data.frame()
      #arrange(Test, rev(rownames(Test)))
      #Test1 = as.data.frame(reactOnFile())
      #Test1= (reactOnFile())
      #Test2 = merge(Test,Test1)
      #if (file.exists(file)) 
        #return (write.csv(TB_Content, file, row.names = FALSE,quote=TRUE,append = TRUE,col.names = FALSE))
      #else 
     # TB_Content[complete.cases(TB_Content),]
        return(write.csv(TB_Content, file, row.names = FALSE,quote=TRUE))
      #write(TB_Content,"D:\\DMX_APP\\www\\DriverTest.csv",append = FALSE,quote = FALSE,sep = ",",eol = "\r\n",dec=".",row.names = FALSE,col.names = FALSE)
    })
  
  ####################
  
  ####
  
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  
  rawData <- eventReactive(input$csvFile, {
    read.csv(input$csvFile$datapath)
  })
  
  output$rawData <- renderTable({
    rawData() %>% colnames()
  })
  
}
