library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(vcd)
library(cluster)
library(pROC)
library(caret)

ui<-dashboardPage(
  dashboardHeader(title='Online ML'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Descriptive Statistics Analysis',tabName = 'tab1'),
      menuItem(
        text = 'Regression',
        menuSubItem('Linear Regresson',tabName = 'tab2')
      ),
      menuItem(
        text = 'Classification',
        menuSubItem('GLM',tabName = 'tab3'),
        menuSubItem('KNN',tabName = 'tab6')
      ),
      menuItem(
        text = 'Clustering',
        menuSubItem('KMeans Clustering',tabName = 'tab4'),
        menuSubItem('KMeans++ Clustering',tabName = 'tab5')
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'tab1',
        fluidRow(
          box(
            width = 3,
            fileInput('file1',label = 'Upload a CSV file')
          ),
          box(
            width = 3,
            textInput(
              'text1',
              label = 'Please input variable name(s) you want to analyze'
            ),
            actionButton('action1','Submit')
          ),
          box(
            width = 3,
            selectInput(
              'choice2',
              label = 'Choose one Variable',
              choices = NULL
            )
          )
        ),
        fluidRow(
          box(
            title = 'Data Preview',
            width = 12,
            collapsible = T,
            collapsed = T,
            dataTableOutput('table1')
          )
        ),
        fluidRow(
          box(
            title = 'Summary Data (Numeric)',
            width = 5,
            collapsible = T,
            collapsed = T,
            dataTableOutput('table2')
          ),
          box(
            title = 'Summary Data (Categorical)',
            width = 4,
            collapsible = T,
            collapsed = T,
            dataTableOutput('table3')
          ),
          box(
            title = 'Count',
            width = 3,
            collapsible = T,
            collapsed = T,
            dataTableOutput('table4')
          )
        ),
        fluidRow(
          box(
            title = 'Distribution',
            width = 6,
            collapsible = T,
            plotOutput('plot1')
          ),
          box(
            title = 'Boxplot',
            width = 3,
            plotOutput('plot2'),
            collapsible = T
          ),
          box(
            title = 'Controller',
            width = 3,
            selectInput(
              'choice1',
              label = 'Choose one Variable',
              choices = NULL
              
            ),
            sliderInput(
              'slider1',
              label = 'Control range of Xlim',
              min = 1,max = 100,value=c(1,100)
            )
          )
        )
      ),
      tabItem(
        tabName = 'tab2',
        fluidRow(
          box(
            title = 'Training',
            width=3,
            fileInput('file2',label = 'Upload a file'),
            selectInput(
              'choice3',
              label = 'Choose x variable(s)',
              choices = NULL,
              multiple = T
            ),
            selectInput(
              'choice4',
              label = 'Choose y variable',
              choices = NULL
            ),
            actionButton(
              'action2',
              label = 'Train model'
            )
          ),
          box(
            title = 'Model Summary',
            width = 6,
            collapsible = T,
            verbatimTextOutput(
              'print1'
            )
          ),
          box(
            width = 3,
            selectInput(
              'choice6',
              label = 'Choose statistics',
              choices = c('coefficients','residuals','effects','rank','fitted.values','df.residual','call','model'),
              selected = NULL
            ),
            verbatimTextOutput(
              'print3'
            )
          )
        ),
        fluidRow(
          box(
            title = 'Prediction',
            width = 3,
            fileInput('file3',label = 'Upload a file'),
            selectInput(
              'choice5',
              label = 'Choose x variable(s)',
              choices = NULL,
              multiple = T
            ),
            actionButton('action3',label = 'Predict')
           ),
          box(
            title = tags$div(
              'Prediction Values',
              style = "display: flex; align-items: center; justify-content: space-between;",
              downloadButton('download1','Download result',id='download1',style='margin-left:20px')
            ),
            width = 9,
            collapsible = T,
            verbatimTextOutput('print2')
          ),
          box(
            width = 9,
            collapsible = T,
            title = 'Prediction Interval',
            column(
              width = 3,
              selectInput(
                'choice7',
                label='Alpha',
                choices = c(0.95,0.99,0.9)
              ),
              selectInput(
                'choice8',
                label='Interval Type',
                choices = c('confidence','prediction')
              )
            ),
            column(
              width = 9,
              verbatimTextOutput('print4')
            )
          )
        )
      ),
      tabItem(
        tabName = 'tab3',
        h3("Logistic Regression"),
        br(),
        fluidRow(
          box(
            title = 'Training',
            width = 3,
            fileInput('glm_f1','Upload a file'),
            selectInput(
              'glm_c1',
              label = 'Choose x variable(s)',
              choices = NULL,
              multiple = T
            ),
            selectInput(
              'glm_c2',
              label = 'Choose y variable',
              choices = NULL
            ),
            actionButton(
              'glm_a1',
              label = 'Train Model'
            )
          ),
          box(
            title = 'Model Summary',
            width = 7,
            collapsible = T,
            verbatimTextOutput('glm_p1')
          )
        ),
        fluidRow(
          box(
            title = 'Prediction',
            width = 3,
            fileInput('glm_f2','Upload a file'),
            selectInput('glm_c3','Alpha',choices = c(0.05,0.01,0.1),selected = 0.05),
            actionButton('glm_a2','Predict'),
            downloadButton('glm_d1','Download Result')
          ),
          box(
            title = 'Output',
            collapsible = T,
            width = 9,
            box(
              title = 'Point Estimation',
              width = 12,
              verbatimTextOutput('glm_p2')
            ),
            box(
              title = 'Interval Estimation',
              width = 12,
              verbatimTextOutput('glm_p3')
            )
          )
        ),
        p("Please bear in mind that ROC, AUC only work when real y exist." ,style='font-size:19px;'),
        fluidRow(
          box(
            width = 4,
            selectInput('glm_c6','Choose real y',choices = NULL),
            actionButton('glm_a5','Submit',class='btn',style = 'margin-bottom:10px;'),
            verbatimTextOutput('glm_p6')
          ),
          box(
            width = 4,
            collapsible = T,
            plotOutput('glm_plot1')
          ),
          box(
            width = 4,
            collapsible = T,
            plotOutput('glm_plot2')
          )
        ),
        h3("Poission Regression"),
        br(),
        p('Alternatively, when a binomial distribution B(n.p) has significantly 
          large n and small p, the binomial distribution is approximated by pois(np).
          Hence, you may consider a log linear regression with Poisson response (Poisson Regression).
          However, the first thing you need to ensure is that the data are 
          count data and follow the poisson distribution.',style='font-size:19px;'),
        fluidRow(
          box(
            title = 'Training',
            width = 3,
            fileInput('glm_f3','Upload a file'),
            selectInput('glm_c4','Select x variable(s)',choices = NULL,multiple = T),
            textInput('glm_t1','Offset?',placeholder = 'Type Yes or No'),
            selectInput('glm_c5','Select y variable',choices = NULL),
            actionButton('glm_a3','Train')
          ),
          box(
            title = 'Model Summary',
            width = 9,
            collapsible = T,
            verbatimTextOutput('glm_p4')
          )
        ),
        fluidRow(
          box(
            title = 'Prediction',
            width = 3,
            fileInput('glm_f4','Upload a file'),
            actionButton('glm_a4','Predict')
          ),
          box(
            title = 'Output',
            width = 9,
            collapsible = T,
            verbatimTextOutput('glm_p5')
          )
        )
      ),
      
      tabItem(
        tabName = 'tab4',
        navbarPage(
          'Step',
          tabPanel(
            'Tune k',
            sidebarLayout(
              sidebarPanel(
                width=3,
                fileInput(
                  'kmeansf1',
                  label = 'Upload a file'
                ),
                textInput(
                  'kmeanst1',
                  'Type initating k',
                  value = 2
                ),
                textInput(
                  'kmeanst2',
                  'Type ending k',
                  value = 10
                ),
                actionButton('kmeansa1',label = 'Submit')
              ),
              mainPanel(
                plotOutput('kmeansplot1'),
                br(),
                verbatimTextOutput('kmeansp1')
              )
            )
          ),
          tabPanel(
            title='Cluster',
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  width=3,
                  textInput('kmeanst3','Set k'),
                  textInput('kmeanst4','Set nstart',value = 25),
                  actionButton('kmeansa2',label = 'Submit'),
                  downloadButton('kmeansdown1','Download Result',class='btn',style = 'margin-top:10px;')
                ),
                mainPanel(
                  verbatimTextOutput('kmeansp2')
                )
              ),
              sidebarLayout(
                sidebarPanel(
                  width=3,
                  textInput('kt1','Type x axis'),
                  textInput('kt2','Type y axis'),
                  actionButton('kmeansa3',label = 'Submit')
                ),
                mainPanel(
                  plotOutput('kmeansplot2')
                )
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'tab5',
        navbarPage(
          'Step',
          tabPanel(
            'Tune k',
            sidebarLayout(
              sidebarPanel(
                width=3,
                fileInput(
                  'kppf1',
                  label = 'Upload a file'
                ),
                textInput(
                  'kppt1',
                  'Type initating k',
                  value = 2
                ),
                textInput(
                  'kppt2',
                  'Type ending k',
                  value = 10
                ),
                actionButton('kppa1',label = 'Submit')
              ),
              mainPanel(
                plotOutput('kppplot1'),
                br(),
                verbatimTextOutput('kppp1')
              )
            )
          ),
          tabPanel(
            title='Cluster',
            fluidPage(
              sidebarLayout(
                sidebarPanel(
                  width=3,
                  textInput('kppt3','Set k'),
                  actionButton('kppa2',label = 'Submit'),
                  downloadButton('kppdown1','Download Result',class='btn',style = 'margin-top:10px;')
                ),
                mainPanel(
                  verbatimTextOutput('kppp2')
                )
              ),
              sidebarLayout(
                sidebarPanel(
                  width=3,
                  textInput('kp1','Type x axis'),
                  textInput('kp2','Type y axis'),
                  actionButton('kppa3',label = 'Submit')
                ),
                mainPanel(
                  plotOutput('kppplot2')
                )
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'tab6',
        navbarPage(
          title='Step',
          tabPanel(
            title='Tune k',
            sidebarLayout(
              sidebarPanel(
                width=3,
                fileInput('knn_f3',label='Upload training set'),
                textInput('knn_t2',label='K folds',value = 10),
                textInput('knn_t3',label='K-neighbor begin',value = 2),
                textInput('knn_t4',label='K-neighbor end',value = 10),
                selectInput('knn_c4',label = 'Choose xtrain',choices = NULL,multiple = T),
                selectInput('knn_c5',label = 'Choose ytrain',choices = NULL),
                actionButton('knn_a2',label='Submit')
              ),
              mainPanel(
                verbatimTextOutput('knn_p2')
              )
            )
          ),
          tabPanel(
            title='Training & Prediction',
            sidebarLayout(
              sidebarPanel(
                width=3,
                fileInput('knn_f1',label='Upload training set'),
                fileInput('knn_f2',label='Upload predicting set'),
                textInput('knn_t1',label='Type k',value = 6),
                selectInput('knn_c1',label = 'Choose xtrain',choices = NULL,multiple = T),
                selectInput('knn_c2',label = 'Choose ytrain',choices = NULL),
                selectInput('knn_c3',label = 'Choose xpred',choices = NULL,multiple = T),
                actionButton('knn_a1',label='Submit'),
                downloadButton('knn_d1',label = 'Download Result')
              ),
              mainPanel(
                verbatimTextOutput('knn_p1')
              )
            ),
            p("Please bear in mind that ROC, AUC only work when real y exist." ,style='font-size:19px;'),
            sidebarLayout(
              sidebarPanel(
                width = 3,
                selectInput('knn_c6','Choose real y',choices = NULL),
                actionButton('knn_a3',label='Submit',style = 'margin-bottom:10px;'),
                verbatimTextOutput('knn_p3')
              ),
              mainPanel(
                fluidRow(
                  column(
                    width=6,
                    plotOutput('knn_plot1')
                  ),
                  column(
                    width=6,
                    plotOutput('knn_plot2')
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)