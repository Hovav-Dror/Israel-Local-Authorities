

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(shinythemes)
library(shinyBS)

load("MunicipalData.rda")

Pop_and_Physical2021 <- Pop_and_Physical2021 %>% select(-"כללי: שם ועדת תכנון ובנייה")
Pop_and_Physical2021 <- Pop_and_Physical2021 %>% 
  mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-20` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 29-20` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 44-30` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`,
         .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 17-0`) %>% 
  mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 45 ומעלה` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`, .before = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>%
  relocate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 75 ומעלה`, .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`)
Names1 <- names(Pop_and_Physical2021)
suppressWarnings(
  Combined <- bind_rows(
  Pop_and_Physical2021 %>% 
    mutate(across(-`שם הרשות`, as.numeric)) %>% 
    pivot_longer(-`שם הרשות`) %>% 
    mutate(Year = 2021) %>% 
    drop_na(),
  Pop_and_Physical2020 %>% 
    select(any_of(Names1)) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-20` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 29-20` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 44-30` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`,
           .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 17-0`) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 45 ומעלה` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`, .before = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>%
    relocate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 75 ומעלה`, .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    mutate(across(-`שם הרשות`, as.numeric)) %>% 
    pivot_longer(-`שם הרשות`) %>% 
    mutate(Year = 2020) %>% 
    drop_na(),
  Pop_and_Physical2019 %>% 
    select(any_of(Names1)) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-20` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 29-20` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 44-30` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`,
           .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 17-0`) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 45 ומעלה` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`, .before = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    relocate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 75 ומעלה`, .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    mutate(across(-`שם הרשות`, as.numeric)) %>% 
    pivot_longer(-`שם הרשות`) %>% 
    mutate(Year = 2019) %>% 
    drop_na(),
  Pop_and_Physical2018 %>% 
    select(any_of(Names1)) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-20` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 29-20` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 44-30` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`,
           .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 17-0`) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 45 ומעלה` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`, .before = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    relocate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 75 ומעלה`, .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    mutate(across(-`שם הרשות`, as.numeric)) %>% 
    pivot_longer(-`שם הרשות`) %>% 
    mutate(Year = 2018) %>% 
    drop_na(),
  Pop_and_Physical2017 %>% 
    select(any_of(Names1)) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-20` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 29-20` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 44-30` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`,
           .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 17-0`) %>% 
    mutate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 45 ומעלה` = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 59-45` + `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 64-60`, .before = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>%
    relocate(`דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 75 ומעלה`, .after = `דמוגרפיה: אחוז באוכלוסייה בסוף השנה בני 65 ומעלה`) %>% 
    mutate(across(-`שם הרשות`, as.numeric)) %>% 
    pivot_longer(-`שם הרשות`) %>% 
    mutate(Year = 2017) %>% 
    drop_na(),
)
)

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cerulean"),
                #tags$head(includeHTML(("google-analytics.html"))),
                #tags$style(type="text/css", "body {padding-top: 70px;}"),
                navbarPage("קובץ רשויות מקומיות", id = "NavBar", position = "fixed-top", selected = "נתוני 2021",
                           tabPanel("הקדמה",
                                    fluidPage(lang = "he",
                                              tags$style(
                                                HTML("
                                      .rtl {
                                        direction: rtl;
                                      }
                                      .formatted-text {
                                        color: blue;
                                        font-size: 16px;
                                      }
                                      .emphasis {
                                        font-weight: bold;
                                        color: red;
                                        font-size: 17px;
                                      }

                                       .center-align {
                                         display: flex;
                                         flex-direction: column;
                                         align-items: center;
                                         justify-content: center;
                                         text-align: center;
                                       }
                                    ")
                                              ),
                                              h4(),
                                              p(), p(), p(), p(),
                                              hr(),
                                              HTML(" <br>"),
                                              HTML("<div class='rtl'><span class='emphasis'>קובץ הרשויות המקומיות בישראל - 2021</div>"),
                                              p(), p(),
                                              HTML("<div class='rtl'><span class='formatted-text'>הלמ\"ס <a href = 'https://www.cbs.gov.il/he/mediarelease/Pages/2023/%D7%A7%D7%95%D7%91%D7%A5-%D7%94%D7%A8%D7%A9%D7%95%D7%99%D7%95%D7%AA-%D7%94%D7%9E%D7%A7%D7%95%D7%9E%D7%99%D7%95%D7%AA-%D7%91%D7%99%D7%A9%D7%A8%D7%90%D7%9C-2021.aspx'  target='_blank'>פרסמו את קובץ הרשויות המקומיות</a> ויש בו המון מידע.</div>"),
                                              HTML("<div class='rtl'><span class='formatted-text'>הכנתי לעצמי משהו שמקל מעט על בחינת חלק מהנתונים שניתן למצוא שם. אז אשתף גם אתכם, אם אתם מחסידי הז'אנר.</div>"),
                                              HTML("<div class='rtl'><span class='formatted-text'>(חוץ מזה פה ושם מוסיף נתונים מקבצי למס אחרים או מקורות נוספים, ואז מידע יופיע בצד בהערות)</div>"),
                                              HTML("<div class='rtl'><span class='formatted-text'>מתאים לשימוש במחשב, לא מהנייד.</div>"),
                                              HTML("<div class='rtl'><span class='formatted-text'>השרת ממוקם כאן: <a href = 'https://numbersguys.com'  target='_blank'>https://numbersguys.com</a> ותוכלו למצוא שם גם הנגשה של נתוני גמלנט, ועוד קצת.</div>"),
                                              HTML("<div class='rtl'><span class='formatted-text'>hovav@hotmail.com - אם שימש אתכם, אשמח לשמוע</div>"),
                                              p(),
                                              hr()
                                    ),
                           ),
                           
                           # tabPanel גרף ------------------------------------------------------------
                           tabPanel("נתוני 2021",
                                    
                                    fluidPage(
                                      hr(),
                                      p(),p(),p(),p(),p(),p(),p(),p(),
                                      fluidRow(),
                                      fluidRow(),
                                      h4(" "),
                                      
                                      
                                      hr(),
                                      fluidRow(
                                        column(1),
                                        column(2, pickerInput("xaxis1", "x-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "שכר ורווחה: אחוז העצמאים המשתכרים עד מחצית השכר הממוצע"  , options = pickerOptions(liveSearch = T))),
                                        column(1, tipify(checkboxInput(inputId = "PopAdjustX", label = "תקנון לאוכלוסיה", value = FALSE), "תקנון ל 1000 אנשים")),
                                        column(1),
                                        column(2, pickerInput("yaxis1", "y-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "חינוך והשכלה: אחוז זכאים לתעודת בגרות מבין תלמידי כיתות יב"  , options = pickerOptions(liveSearch = T))),
                                        column(1, tipify(checkboxInput(inputId = "PopAdjustY", label = "תקנון לאוכלוסיה", value = FALSE), "תקנון ל 1000 אנשים")),
                                        #column(2, pickerInput("y-axis", "yaxis1", choices = names(Pop_and_Physical2021), selected = " צפיפות_אוכלוסייה_לקמר_ביישובים_שמנו_5_000_תושבים_ויותר"  )),
                                        column(2,
                                               # tipify(
                                               #   materialSwitch(
                                               #   inputId = "BarPlot",
                                               #   label = "מיפוי משתנה בודד",
                                               #   value = FALSE,
                                               #   status = "primary"
                                               # ),
                                               # "בלבד Y גרף עמודות, המשתמש בציר"),
                                               tipify(
                                               radioGroupButtons(
                                                 inputId = "BarPlot",
                                                 
                                                 label = "סוג תרשים",
                                                 choices = c("Scatter",
                                                             "Bar", "Group", "Boxplot"),
                                                 justified = TRUE,
                                                 checkIcon = list(
                                                   yes = icon("ok",
                                                              lib = "glyphicon"))
                                               ),
                                               HTML("שתי האפשרויות הראשונות מתאימות להשוואה בין ערים<br>למשל לראות באיזה ערים יש אחוז אבטלה גבוה, כתלות באחוז השמנה<br><br>שתי האפשרויות האחרונות מתאימות להשוואה בין משתנים, ללא פרוט לעיר ספציפית<br>למשל מה התפלגות מספר המורים כתלות באשכול כלכלי-חברתי")),
                                               
                                        ),
                                        
                                      ), # fluidRow
                                      fluidRow(
                                        column(1),
                                        column(2, pickerInput("size1", "Size", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה"
                                                              , options = pickerOptions(liveSearch = T))),
                                        column(2),
                                        column(2, pickerInput("color1", "Color", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "none" , options = pickerOptions(liveSearch = T) )),
                                      ),
                                      #p(),p(),
                                      #fluidRow(uiOutput("Comments1")),
                                      fluidRow(
                                        column(7,
                                               div(
                                                 #style = "display: flex; align-items: center; justify-content: center; min-height: 800px; padding-bottom: 000px; padding-top: 0px;",
                                                 #style = "display: flex; align-items: center; justify-content: center; min-height: 1000px; padding-bottom: 100px; padding-top: 00px;margin-top: -250px;",
                                                 style = "display: flex; align-items: center; justify-content: center; padding-bottom: 100px; padding-top: 0px;",
                                                 uiOutput("EDAxyPlot"),
                                               )),
                                        column(5, div(
                                          style = "display: flex; align-items: center; justify-content: center; min-height: 800px; padding-bottom: 100px; padding-top: 00px;",
                                          uiOutput("Comments1")))
                                      ),
                                      
                                      
                                      p(),p(),p(),
                                      hr(),
                                      
                                      # מסננים ותוספות ----------------------------------------------------------
                                      
                                      
                                      h4("מסננים ותוספות"),
                                      fluidRow(
                                        column(2),
                                        column(1, checkboxInput(inputId = "AddDiagLine", label = "הוספת קו שוויון", value = FALSE)),
                                        column(1, checkboxInput(inputId = "AddTrendLine", label = "הוספת קו מגמה", value = FALSE)),
                                        column(1, checkboxInput(inputId = "AddHorizontalLine", label = "הוספת קו אופקי", value = FALSE)),
                                        column(1, numericInput("Horizontal0", "", value = 0, width = "50%")),
                                        column(1, checkboxInput(inputId = "AddVertiaclLine", label = "הוספת קו אנכי", value = FALSE)),
                                        column(1, numericInput("Vertical0", "", value = 0, width = "50%")),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2,
                                               pickerInput(inputId = "Topics", label = "סינון נושאים", 
                                                           choices = names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           selected =names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           #selected = c("דמוגרפיה", "בריאות", "חינוך והשכלה", "מדד חברתי-כלכלי", "מתוך סקר הוצאות  משקי הבית" , "מתוך סקר כוח אדם", "שכר ורווחה", "תחבורה"),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                        #column(1),
                                        column(2,
                                               pickerInput(inputId = "towns", label = "סינון ישובים", 
                                                           choices = Pop_and_Physical2021 %>% pull(1),
                                                           selected = Pop_and_Physical2021 %>% pull(1),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                        column(2,
                                               pickerInput(inputId = "HighlightTowns", label = "ישובים להבליט בכתום", 
                                                           choices = Pop_and_Physical2021 %>% pull(1),
                                                           selected = NULL,
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2,
                                               # sliderInput(inputId = "TownSizeSlider", label = "מספר תושבים בישוב", 
                                               #             min = min(Pop_and_Physical2021$`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T), max = max(Pop_and_Physical2021$`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T), 
                                               #             value = c(1000, 1000000), log = TRUE,
                                               # )
                                               sliderTextInput(
                                                 inputId = "TownSizeSlider",
                                                 label = "מספר תושבים בישוב", 
                                                 choices = c(1000, 5000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                                                 grid = TRUE,selected = c(1000, 1000000), 
                                               )
                                        ),
                                        column(2),
                                        column(2,
                                               pickerInput(inputId = "AdjustPopBy", label = "בתקנון לאוכלוסיה", 
                                                           choices = Names1[str_detect(Names1, "ייה בסוף השנה")],
                                                           selected = Names1[str_detect(Names1, "ייה בסוף השנה")][1],
                                                           #options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = FALSE
                                               )),
                                      ),
                                      fluidRow(
                                        column(2),
                                        
                                        column(2, sliderInput("Eshkol", "אשכול חברתי-כלכלי", min = 1, max = 10, value = c(1,10))),
                                        column(2, sliderInput("Coalition", "אחוז הצבעה לקואליציה", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("Opposition", "אחוז הצבעה לאופוזיציה", min = 0, max = 100, value = c(0,100))),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2, sliderInput("Religious", "אחוז הצבעה למפלגות דתיות", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("UltraReligious", "אחוז חרדים", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("Arabs", "אחוז ערבים", min = 0, max = 100, value = c(0,100))),
                                      ),
                                      # column(2, sliderInput("Opposition", "אחוז הצבעה לאופוזיציה", min = 0, max = 100, value = c(0,100))),
                                      # column(2, pickerInput("size2", "Size", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה"
                                      #                       , options = pickerOptions(liveSearch = T))),
                                      # column(2),
                                      # column(2, pickerInput("color2", "Color", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "none" , options = pickerOptions(liveSearch = T) )),
                                      #),
                                      
                                      
                                    ) # fluidPage
                           ), # tabPanel 1
                           
                           # TabPanel Previous Years -------------------------------------------------
                           tabPanel("Previous Years",

                                    fluidPage(
                                      hr(),
                                      p(),p(),p(),p(),p(),p(),p(),p(),
                                      fluidRow(),
                                      fluidRow(),
                                      h4(" "),
                                      
                                      
                                      hr(),
                                      fluidRow(
                                        column(1),
                                        column(2, pickerInput("xaxisB1", "x-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "שכר ורווחה: אחוז העצמאים המשתכרים עד מחצית השכר הממוצע"  , options = pickerOptions(liveSearch = T))),
                                        column(1, tipify(checkboxInput(inputId = "PopAdjustBX", label = "תקנון לאוכלוסיה", value = FALSE), "תקנון ל 1000 אנשים")),
                                        column(1),
                                        column(2, pickerInput("yaxisB1", "y-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "חינוך והשכלה: אחוז זכאים לתעודת בגרות מבין תלמידי כיתות יב"  , options = pickerOptions(liveSearch = T))),
                                        column(1, tipify(checkboxInput(inputId = "PopAdjustBY", label = "תקנון לאוכלוסיה", value = FALSE), "תקנון ל 1000 אנשים")),
                                        #column(2, pickerInput("y-axis", "yaxis1", choices = names(Pop_and_Physical2021), selected = " צפיפות_אוכלוסייה_לקמר_ביישובים_שמנו_5_000_תושבים_ויותר"  )),
                                        column(2,
                                               # tipify(
                                               #   materialSwitch(
                                               #   inputId = "BarPlot",
                                               #   label = "מיפוי משתנה בודד",
                                               #   value = FALSE,
                                               #   status = "primary"
                                               # ),
                                               # "בלבד Y גרף עמודות, המשתמש בציר"),
                                               tipify(
                                                 radioGroupButtons(
                                                   inputId = "BarPlotB",
                                                   
                                                   label = "סוג תרשים",
                                                   choices = c(
                                                     "Scatter","Bar", 
                                                     "Group", "Boxplot"),
                                                   selected = "Bar",
                                                   justified = TRUE,
                                                   checkIcon = list(
                                                     yes = icon("ok",
                                                                lib = "glyphicon"))
                                                 ),
                                                 HTML("שתי האפשרויות הראשונות מתאימות להשוואה בין ערים<br>למשל לראות באיזה ערים יש אחוז אבטלה גבוה, כתלות באחוז השמנה<br><br>שתי האפשרויות האחרונות מתאימות להשוואה בין משתנים, ללא פרוט לעיר ספציפית<br>למשל מה התפלגות מספר המורים כתלות באשכול כלכלי-חברתי")),
                                               
                                        ),
                                        
                                      ), # fluidRow
                                      fluidRow(
                                        column(1),
                                        column(2, pickerInput("sizeB1", "Size", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה"
                                                              , options = pickerOptions(liveSearch = T))),
                                        column(2),
                                        column(2, pickerInput("colorB1", "Color", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "none" , options = pickerOptions(liveSearch = T) )),
                                      ),
                                      #p(),p(),
                                      #fluidRow(uiOutput("Comments1")),
                                      fluidRow(
                                        column(7,
                                               div(
                                                 #style = "display: flex; align-items: center; justify-content: center; min-height: 800px; padding-bottom: 000px; padding-top: 0px;",
                                                 #style = "display: flex; align-items: center; justify-content: center; min-height: 1000px; padding-bottom: 100px; padding-top: 00px;margin-top: -250px;",
                                                 style = "display: flex; align-items: center; justify-content: center; padding-bottom: 100px; padding-top: 0px;",
                                                 uiOutput("EDAxyPlotB"),
                                               )),
                                        column(5, div(
                                          style = "display: flex; align-items: center; justify-content: center; min-height: 800px; padding-bottom: 100px; padding-top: 00px;",
                                          uiOutput("Comments1B")))
                                      ),
                                      
                                      
                                      p(),p(),p(),
                                      hr(),
                                      
                                      # מסננים ותוספות ----------------------------------------------------------
                                      
                                      
                                      h4("מסננים ותוספות"),
                                      fluidRow(
                                        column(2),
                                        column(1, checkboxInput(inputId = "AddDiagLineB", label = "הוספת קו שוויון", value = FALSE)),
                                        column(1, checkboxInput(inputId = "AddTrendLineB", label = "הוספת קו מגמה", value = FALSE)),
                                        column(1, checkboxInput(inputId = "AddHorizontalLineB", label = "הוספת קו אופקי", value = FALSE)),
                                        column(1, numericInput("Horizontal0B", "", value = 0, width = "50%")),
                                        column(1, checkboxInput(inputId = "AddVertiaclLineB", label = "הוספת קו אנכי", value = FALSE)),
                                        column(1, numericInput("Vertical0B", "", value = 0, width = "50%")),
                                        # column(2,
                                        #        pickerInput(inputId = "HighlightTownsB", label = "ישובים להבליט בכתום", 
                                        #                    choices = Pop_and_Physical2021 %>% pull(1),
                                        #                    selected = NULL,
                                        #                    options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                        #                    multiple = TRUE
                                        #        )),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2,
                                               pickerInput(inputId = "TopicsB", label = "סינון נושאים", 
                                                           choices = names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           selected =names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           #selected = c("דמוגרפיה", "בריאות", "חינוך והשכלה", "מדד חברתי-כלכלי", "מתוך סקר הוצאות  משקי הבית" , "מתוך סקר כוח אדם", "שכר ורווחה", "תחבורה"),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                        #column(1),
                                        column(2,
                                               pickerInput(inputId = "townsB", label = "סינון ישובים", 
                                                           choices = Pop_and_Physical2021 %>% pull(1),
                                                           selected = Pop_and_Physical2021 %>% pull(1),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                        column(2,
                                               pickerInput(inputId = "HighlightTownsB", label = "ישובים להבליט בכתום",
                                                           choices = Pop_and_Physical2021 %>% pull(1),
                                                           selected = NULL,
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2,
                                               # sliderInput(inputId = "TownSizeSlider", label = "מספר תושבים בישוב", 
                                               #             min = min(Pop_and_Physical2021$`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T), max = max(Pop_and_Physical2021$`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T), 
                                               #             value = c(1000, 1000000), log = TRUE,
                                               # )
                                               sliderTextInput(
                                                 inputId = "TownSizeSliderB",
                                                 label = "מספר תושבים בישוב", 
                                                 choices = c(1000, 5000, 10000, 25000, 50000, 100000, 250000, 500000, 1000000),
                                                 grid = TRUE,selected = c(1000, 1000000), 
                                               )
                                        ),
                                        column(2),
                                        column(2,
                                               pickerInput(inputId = "AdjustPopByB", label = "בתקנון לאוכלוסיה", 
                                                           choices = Names1[str_detect(Names1, "ייה בסוף השנה")],
                                                           selected = Names1[str_detect(Names1, "ייה בסוף השנה")][1],
                                                           #options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = FALSE
                                               )),
                                      ),
                                      fluidRow(
                                        column(2),
                                        
                                        column(2, sliderInput("EshkolB", "אשכול חברתי-כלכלי", min = 1, max = 10, value = c(1,10))),
                                        column(2, sliderInput("CoalitionB", "אחוז הצבעה לקואליציה", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("OppositionB", "אחוז הצבעה לאופוזיציה", min = 0, max = 100, value = c(0,100))),
                                      ),
                                      fluidRow(
                                        column(2),
                                        column(2, sliderInput("ReligiousB", "אחוז הצבעה למפלגות דתיות", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("UltraReligiousB", "אחוז חרדים", min = 0, max = 100, value = c(0,100))),
                                        column(2, sliderInput("ArabsB", "אחוז ערבים", min = 0, max = 100, value = c(0,100))),
                                      ),
                                      # column(2, sliderInput("Opposition", "אחוז הצבעה לאופוזיציה", min = 0, max = 100, value = c(0,100))),
                                      # column(2, pickerInput("size2", "Size", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה"
                                      #                       , options = pickerOptions(liveSearch = T))),
                                      # column(2),
                                      # column(2, pickerInput("color2", "Color", choices = c("none", names(Pop_and_Physical2021 %>% select_if(is.numeric))), selected = "none" , options = pickerOptions(liveSearch = T) )),
                                      #),
                                    ) 
                                      
                           ) # tabPanel Years
                ) # navbarPage
) # ui


# Define server logic required to draw a histogram

# SERVER ------------------------------------------------------------------
server <- function(session, input, output) {
  
  
  # observeEvent info$Topics ------------------------------------------------
  observeEvent(input$Topics, {
    
    xSelected <- input$xaxis1
    xNewOptions <- Pop_and_Physical2021 %>% select( matches(paste(input$Topics, collapse = "|"))) %>% select_if(is.numeric) %>% names()
    if ("פסולת מוצקה ביתית, מסחרית וגזם (ק\"ג ליום לנפש)" %in% input$Topics) {xNewOptions = c(xNewOptions, Pop_and_Physical2021 %>% select(contains("פסולת מוצקה")) %>% names)}
    if ("פשיעה ומשפט (ברשויות המונות 50,000 תושבים ויותר)" %in% input$Topics) {xNewOptions = c(xNewOptions, Pop_and_Physical2021 %>% select(contains("פשיעה ומשפט")) %>% names)}
    if (!(xSelected %in% xNewOptions | xSelected == "none")) {xSelected <- xNewOptions[1]}
    updatePickerInput(session, "xaxis1", 
                      choices = xNewOptions,
                      selected = xSelected
    )
    
    ySelected <- input$yaxis1
    #yNewOptions <- Pop_and_Physical2021 %>% select( matches(paste(input$Topics, collapse = "|"))) %>% select_if(is.numeric) %>% names()
    if (!(ySelected %in% xNewOptions | ySelected == "none")) {ySelected <- xNewOptions[1]}
    updatePickerInput(session, "yaxis1",
                      choices = xNewOptions,
                      selected = ySelected
    )
    
    cSelected <- input$color1
    #xNewOptions <- Pop_and_Physical2021 %>% select( matches(paste(input$Topics, collapse = "|"))) %>% select_if(is.numeric) %>% names()
    if (!(cSelected %in% xNewOptions | cSelected == "none")) {cSelected <- xNewOptions[1]}
    updatePickerInput(session, "color1",
                      choices = c("none", xNewOptions),
                      selected = cSelected
    )
    
    sSelected <- input$size1
    #xNewOptions <- Pop_and_Physical2021 %>% select( matches(paste(input$Topics, collapse = "|"))) %>% select_if(is.numeric) %>% names()
    if (!(sSelected %in% xNewOptions | sSelected == "none")) {sSelected <- xNewOptions[1]}
    updatePickerInput(session, "size1",
                      choices = c("none", xNewOptions),
                      selected = sSelected
    )
  })
  
  
  # output$EDAxyPlot --------------------------------------------------------
  output$EDAxyPlot <-  renderUI({
    
    
    #Names <- names(Pop_and_Physical2021)
    #names(Pop_and_Physical2021)[c(1,which(str_detect(names(Pop_and_Physical2021), "מרחקים|סקר")))]
    
    if (input$AdjustPopBy == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      Metuknan = " (מתוקנן לאוכלוסיה)"
    } else {
      Metuknan = paste0(
        " (מתוקנן לאוכלוסיית ",
        Names1[str_detect(Names1, input$AdjustPopBy)] %>% str_extract("בני.*"),
        ")"
      )
    }
    
    YLAB = paste0(names3 %>% filter(N3 == input$yaxis1) %>% pull(N4),
                  ifelse(input$PopAdjustY & !str_detect(input$yaxis1, "מתוקנן") & !str_detect(input$yaxis1, "אחוז")& !str_detect(input$yaxis1, "ל-1000"),
                         paste0("<br>", Metuknan), "")) %>% str_replace("<br><br>", "<br>")
    XLAB = paste0(names3 %>% filter(N3 == input$xaxis1) %>% pull(N4),
                  ifelse(input$PopAdjustX & !str_detect(input$xaxis1, "מתוקנן") & !str_detect(input$xaxis1, "אחוז")& !str_detect(input$xaxis1, "ל-1000"),
                         paste0("<br>", Metuknan), "")) %>% str_replace("<br><br>", "<br>")
    
    db <- Pop_and_Physical2021 %>% 
      filter(`שם הרשות` %in% input$towns) %>% 
      filter(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`>= as.numeric(input$TownSizeSlider[1]), `דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`<= as.numeric(input$TownSizeSlider[2])) %>% 
      filter( `מדד חברתי-כלכלי: אשכול (מ-1 עד 10, 1 הנמוך ביותר)` >= input$Eshkol[1],  `מדד חברתי-כלכלי: אשכול (מ-1 עד 10, 1 הנמוך ביותר)` <= input$Eshkol[2]) %>% 
      filter(`דמוגרפיה: אחוז הצבעה למפלגות הקואליציה, בחירות לכנסת 25` >= input$Coalition[1], `דמוגרפיה: אחוז הצבעה למפלגות הקואליציה, בחירות לכנסת 25` <= input$Coalition[2]) %>% 
      filter(`דמוגרפיה: אחוז הצבעה למפלגות האופוזיציה, בחירות לכנסת 25` >= input$Opposition[1], `דמוגרפיה: אחוז הצבעה למפלגות האופוזיציה, בחירות לכנסת 25` <= input$Opposition[2]) %>% 
      filter(`דמוגרפיה: אחוז הצבעה למפלגות דתיות, בחירות לכנסת 25` >= input$Religious[1], `דמוגרפיה: אחוז הצבעה למפלגות דתיות, בחירות לכנסת 25` <= input$Religious[2]) %>% 
      filter(`דמוגרפיה: אחוז חרדים` >= input$UltraReligious[1], `דמוגרפיה: אחוז חרדים` <= input$UltraReligious[2]) %>% 
      filter(`דמוגרפיה: ערבים (אחוזים)` >= input$Arabs[1], `דמוגרפיה: ערבים (אחוזים)` <= input$Arabs[2])
    # select(1, matches(paste(input$Topics, collapse = "|")))
    
    if (input$size1 != "none") {
      db <- db %>% mutate(s0 = .data[[input$size1]])
    } else {db <- db %>% mutate(s0 = "")}
    
    if (input$color1 != "none") {
      db <- db %>% mutate(c0 = .data[[input$color1]])
    } else {db <- db %>% mutate(c0 = "")}
    
    if (input$PopAdjustX & !str_detect(input$xaxis1, "מתוקנן") & !str_detect(input$xaxis1, "אחוז") & !str_detect(input$xaxis1, "ל-1000")) {
      if (input$AdjustPopBy == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
        db <- db %>% 
          mutate(x0 = .data[[input$xaxis1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * 1000) 
      } else {
        db <- db %>% 
          mutate(x0 = .data[[input$xaxis1]] / ( .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * .data[[input$AdjustPopBy]]/100) * 1000) 
      }
    } else {
      db <- db %>% mutate(x0 = .data[[input$xaxis1]])
    }
    if (input$PopAdjustY & !str_detect(input$yaxis1, "מתוקנן") & !str_detect(input$yaxis1, "אחוז")& !str_detect(input$yaxis1, "ל-1000")) {
      if (input$AdjustPopBy == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      db <- db %>% 
        mutate(y0 = .data[[input$yaxis1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * 1000) 
      } else {
        db <- db %>% 
          mutate(y0 = .data[[input$yaxis1]] / ( .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * .data[[input$AdjustPopBy]]/100) * 1000) 
      }
    } else {
      db <- db %>% mutate(y0 = .data[[input$yaxis1]])
    }
    
    db <- db %>% drop_na(y0)
    
    
    if (input$BarPlot == "Scatter") { # do a scatterplot
      
      p <- db %>% 
        drop_na(x0) %>% 
        #filter(` שם_הרשות` %in% input$towns) %>% 
        #mutate(y0 = .data[[input$yaxis1]]) %>%  
        #s0 = ifelse(input$size1 != "none", .data[[input$size1]], ""), c0 = ifelse(input$color1 != "none",.data[[input$color1]], 99999.88888)) %>% 
        #select(1, x0, y0, s0, c0) %>% 
        #mutate(across(c(x0, y0, s0, c0), ~ ifelse((is.numeric(.)) , prettyNum(.,  scientific = FALSE, big.mark = ","), .))) %>% 
        mutate(text =  paste0(`שם הרשות`  , "<br>", 
                              input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$size1, " ", prettyNum(s0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$color1, " ", prettyNum(c0, scientific = F, big.mark = ",", digits = 4))) %>% 
        mutate(text = str_replace_all(text, "none <br>", "")) %>% 
        mutate(text = str_replace_all(text, "none ", "")) %>% 
        mutate(text = str_replace_all(text, "NA", "")) %>% 
        ggplot(aes(x = x0, y = y0, text = text) 
        ) +
        ggplot2::theme_bw() +
        theme(
          axis.title = element_text(size = 15, face = "bold"),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", size = 18),
          axis.text = element_text(face = "bold", size = 12)
        ) #+
      #geom_text(vjust = -1, aes(label =  `שם הרשות`  , y = y0 + 
      #                            0.02*(max(y0, na.rm = T) - min(y0, na.rm = T))), size = 2)
      
      
      
      if (input$color1 == "none" & input$size1 == "none") {
        p <- p + geom_point(color = "darkblue", alpha = 0.5)
      } else if (input$color1 == "none") {
        p <- p + geom_point(aes(size = .data[[input$size1]]),color = "darkblue", alpha = 0.5) + scale_size_area()
      } else if (input$size1 == "none") {
        p <- p + geom_point(aes(color = .data[[input$color1]])) + scale_color_viridis_c()
      } else {
        p <- p + geom_point(aes(color = .data[[input$color1]], size = .data[[input$size1]])) + scale_color_viridis_c() + scale_size_area()
      }
      
      if (input$AddDiagLine) {p <- p + geom_abline(linetype = 3)}
      if (input$AddTrendLine) {p <- p + geom_smooth(aes(x = x0, y = y0, text = NULL), linetype = 3, method = "lm", se = F, na.rm = T, color = "grey24")}
      
      if (input$AddHorizontalLine) {p <- p + geom_hline(yintercept = input$Horizontal0, linetype = 3)}
      if (input$AddVertiaclLine) {p <- p + geom_vline(xintercept = input$Vertical0, linetype = 3)}
      
      if (!is.null(input$HighlightTowns)) {
        if (input$size1 == "none") {
          p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTowns), 
                              size = 1.2, color = "orange"
          )
        } else {
          p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTowns) %>% 
                                mutate(Size = 1.2 * .data[[input$size1]]), 
                              aes(size = Size), color = "orange"
          )
        }
      }
      
      p <- p +
        labs(
          x = XLAB,
          #y = YLAB,
          y = NULL,
          title = YLAB,
          color = NULL
        ) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
      
    }  else if (input$BarPlot == "Bar") { # do a bar plot
      
      p <- db %>% 
        mutate(text =  paste0(`שם הרשות`  , "<br>", 
                              # input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ","), "<br>", 
                              input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$size1, " ", prettyNum(s0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$color1, " ", prettyNum(c0, scientific = F, big.mark = ",", digits = 4))) %>% 
        mutate(text = str_replace_all(text, "none <br>", "")) %>% 
        mutate(text = str_replace_all(text, "none ", "")) %>% 
        mutate(text = str_replace_all(text, "NA", "")) %>% 
        drop_na(y0) %>% 
        ggplot(aes(x = y0, y = reorder(`שם הרשות`, y0), text = text) 
        ) +
        ggplot2::theme_bw() +
        theme(
          axis.title = element_text(size = 15, face = "bold"),
          strip.background = element_rect(fill = "black"),
          strip.text = element_text(color = "white", size = 18),
          axis.text = element_text(face = "bold", size = 12)
        ) +
        geom_text(aes(label =  paste0( `שם הרשות`  ," ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4)), x = y0 - 
                        0.1*(max(y0, na.rm = T) - min(y0, na.rm = T))), size = 3, color = "white")
      
      if (input$color1 == "none" & input$size1 == "none") {
        p <- p + geom_col(fill = "darkblue", alpha = 0.5)
      } else if (input$color1 == "none") {
        p <- p + geom_col(aes(size = .data[[input$size1]]), fill = "darkblue", alpha = 0.5) 
      } else if (input$size1 == "none") {
        p <- p + geom_col(aes(fill = .data[[input$color1]])) + scale_fill_viridis_c()
      } else {
        p <- p + geom_col(aes(fill = .data[[input$color1]]#, size = .data[[input$size1]])
        )) + scale_fill_viridis_c() 
      }
      
      p <- p +
        labs(
          #x = names3 %>% filter(N3 == input$xaxis1) %>% pull(N4),
          y = NULL,
          x = names3 %>% filter(N3 == input$yaxis1) %>% pull(N4),
          color = NULL, fill = NULL
        ) 
    }  else if (input$BarPlot == "Group") { # do a bar plot 
      
      db3 <- db %>% 
        mutate(x0 = case_when(
          n_distinct(x0) < 20 ~ factor(x0),
          TRUE ~ cut(x0, scales::pretty_breaks(10)(x0), include.lowest = TRUE, dig.lab = 12)
        )) %>% 
        group_by(x0) %>% 
        summarise(y0 = stats::weighted.mean(y0, w = `דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`), N = n(), Pop = sum(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T)) %>% 
        mutate(text2 =  paste0(x0  , " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4))) %>% 
        mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
        mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
        mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
        mutate(text =  paste0(x0,"<br>",
                              " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              "ישובים: ", N, "<br>", 
                              "אוכלוסיה: ", prettyNum(Pop, scientific = F, big.mark = ",", digits = 4), "<br>"
        )) %>% 
        mutate(text = str_replace_all(text, "none <br>", "")) %>% 
        mutate(text = str_replace_all(text, "none ", "")) %>% 
        mutate(text = str_replace_all(text, "NA", "")) %>% 
        mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
        mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
        mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
        drop_na(y0) %>% rename(text3 = text)
      
      YLAB <- paste0("ממוצע ", YLAB, "<br>", "לפי קבוצות", XLAB)
    } else if (input$BarPlot == "Boxplot") { # do a BoxPlot plot 
      
      db3 <- db %>% 
        mutate(x0 = case_when(
          n_distinct(x0) < 20 ~ factor(x0),
          TRUE ~ cut(x0, scales::pretty_breaks(10)(x0), include.lowest = TRUE, dig.lab = 12)
        )) %>% 
        group_by(x0) %>% 
        mutate(N = n(), Pop = sum(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T)) %>% 
        #mutate(text2 =  paste0(x0  , " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4))) %>% 
        #mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
        #mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
        #mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
        mutate(text =  paste0(x0,"<br>",
                              " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              "ישובים: ", N, "<br>", 
                              "אוכלוסיה: ", prettyNum(Pop, scientific = F, big.mark = ",", digits = 4), "<br>"
        )) %>% 
        mutate(text = str_replace_all(text, "none <br>", "")) %>% 
        mutate(text = str_replace_all(text, "none ", "")) %>% 
        mutate(text = str_replace_all(text, "NA", "")) %>% 
        # mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
        # mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
        # mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
        drop_na(y0) %>% rename(text3 = text)
      
      YLAB <- paste0("התפלגות ", YLAB, "<br>", "לפי קבוצות", XLAB)
    }
    
    output$p1i <- renderPlotly({
      if (input$BarPlot == "Scatter") {
        #TopMargin = ifelse (str_count(YLAB, "<br>") >=3, 250, 150)
        TopMargin0 = str_count(YLAB, "<br>") 
        TopMargin <- case_when(TopMargin0 >= 3 ~ 250, TopMargin0 == 2~ 150, TRUE ~50)
        ggplotly(p, height = 800, width = 1000, tooltip = "text", dynamicTicks = TRUE) %>% 
          add_annotations(
            x = ~x0,  # X coordinates of the labels
            y = ~y0,  # Y coordinates of the labels
            text = ~ ifelse(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה` > 250000, paste("<b>", `שם הרשות`, "</b>"), paste(`שם הרשות`)) ,  # Text content of the labels
            showarrow = FALSE,  # Hide arrow
            xanchor = "right",  # Horizontal anchor point
            yanchor = "bottom",  # Vertical anchor point
            font = list(size = 8, color = "black", alpha = 0.8),  # Text font properties
            xshift = 0,  # Horizontal shift (in pixels)
            yshift = 0  # Vertical shift (in pixels)
          ) %>% 
          layout(margin = list(t = TopMargin)) %>% 
          config(displayModeBar = FALSE)
      } else if (input$BarPlot == "Bar") { # BarPlot
        
        db2 <- db %>% select(x0, y0, s0, c0, 1) %>% 
          mutate(text2 =  paste0(`שם הרשות`  , " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4)
                                 # "<br>", 
                                 # # input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ","), "<br>", 
                                 # input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ","), "<br>", 
                                 # input$size1, " ", prettyNum(s0, scientific = F, big.mark = ","), "<br>", 
                                 # input$color1, " ", prettyNum(c0, scientific = F, big.mark = ","))
          )) %>% 
          mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
          mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
          mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
          mutate(text =  paste0(`שם הרשות`  , 
                                "<br>", #prettyNum(y0, scientific = F, big.mark = ","),
                                # # input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ","), "<br>", 
                                input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                input$size1, " ", prettyNum(s0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                input$color1, " ", prettyNum(c0, scientific = F, big.mark = ",", digits = 4)
          )) %>% 
          mutate(text = str_replace_all(text, "none <br>", "")) %>% 
          mutate(text = str_replace_all(text, "none ", "")) %>% 
          mutate(text = str_replace_all(text, "NA", "")) %>% 
          mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
          mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
          mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
          drop_na(y0) %>% rename(text3 = text)
        
        db2 <- db2 %>% arrange(desc(y0)) %>% mutate(`שם הרשות` = fct_rev(fct_inorder(`שם הרשות`))) 
        
        if (input$color1 == "none") {
          
          if (!is.null(input$HighlightTowns)) {
            plot_ly(db2, x = ~y0, y = ~`שם הרשות`, type = "bar", orientation = "h", text = ~text3, hoverinfo = ~text2, texttemplate = "%{hoverinfo}",  marker = list(color = ifelse(db2$`שם הרשות` %in% input$HighlightTowns, "orange", "blue"))) %>% 
              layout( xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 800) %>% 
              config(displayModeBar = FALSE)
          } else {
            plot_ly(db2, x = ~y0, y = ~`שם הרשות`, type = "bar", orientation = "h", text = ~text3, hoverinfo = ~text2, texttemplate = "%{hoverinfo}") %>% 
              layout(xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 800) %>% 
              config(displayModeBar = FALSE)
          }
          
        } else {
          plot_ly(db2, x = ~y0, y = ~`שם הרשות`, type = "bar", orientation = "h", text = ~text3, hoverinfo = ~text2, texttemplate = "%{hoverinfo}",marker = list(color = ~s0)) %>% 
            #layout(xaxis = list(title = list(text = names3 %>% filter(N3 == input$yaxis1) %>% pull(N4), font = list(weight = "bold", size = 15))), yaxis = list(title = '')) %>% 
            layout(xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 800) %>% 
            config(displayModeBar = FALSE)
        }
        
        
      } else if (input$BarPlot == "Group") { # Group
        plot_ly(db3, x = ~y0, y = ~x0, type = "bar", orientation = "h", text = ~text3, hoverinfo = ~text2, texttemplate = "%{hoverinfo}") %>% 
          layout(xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 800) %>% 
          config(displayModeBar = FALSE)
      } else if (input$BarPlot == "Boxplot") { # Boxplot
        plot_ly(db3, x = ~y0, y = ~x0, type = "box", orientation = "h") %>% 
          layout(xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 800) %>% 
          config(displayModeBar = FALSE)
      }
    })
    plotlyOutput("p1i")
    
  }) # EDAxyplot
  
  
  # output$Comments1 --------------------------------------------------------
  output$Comments1 <- renderUI({
    
    #Comments1 <- Comments1 %>% filter(character != "סה\"כ" , character != "\\.\\.")
    Comments1label = ""
    
    if (str_detect(input$xaxis1, "סקר כוח אדם") | str_detect(input$yaxis1, "סקר כוח אדם") | str_detect(input$size1, "סקר כוח אדם") | str_detect(input$color1, "סקר כוח אדם")) {
      Comments1label <- paste0(Comments1label, "<br>נתוני סקר כח אדם קיימים עבור ערים המונות 50,000 תושבים ויותר<br>")
    }
    
    
    if (str_detect(input$xaxis1, "סקר הוצאות  משקי הבית") | str_detect(input$yaxis1, "סקר הוצאות  משקי הבית") | str_detect(input$size1, "סקר הוצאות  משקי הבית") | str_detect(input$color1, "סקר הוצאות  משקי הבית")) {
      Comments1label <- paste0(Comments1label, "<br>נתוני סקר הוצאות משקי הבית קיימים עבור ערים המונות 50,000 תושבים ויותר<br>")
    }
    
    
    c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$xaxis1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
      pull(comment)
    if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$xaxis1, ": <b><br>", c1, "</b><br>") }
    if (input$xaxis1 != input$yaxis1) {
      
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$yaxis1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$yaxis1, ": <b><br>", c1, "</b><br>") }
    }
    if (input$size1 != input$yaxis1 & input$size1 != input$xaxis1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$size1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$size1, ": <b><br>",  c1, "</b><br>") }
    }
    if (input$color1 != input$yaxis1 & input$color1 != input$xaxis1 & input$color1 != input$size1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$color1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$color1, ": <b><br>", c1, "</b><br>") }
    }
   
    if (first(Comments1label) != "") {Comments1label <- paste0("הערות:", "<br>", paste0(Comments1label, collapse = "<br>"))}
    
    if (input$AdjustPopBy == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      Metuknan = " (מתוקנן לאוכלוסיה)"
    } else {
      Metuknan = paste0(
        " (מתוקנן לאוכלוסיית ",
        Names1[str_detect(Names1, input$AdjustPopBy)] %>% str_extract("בני.*"),
        ")"
                        )
    }
    
    Comments1label <- paste0("ציר Y: <b>", input$yaxis1, ifelse(input$PopAdjustY & !str_detect(input$yaxis1, "מתוקנן") & !str_detect(input$yaxis1, "אחוז")& !str_detect(input$yaxis1, "ל-1000"), Metuknan, ""),"</b><br>",
                             "ציר X: <b>", input$xaxis1, ifelse(input$PopAdjustX & !str_detect(input$xaxis1, "מתוקנן") & !str_detect(input$xaxis1, "אחוז") & !str_detect(input$xaxis1, "ל-1000"), Metuknan, ""), "</b><br>",
                             ifelse(input$size1 != "none",  paste0("גודל: ",input$size1, "<br>"), ""),
                             ifelse(input$color1 != "none", paste0("צבע: ",input$color1, "<br>"), ""),
                             "<br><br><br>",
                             Comments1label
    )
    
    div(
      HTML("<div dir='rtl'>", Comments1label,"</div>"),
      style = "text-align: right; margin: 000px;"
    )
    
  })
  
  # output$EDAxyPlotB --------------------------------------------------------
  output$EDAxyPlotB <-  renderUI({
    
    if (input$AdjustPopByB == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      Metuknan = " (מתוקנן לאוכלוסיה)"
    } else {
      Metuknan = paste0(
        " (מתוקנן לאוכלוסיית ",
        Names1[str_detect(Names1, input$AdjustPopByB)] %>% str_extract("בני.*"),
        ")"
      )
    }
    
    
    YLAB = paste0(names3 %>% filter(N3 == input$yaxisB1) %>% pull(N4),
                  ifelse(input$PopAdjustBY & !str_detect(input$yaxisB1, "מתוקנן") & !str_detect(input$yaxisB1, "אחוז")& !str_detect(input$yaxisB1, "ל-1000"),
                         paste0("<br>", Metuknan), "")) %>% str_replace("<br><br>", "<br>")
    XLAB = paste0(names3 %>% filter(N3 == input$xaxisB1) %>% pull(N4),
                  ifelse(input$PopAdjustBX & !str_detect(input$xaxisB1, "מתוקנן") & !str_detect(input$xaxisB1, "אחוז")& !str_detect(input$xaxisB1, "ל-1000"),
                         paste0("<br>", Metuknan), "")) %>% str_replace("<br><br>", "<br>")
    
    db <- Combined %>% 
      filter(`שם הרשות` %in% input$townsB) %>% 
      group_by(`שם הרשות`) %>% 
      filter(any(name == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" & value >= input$TownSizeSliderB[1] & value <= input$TownSizeSliderB[2])) %>% 
      #filter(any(name == input$AdjustPopByB )) %>% 
      filter(any(name == "מדד חברתי-כלכלי: אשכול (מ-1 עד 10, 1 הנמוך ביותר)" & value >= input$EshkolB[1] & value <= input$EshkolB[2])) %>% 
      filter(any(name == "דמוגרפיה: אחוז הצבעה למפלגות הקואליציה, בחירות לכנסת 25" & value >= input$CoalitionB[1] & value <= input$CoalitionB[2])) %>% 
      filter(any(name == "דמוגרפיה: אחוז הצבעה למפלגות האופוזיציה, בחירות לכנסת 25" & value >= input$OppositionB[1] & value <= input$OppositionB[2])) %>% 
      filter(any(name == "דמוגרפיה: אחוז הצבעה למפלגות דתיות לא חרדיות, בחירות לכנסת 25" & value >= input$ReligiousB[1] & value <= input$ReligiousB[2])) %>% 
      filter(any(name == "דמוגרפיה: אחוז חרדים" & value >= input$UltraReligiousB[1] & value <= input$UltraReligiousB[2])) %>% 
      filter(any(name == "דמוגרפיה: ערבים (אחוזים)" & value >= input$ArabsB[1] & value <= input$ArabsB[2])) %>% 
      filter(str_detect(str_replace_all(name, "\\(|\\)", ""), paste( input$AdjustPopByB, str_replace_all(input$xaxisB1, "\\(|\\)", ""), str_replace_all(input$yaxisB1, "\\(|\\)", ""), str_replace_all(input$colorB1, "\\(|\\)", ""), str_replace_all(input$sizeB1, "\\(|\\)", ""), "כ אוכלוסייה בסוף השנה", sep = "|"))) %>% 
      ungroup
      
    db <- db %>% pivot_wider(names_from = name, values_from = value)
    
    
      if (input$PopAdjustBX & !str_detect(input$xaxisB1, "מתוקנן") & !str_detect(input$xaxisB1, "אחוז") & !str_detect(input$xaxisB1, "ל-1000")) {
        if (input$AdjustPopByB == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
        db <- db %>% 
          mutate(x0 = .data[[input$xaxisB1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * 1000)
        } else {
          db <- db %>% 
            mutate(x0 = .data[[input$xaxisB1]] / ( .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * .data[[input$AdjustPopByB]]/100) * 1000) 
        }
      } else {
        db <- db %>% mutate(x0 = .data[[input$xaxisB1]])
      }
    if (input$PopAdjustBY & !str_detect(input$yaxisB1, "מתוקנן") & !str_detect(input$yaxisB1, "אחוז")& !str_detect(input$yaxisB1, "ל-1000")) {
      if (input$AdjustPopByB == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      db <- db %>% 
        mutate(y0 = .data[[input$yaxisB1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * 1000)
      } else {
        db <- db %>% 
          mutate(y0 = .data[[input$yaxisB1]] / ( .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]] * .data[[input$AdjustPopByB]]/100) * 1000) 
      }
    } else {
      db <- db %>% mutate(y0 = .data[[input$yaxisB1]])
    }
     
   
    if (input$sizeB1 != "none") {
      db <- db %>% mutate(s0 = .data[[input$sizeB1]])
      if (input$sizeB1 == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {db <- db %>% mutate(s0 = max(.data[[input$sizeB1]], na.rm = T), .by = `שם הרשות`)}
    } else {db <- db %>% mutate(s0 = "")}
    
    if (input$colorB1 != "none") {
      db <- db %>% mutate(c0 = .data[[input$colorB1]])
    } else {db <- db %>% mutate(c0 = "")}
    # select(1, matches(paste(input$Topics, collapse = "|")))
    
    db <- db %>% drop_na(y0)
    
    output$p2i <- renderPlotly({
      
      if (input$BarPlotB == "Scatter") { # Group
        
        db <- db %>% 
          drop_na(x0) 
        
        UsingTowns <- db %>% select(townname = `שם הרשות`, contains("כ אוכלוסייה בסוף השנה"), Year) %>% filter(Year == 2021) %>% 
          rename(Size = 2) %>% ungroup %>% distinct(townname, Size) %>% arrange(-Size) %>% slice(1:min(c(12, nrow(.)))) %>% pull(1)
        
        db <- db %>% filter(`שם הרשות` %in% UsingTowns) %>% group_by(`שם הרשות`) %>% arrange(Year) %>% mutate(Year = factor(Year))
        
        db <- db %>% mutate(c0 = Year)
        
        p <- db %>% mutate(text =  paste0(`שם הרשות`  , "<br>", 
                              input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$size1, " ", prettyNum(s0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                              input$color1, " ", prettyNum(c0, scientific = F, big.mark = ",", digits = 4))) %>% 
          mutate(text = str_replace_all(text, "none <br>", "")) %>% 
          mutate(text = str_replace_all(text, "none ", "")) %>% 
          mutate(text = str_replace_all(text, "NA", "")) %>% 
          ggplot(aes(x = x0, y = y0, text = text) 
          ) +
          ggplot2::theme_bw() +
          theme(
            axis.title = element_text(size = 15, face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 18),
            axis.text = element_text(face = "bold", size = 12)
          ) #+
        #geom_text(vjust = -1, aes(label =  `שם הרשות`  , y = y0 + 
        #                            0.02*(max(y0, na.rm = T) - min(y0, na.rm = T))), size = 2)
        
        
        p <- p + geom_path(linetype = 3, aes(group = `שם הרשות`))
        
        if (input$colorB1 == "none" & input$sizeB1 == "none") {
          p <-  p + geom_point(aes(color = c0)) +scale_color_brewer()
        } else if (input$colorB1 == "none") {
          p <- p + geom_point(aes(size = s0, color = c0), alpha = 0.5) + scale_size_area()+scale_color_brewer()
        } else if (input$sizeB1 == "none") {
          p <- p + geom_point(aes(color = c0)) +scale_color_brewer()
        } else {
          p <- p + geom_point(aes(color = c0, size = s0)) + scale_color_brewer() + scale_size_area()
        }
        
        if (input$AddDiagLineB) {p <- p + geom_abline(linetype = 3)}
        if (input$AddTrendLineB) {p <- p + geom_smooth(aes(x = x0, y = y0, text = NULL), linetype = 3, method = "lm", se = F, na.rm = T, color = "grey24")}
        
        if (input$AddHorizontalLineB) {p <- p + geom_hline(yintercept = input$Horizontal0B, linetype = 3)}
        if (input$AddVertiaclLineB) {p <- p + geom_vline(xintercept = input$Vertical0B, linetype = 3)}
        
        if (!is.null(input$HighlightTownsB)) {
          if (input$sizeB1 == "none") {HorizontalHorizontal
            p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTownsB), 
                                size = 1.2, color = "orange"
            )
          } else {
            p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTownsB) %>% 
                                  mutate(Size = 1.2 * s0), 
                                aes(size = Size), color = "orange"
            )
          }
        }
        
        # p <- p +
        #   labs(
        #     x = XLAB,
        #     y = YLAB,
        #     color = NULL,
        #     size = NULL
        #   ) 
        
        p <- p +
          labs(
            x = XLAB,
            #y = YLAB,
            y = NULL,
            title = YLAB,
            color = NULL,
            size = NULL
          ) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 18))
        
        TopMargin0 = str_count(YLAB, "<br>") 
        TopMargin <- case_when(TopMargin0 >= 3 ~ 250, TopMargin0 == 2~ 150, TRUE ~50)
        
        ggplotly(p, height = 800, width = 1000, tooltip = "text", dynamicTicks = TRUE) %>% 
          add_annotations(
            x = ~x0,  # X coordinates of the labels
            y = ~y0,  # Y coordinates of the labels
            text = ~ ifelse(s0 > 250000, paste("<b>", `שם הרשות`, "</b>"), paste(`שם הרשות`)) ,  # Text content of the labels
            showarrow = FALSE,  # Hide arrow
            xanchor = "right",  # Horizontal anchor point
            yanchor = "bottom",  # Vertical anchor point
            font = list(size = 8, color = "black", alpha = 0.8),  # Text font properties
            xshift = 0,  # Horizontal shift (in pixels)
            yshift = 0  # Vertical shift (in pixels)
          ) %>% 
          layout(legend = list(traceorder = "reversed", showlegend = TRUE), margin = list(t = TopMargin)) %>% 
          config(displayModeBar = FALSE)
        
      } else if (input$BarPlotB == "Bar") { # Bar
        
        UsingTowns <- db %>% select(townname = `שם הרשות`, contains("כ אוכלוסייה בסוף השנה"), Year) %>% filter(Year == 2021) %>% 
          rename(Size = 2) %>% ungroup %>% distinct(townname, Size) %>% arrange(-Size) %>% slice(1:min(c(12, nrow(.)))) %>% pull(1)
        
        db <- db %>% filter(`שם הרשות` %in% UsingTowns) %>% group_by(`שם הרשות`) %>% arrange(Year) %>% mutate(Year = factor(Year))
        
        db <- db %>% mutate(c0 = Year)
        
        p <- db %>% mutate(text =  paste0(`שם הרשות`  , "<br>", 
                                          Year, "<br>",
                                          #input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                          input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>"
                                          #input$size1, " ", prettyNum(s0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                          #input$color1, " ", prettyNum(c0, scientific = F, big.mark = ",", digits = 4))
                                          )) %>% 
          mutate(text = str_replace_all(text, "none <br>", "")) %>% 
          mutate(text = str_replace_all(text, "none ", "")) %>% 
          mutate(text = str_replace_all(text, "NA", "")) %>% 
          ggplot(aes(y = Year, x = y0, text = text, fill = c0) 
          ) +
          ggplot2::theme_bw() +
          theme(
            axis.title = element_text(size = 15, face = "bold"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(color = "white", size = 18),
            axis.text = element_text(face = "bold", size = 12)
          ) #+
        #geom_text(vjust = -1, aes(label =  `שם הרשות`  , y = y0 + 
        #                            0.02*(max(y0, na.rm = T) - min(y0, na.rm = T))), size = 2)
        
        
        p <- p + geom_col() + scale_fill_brewer()
        
        p <- p + facet_wrap(~`שם הרשות`)
        
        # 
        # if (!is.null(input$HighlightTowns)) {
        #   if (input$size1 == "none") {
        #     p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTowns), 
        #                         size = 1.2, color = "orange"
        #     )
        #   } else {
        #     p <- p + geom_point(data = . %>% filter(`שם הרשות` %in% input$HighlightTowns) %>% 
        #                           mutate(Size = 1.2 * s0), 
        #                         aes(size = Size), color = "orange"
        #     )
        #   }
        # }
        
        p <- p +
          labs(
            x = YLAB %>% str_replace_all("<br>", "\n"),
            y = NULL,
            fill = NULL,
            size = NULL
          ) 
        
        p <- p + guides(fill = guide_legend(reverse = TRUE)) +theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        p <- p + geom_text(aes(x = 0.8*y0, label = paste0(Year, ": ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4))))
        
        
        ggplotly(p, height = 600, width = 1000, tooltip = "text") %>% 
          # add_annotations(
          #   x = ~y0,  # X coordinates of the labels
          #   y = ~Year,  # Y coordinates of the labels
          #   text = ~ prettyNum(y0, scientific = F, big.mark = ",", digits = 4) ,  # Text content of the labels
          #   showarrow = FALSE,  # Hide arrow
          #   xanchor = "right",  # Horizontal anchor point
          #   yanchor = "bottom",  # Vertical anchor point
          #   font = list(size = 8, color = "black", alpha = 0.8),  # Text font properties
          #   xshift = 0,  # Horizontal shift (in pixels)
          #   yshift = 0  # Vertical shift (in pixels)
          # ) %>%
          # layout(margin = list(l = 150)) %>%
          layout(legend = list(traceorder = "reversed", showlegend = TRUE), xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20, color = "white"))), yaxis = list(title = ''), width = 1000, height = 600) %>% 
          config(displayModeBar = FALSE)
      } else if (input$BarPlotB == "Group") { # Group
        
        db <- db %>% 
          mutate(x0 = case_when(
            n_distinct(x0) < 20 ~ factor(x0),
            TRUE ~ cut(x0, scales::pretty_breaks(10)(x0), include.lowest = TRUE, dig.lab = 12)
          )) %>% 
          drop_na() %>% 
          group_by(x0, Year) %>% 
          summarise(y0 = stats::weighted.mean(y0, w = `דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`), N = n(), Pop = sum(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T)) %>% 
          mutate(text =  paste0(x0,"<br>",
                                " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                "ישובים: ", N, "<br>", 
                                "אוכלוסיה: ", prettyNum(Pop, scientific = F, big.mark = ",", digits = 4), "<br>"
          )) %>% 
          mutate(text = str_replace_all(text, "none <br>", "")) %>% 
          mutate(text = str_replace_all(text, "none ", "")) %>% 
          mutate(text = str_replace_all(text, "NA", "")) %>% 
          drop_na(y0) %>% rename(text3 = text) %>% ungroup %>% arrange(Year) %>% mutate(Year = factor(Year))
        
        YLAB <- paste0("ממוצע ", YLAB, "<br>", "לפי קבוצות", XLAB)

        # plot_ly(db, x = ~y0, y = ~x0, type = "bar", orientation = "h", text = ~text3, hoverinfo = ~text2, texttemplate = "%{hoverinfo}") %>% 
        #   layout(xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 600) %>% 
        #   config(displayModeBar = FALSE)
        plot_ly(db, x = ~y0, y = ~x0, color = ~Year, type = "bar", orientation = "h", colors = c("red", "blue")) %>% 
          layout(legend = list(traceorder = "reversed"), boxmode = "group", xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 600) %>% 
          config(displayModeBar = FALSE)
      } else  if (input$BarPlotB == "Boxplot") { # Boxplot
        db <- db %>% 
          mutate(x0 = case_when(
            n_distinct(x0) < 20 ~ factor(x0),
            TRUE ~ cut(x0, scales::pretty_breaks(10)(x0), include.lowest = TRUE, dig.lab = 12)
          )) %>% 
          group_by(x0) %>% 
          mutate(N = n(), Pop = sum(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`, na.rm = T)) %>% 
          #mutate(text2 =  paste0(x0  , " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4))) %>% 
          #mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
          #mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
          #mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
          mutate(text =  paste0(x0,"<br>",
                                " ממוצע משוקלל: ", prettyNum(y0, scientific = F, big.mark = ",", digits = 4), "<br>", 
                                "ישובים: ", N, "<br>", 
                                "אוכלוסיה: ", prettyNum(Pop, scientific = F, big.mark = ",", digits = 4), "<br>"
          )) %>% 
          mutate(text = str_replace_all(text, "none <br>", "")) %>% 
          mutate(text = str_replace_all(text, "none ", "")) %>% 
          mutate(text = str_replace_all(text, "NA", "")) %>% 
          # mutate(text2 = str_replace_all(text2, "none <br>", "")) %>% 
          # mutate(text2 = str_replace_all(text2, "none ", "")) %>% 
          # mutate(text2 = str_replace_all(text2, "NA", "")) %>% 
          drop_na(y0) %>% rename(text3 = text) %>% ungroup %>% arrange(Year) %>% mutate(Year = factor(Year))
        
        YLAB <- paste0("התפלגות ", YLAB, "<br>", "לפי קבוצות", XLAB)
        
        suppressWarnings(
        plot_ly(db, x = ~y0, y = ~x0, color = ~Year, type = "box", orientation = "h", colors = c("red", "blue")) %>% 
          layout(legend = list(traceorder = "reversed"), boxmode = "group", xaxis = list(title = list(text = YLAB, font = list(weight = "bold", size = 20))), yaxis = list(title = ''), width = 1000, height = 600) %>% 
          config(displayModeBar = FALSE)
        )
      }
    })
    plotlyOutput("p2i")
    
  }) # EDAxyPlotB
  
  # output$Comments1B --------------------------------------------------------
  output$Comments1B <- renderUI({
    
    #Comments1 <- Comments1 %>% filter(character != "סה\"כ" , character != "\\.\\.")
    Comments1label = ""
    
    if (str_detect(input$xaxisB1, "סקר כוח אדם") | str_detect(input$yaxisB1, "סקר כוח אדם") | str_detect(input$sizeB1, "סקר כוח אדם") | str_detect(input$colorB1, "סקר כוח אדם")) {
      Comments1label <- paste0(Comments1label, "<br>נתוני סקר כח אדם קיימים עבור ערים המונות 50,000 תושבים ויותר<br>")
    }
    
    
    if (str_detect(input$xaxisB1, "סקר הוצאות  משקי הבית") | str_detect(input$yaxisB1, "סקר הוצאות  משקי הבית") | str_detect(input$sizeB1, "סקר הוצאות  משקי הבית") | str_detect(input$color1, "סקר הוצאות  משקי הבית")) {
      Comments1label <- paste0(Comments1label, "<br>נתוני סקר הוצאות משקי הבית קיימים עבור ערים המונות 50,000 תושבים ויותר<br>")
    }
    
    
    c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$xaxisB1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
      pull(comment)
    if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$xaxisB1, ": <b><br>", c1, "</b><br>") }
    if (input$xaxisB1 != input$yaxisB1) {
      
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$yaxisB1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$yaxisB1, ": <b><br>", c1, "</b><br>") }
    }
    if (input$sizeB1 != input$yaxisB1 & input$sizeB1 != input$xaxisB1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$sizeB1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$sizeB1, ": <b><br>",  c1, "</b><br>") }
    }
    if (input$color1 != input$yaxisB1 & input$color1 != input$xaxisB1 & input$colorB1 != input$sizeB1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(str_replace_all(input$colorB1, "\\(|\\)", ""), str_trim(str_replace_all(character, "\\(|\\)", "")))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$colorB1, ": <b><br>", c1, "</b><br>") }
    }
    
    if (first(Comments1label) != "") {Comments1label <- paste0("הערות:", "<br>", paste0(Comments1label, collapse = "<br>"))}
    
    if (input$AdjustPopByB == "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה") {
      Metuknan = " (מתוקנן לאוכלוסיה)"
    } else {
      Metuknan = paste0(
        " (מתוקנן לאוכלוסיית ",
        Names1[str_detect(Names1, input$AdjustPopByB)] %>% str_extract("בני.*"),
        ")"
      )
    }
    
    
    
    Comments1label <- paste0("ציר Y: <b>", input$yaxisB1, ifelse(input$PopAdjustBY & !str_detect(input$yaxisB1, "מתוקנן") & !str_detect(input$yaxisB1, "אחוז")& !str_detect(input$yaxisB1, "ל-1000"), Metuknan, ""),"</b><br>",
                             "ציר X: <b>", input$xaxisB1, ifelse(input$PopAdjustBX & !str_detect(input$xaxisB1, "מתוקנן") & !str_detect(input$xaxisB1, "אחוז") & !str_detect(input$xaxisB1, "ל-1000"), Metuknan, ""), "</b><br>",
                             ifelse(input$sizeB1 != "none",  paste0("גודל: ",input$sizeB1, "<br>"), ""),
                             ifelse(input$colorB1 != "none", paste0("צבע: ",input$colorB1, "<br>"), ""),
                             "<br><br><br>",
                             Comments1label
    )
    
    div(
      HTML("<div dir='rtl'>", Comments1label,"</div>"),
      style = "text-align: right; margin: 000px;"
    )
    
  })
  
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
