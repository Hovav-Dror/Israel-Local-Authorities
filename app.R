

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(shinythemes)
library(shinyBS)

load("MunicipalData.rda")

# UI ----------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("קובץ רשויות מקומיות", id = "NavBar", position = "fixed-top",
                           tabPanel("גרף",
                                    fluidPage(
                                      hr(),
                                      p(),p(),p(),p(),p(),p(),p(),p(),
                                      fluidRow(),
                                      fluidRow(),
                                      h4(" "),
                                      hr(),
                                      fluidRow(
                                        column(1),
                                        column(2,
                                               pickerInput(inputId = "Topics", label = "סינון נושאים", 
                                                           choices = names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           #selected =names3 %>% select(N3) %>% mutate(NN = str_extract(N3, "([^:]+)")) %>% distinct(NN) %>% arrange(NN) %>% filter(NN!="שם הרשות") %>% pull(NN),
                                                           selected = c("דמוגרפיה", "בריאות", "חינוך והשכלה", "מדד חברתי-כלכלי", "מתוך סקר הוצאות  משקי הבית" , "מתוך סקר כוח אדם", "שכר ורווחה", "תחבורה"),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
                                        column(2),
                                        column(2,
                                               pickerInput(inputId = "towns", label = "סינון ישובים", 
                                                           choices = Pop_and_Physical2021 %>% pull(1),
                                                           selected = Pop_and_Physical2021 %>% pull(1),
                                                           options = list(`live-search` = TRUE , `actions-box` = TRUE, `size` = 10 ),
                                                           multiple = TRUE
                                               )),
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
                                               tipify(materialSwitch(
                                                 inputId = "BarPlot",
                                                 label = "מיפוי משתנה בודד", 
                                                 value = FALSE,
                                                 status = "primary"
                                               ),
                                               "גרף עמודות, משתמש בציר Y בלבד, לעומת גרף x-y"),
                                               
                                        ),
                                      ),
                                      hr(),
                                      fluidRow(
                                        column(1),
                                        column(2, pickerInput("xaxis1", "x-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה"  , options = pickerOptions(liveSearch = T))),
                                        column(1, checkboxInput(inputId = "PopAdjustX", label = "תקנון לאוכלוסיה", value = FALSE)),
                                        column(1),
                                        column(2, pickerInput("yaxis1", "y-axis", choices = names(Pop_and_Physical2021 %>% select_if(is.numeric)), selected = "דמוגרפיה: צפיפות אוכלוסייה לקמ''ר ביישובים שמנו 5,000 תושבים ויותר"  , options = pickerOptions(liveSearch = T))),
                                        column(1, checkboxInput(inputId = "PopAdjustY", label = "תקנון לאוכלוסיה", value = FALSE)),
                                        #column(2, pickerInput("y-axis", "yaxis1", choices = names(Pop_and_Physical2021), selected = " צפיפות_אוכלוסייה_לקמר_ביישובים_שמנו_5_000_תושבים_ויותר"  )),
                                        column(1),
                                        
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
                                          
                                          uiOutput("EDAxyPlot"),
                                        )),
                                        column(5, uiOutput("Comments1"))
                                        ),
                                      
                                      
                                      p(),
                                      
                                    ) # fluidPage
                                    ) # tabPanel 1
                           ) # navbarPage
) # ui


# Define server logic required to draw a histogram

# SERVER ------------------------------------------------------------------
server <- function(session, input, output) {
  

# observeEvent info$Topics ------------------------------------------------
  observeEvent(input$Topics, {
    #browser()
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
    #browser()
    
    #Names <- names(Pop_and_Physical2021)
    #names(Pop_and_Physical2021)[c(1,which(str_detect(names(Pop_and_Physical2021), "מרחקים|סקר")))]
    
  
    
    db <- Pop_and_Physical2021 %>% 
      filter(`שם הרשות` %in% input$towns) %>% 
      filter(`דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`>= as.numeric(input$TownSizeSlider[1]), `דמוגרפיה: סה"כ אוכלוסייה בסוף השנה`<= as.numeric(input$TownSizeSlider[2]))
     # select(1, matches(paste(input$Topics, collapse = "|")))
    
    if (input$size1 != "none") {
      db <- db %>% mutate(s0 = .data[[input$size1]])
    } else {db <- db %>% mutate(s0 = "")}
    
    if (input$color1 != "none") {
      db <- db %>% mutate(c0 = .data[[input$color1]])
    } else {db <- db %>% mutate(c0 = "")}
    
    if (input$PopAdjustX & !str_detect(input$xaxis1, "מתוקנן") & !str_detect(input$xaxis1, "אחוז") & !str_detect(input$xaxis1, "ל-1000")) {
      db <- db %>% 
        mutate(x0 = .data[[input$xaxis1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]])
    } else {
      db <- db %>% mutate(x0 = .data[[input$xaxis1]])
    }
    if (input$PopAdjustY & !str_detect(input$yaxis1, "מתוקנן") & !str_detect(input$yaxis1, "אחוז")& !str_detect(input$yaxis1, "ל-1000")) {
      db <- db %>% 
        mutate(y0 = .data[[input$yaxis1]] / .data[["דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה" ]])
    } else {
      db <- db %>% mutate(y0 = .data[[input$yaxis1]])
    }
    
    
    if (!input$BarPlot) { # do a scatterplot
    
    p <- db %>% 
      #filter(` שם_הרשות` %in% input$towns) %>% 
      #mutate(y0 = .data[[input$yaxis1]]) %>%  
             #s0 = ifelse(input$size1 != "none", .data[[input$size1]], ""), c0 = ifelse(input$color1 != "none",.data[[input$color1]], 99999.88888)) %>% 
      #select(1, x0, y0, s0, c0) %>% 
      #mutate(across(c(x0, y0, s0, c0), ~ ifelse((is.numeric(.)) , prettyNum(.,  scientific = FALSE, big.mark = ","), .))) %>% 
      mutate(text =  paste0(`שם הרשות`  , "<br>", 
                            input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ","), "<br>", 
                            input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ","), "<br>", 
                            input$size1, " ", prettyNum(s0, scientific = F, big.mark = ","), "<br>", 
                            input$color1, " ", prettyNum(c0, scientific = F, big.mark = ","))) %>% 
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
      ) +
      geom_text(vjust = -1, aes(label =  `שם הרשות`  , y = y0 + 
                                  0.02*(max(y0, na.rm = T) - min(y0, na.rm = T))), size = 2)
    
    if (input$color1 == "none" & input$size1 == "none") {
      p <- p + geom_point(color = "darkblue", alpha = 0.5)
    } else if (input$color1 == "none") {
      p <- p + geom_point(aes(size = .data[[input$size1]]),color = "darkblue", alpha = 0.5) + scale_size_area()
    } else if (input$size1 == "none") {
      p <- p + geom_point(aes(color = .data[[input$color1]])) + scale_color_viridis_c()
    } else {
      p <- p + geom_point(aes(color = .data[[input$color1]], size = .data[[input$size1]])) + scale_color_viridis_c() + scale_size_area()
    }
    
    p <- p +
      labs(
        x = names3 %>% filter(N3 == input$xaxis1) %>% pull(N4),
        y = names3 %>% filter(N3 == input$yaxis1) %>% pull(N4),
        color = NULL
      ) 
    
    }  else { # do a bar plot
     
    p <- db %>% 
      mutate(text =  paste0(`שם הרשות`  , "<br>", 
                           # input$xaxis1, " ", prettyNum(x0, scientific = F, big.mark = ","), "<br>", 
                            input$yaxis1, " ", prettyNum(y0, scientific = F, big.mark = ","), "<br>", 
                            input$size1, " ", prettyNum(s0, scientific = F, big.mark = ","), "<br>", 
                            input$color1, " ", prettyNum(c0, scientific = F, big.mark = ","))) %>% 
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
      geom_text(aes(label =  paste0( `שם הרשות`  ," ", prettyNum(y0, scientific = F, big.mark = ",")), x = y0 - 
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
  }

    

    output$p1i <- renderPlotly({
      if (!input$BarPlot) {
        ggplotly(p, height = 600, width = 1000, tooltip = "text", dynamicTicks = TRUE) %>% 
        
          # layout(annotations = 
          #          list(x = 0.02, y = 0.02, text = CaptionCPI, 
          #               showarrow = F, xref='paper', yref='paper', 
          #               xanchor='left', yanchor='auto', xshift=0, yshift=0,
          #               font=list(size=15, color="black"))
          #) %>% 
          config(displayModeBar = FALSE)
      } else {
     # ggplotly(p, height = 600, width = 1000, tooltip = "text", dynamicTicks = TRUE) %>% 
        ggplotly(p, height = 600, width = 1000, tooltip = "text") %>% 
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
    
    
    c1 <- Comments1r %>% mutate(Yeap = str_detect(input$xaxis1, character)) %>% filter(Yeap) %>% #slice(1) %>% 
      pull(comment)
    if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$xaxis1, ": <b><br>", c1, "</b><br>") }
    if (input$xaxis1 != input$yaxis1) {
      
      c1 <- Comments1r %>% mutate(Yeap = str_detect(input$yaxis1, str_trim(character))) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$yaxis1, ": <b><br>", c1, "</b><br>") }
    }
    if (input$size1 != input$yaxis1 & input$size1 != input$xaxis1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(input$size1, character)) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$size1, ": <b><br>",  c1, "</b><br>") }
    }
    if (input$color1 != input$yaxis1 & input$color1 != input$xaxis1 & input$color1 != input$size1) {
      c1 <- Comments1r %>% mutate(Yeap = str_detect(input$color1, character)) %>% filter(Yeap) %>% #slice(1) %>% 
        pull(comment)
      if (length(c1)>0) {Comments1label <- paste0(Comments1label, "<br>", input$color1, ": <b><br>", c1, "</b><br>") }
    }
    #browser()
    if (first(Comments1label) != "") {Comments1label <- paste0("הערות:", "<br>", Comments1label, collapse = "<br>")}
    
    Comments1label <- paste0("ציר Y: <b>", input$yaxis1, ifelse(input$PopAdjustY, " (מתוקנן לאוכלוסיה)", ""),"</b><br>",
                             "ציר X: <b>", input$xaxis1, ifelse(input$PopAdjustX, " (מתוקנן לאוכלוסיה)", ""), "</b><br>",
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
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
