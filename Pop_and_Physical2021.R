# Main Data files for years 1999-2021 can be found at:
# https://www.cbs.gov.il/he/publications/Pages/2019/%D7%94%D7%A8%D7%A9%D7%95%D7%99%D7%95%D7%AA-%D7%94%D7%9E%D7%A7%D7%95%D7%9E%D7%99%D7%95%D7%AA-%D7%91%D7%99%D7%A9%D7%A8%D7%90%D7%9C-%D7%A7%D7%95%D7%91%D7%A6%D7%99-%D7%A0%D7%AA%D7%95%D7%A0%D7%99%D7%9D-%D7%9C%D7%A2%D7%99%D7%91%D7%95%D7%93-1999-2017.aspx
# https://www.cbs.gov.il/he/publications/Pages/2019/הרשויות-המקומיות-בישראל-קובצי-נתונים-לעיבוד-1999-2017.aspx

# There are also files with immigration between major cities
# גם קבצי הגירה פנימית לתל אביב, ירושלים, חיפה, אשדוד, באר שבע, בני ברק, נתניה, פתח תקוה, ראשון לציון



# first sheet -------------------------------------------------------------

library(HovavLoadPackage5)
library(readxl)

Pop_and_Physical2021 <- read_excel("p_libud_21.xlsx", 
                         sheet = "נתונים פיזיים ונתוני אוכלוסייה ", 
                         skip = 3) #%>% janitor::clean_names(case = "none", ascii = F)

PopCat <- read_excel("p_libud_21.xlsx", 
           sheet = "נתונים פיזיים ונתוני אוכלוסייה ", 
           skip = 1, n_max = 1)

PopCat <- tibble(Cat = names(PopCat)) %>% 
  bind_rows(tibble(Cat = rep(NA_character_, ncol(Pop_and_Physical2021) -  length(PopCat)))) %>% 
  mutate(Cat = ifelse(str_detect(Cat, "\\.\\.\\."), NA, Cat)) %>% 
  fill(Cat) %>% 
  mutate(Cat = str_replace_all(Cat, "\r\n", " ")) %>% 
  mutate(Cat = str_trim(Cat)) %>% 
  mutate(Cat = ifelse(is.na(Cat), "", paste0(Cat,": ")))

PopCat %>% count(Cat)

names1 <- names(Pop_and_Physical2021)
names2 <- Pop_and_Physical2021[1,] %>% unlist %>% unname

names3 <- tibble(N0 = PopCat$Cat, N1 = names1, N2 = names2) %>% 
  #mutate(N3 = ifelse(str_detect(N1, "X"), NA, N1), N2 = ifelse(is.na(N2), "", N2)) %>% 
  mutate(N3 = ifelse(str_detect(N1, "\\.\\.\\.") &!str_detect(N1, "ערך"), NA, N1), N2 = ifelse(is.na(N2), "", N2)) %>% 
  fill(N3) %>% 
  mutate(N4 = paste0(":", str_replace(N0, ":", ""), "<br>", N3, "<br>", N2), N3 = paste(N0, N3, N2)) %>% 
  #mutate(N4 = paste0(N0, "<br>", N3, "<br>", N2), N3 = paste(N0, N3, N2)) %>% 
  mutate(N3 = str_replace_all(N3, "\r\n", " "), N4 = str_replace_all(N4, "\r\n", " ")) %>% 
  mutate(N3 = str_replace_all(N3, "  ", " "), N4 = str_replace_all(N4, "  ", " ")) %>% 
  mutate(N3 = str_replace_all(N3, "  ", " "), N4 = str_replace_all(N4, "  ", " ")) %>% 
  mutate(N3 = str_trim(N3), N4 = str_trim(N4)) 
  # mutate(N3 = str_replace_all(N3, "5_000", "5,000")) %>%
  # mutate(N3 = str_replace_all(N3, "1_000", "1,000")) %>%
  # mutate(N3 = str_replace_all(N3, "100_000", "100,000")) %>%
  # mutate(N3 = str_replace_all(N3, "2_000", "2,000")) %>%
  # mutate(N3 = str_replace_all(N3, "4_999", "4,999")) %>%
  # mutate(N3 = str_replace_all(N3, "19_999", "19,999")) %>%
  # mutate(N3 = str_replace_all(N3, "9_999", "9,999")) %>%
  # mutate(N3 = str_replace_all(N3, "_", " ")) %>% 
  # mutate(N3 = str_trim(N3)) %>% 
  #pull(N3)

names(Pop_and_Physical2021) = names3 %>% pull(N3)

Pop_and_Physical2021 %>% 
  #select(contains("שטח")) %>% 
  #select(contains("צפיפות")) %>% 
  #select(contains("אחוזים")) %>% 
  #select(contains("בני")) %>% 
  #select(contains("כולל")) %>% 
  #select(contains("פטירות")) %>% 
  #select(contains("נכנסים")) %>% 
  select(contains("תושבים")) %>% 
  slice(1) %>% t()

Pop_and_Physical2021 <- Pop_and_Physical2021 %>% 
  mutate(across(c(contains("שטח"), contains("צפיפות"), contains("אחוז"), contains("בני"), contains("כולל"), contains("פטירות"), contains("נכנסים"), contains("יוצאים"), contains("מרחק"), contains("שנת_קבלת"),
                  contains("נישאים"), contains("מתגרשים"), contains("השתקעות"), contains("תוחלת"), contains("סוכרת"), contains("סרטן"),
                  contains("אבטלה"), contains("הכשרה"), contains("קצבאות"), contains("סרטן"), contains("גמלת"), contains("גמלאות"),
                  contains("קצבאות"), contains("שכר"), contains("בגנים"), contains("ספר"), contains("ביניים"), contains("תלמידים"),
                  contains("כיתות"), contains("הוראה"), contains("מים"), contains("כבישים"), contains("צינורות"), contains("פסולת"),
                  contains("שפכים"), contains("דרכים"), contains("הרוגים"), contains("תושבים"), contains("יישובים"), contains("מושבים"),
                  contains("קיבוצים"), contains("דרכים"), contains("הרוגים"), contains("תושבים"), contains("יישובים"), contains("מושבים"),
                  ), as.numeric))

# tibble(N1 = names1, N2 = names2) %>% 
#   mutate(N3 = ifelse(str_detect(N1, "X"), NA, N1), N2 = ifelse(is.na(N2), "", N2)) %>% 
#   fill(N3) %>% 
#   mutate(N3 = paste(N2, N3)) %>% 
#   select(N3) %>% 
#   filter(str_detect(N3, "_000|_500"))


Pop_and_Physical2021 <- Pop_and_Physical2021 %>% slice(-1)
Pop_and_Physical2021 <- Pop_and_Physical2021 %>% 
  mutate(`דמוגרפיה: מאזן הגירה ביישוב מתוקנן ל 1000 תושבים` = `דמוגרפיה: מאזן הגירה ביישוב סה\"כ` / `דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה` * 1000, .after = `דמוגרפיה: מאזן הגירה ביישוב סה\"כ`) %>% 
  mutate(`דמוגרפיה: מאזן הגירה פנימית מתוקנן ל 1000 תושבים` = `דמוגרפיה: מאזן הגירה פנימית` / `דמוגרפיה: סה\"כ אוכלוסייה בסוף השנה` * 1000, .after = `דמוגרפיה: מאזן הגירה פנימית`) 

names(Pop_and_Physical2021)

Pop_and_Physical2021 %>% select_if(is.numeric)
Pop_and_Physical2021 %>% select_if(function(x) !is.numeric(x)) %>% View


# read comments  ----------------------------------------------------------

# library(openxlsx)
# wb <- loadWorkbook("p_libud_21.xlsx")
# sheet_names <- names(wb)
# sheet_names[2]
# sheet <- read.xlsx("p_libud_21.xlsx", sheet = sheet_names[2])

library(tidyxl)

# Import the Excel file
df <- read_excel("p_libud_21.xlsx", sheet = "נתונים פיזיים ונתוני אוכלוסייה ")
x <- xlsx_cells("p_libud_21.xlsx", sheets = "נתונים פיזיים ונתוני אוכלוסייה ")
x[!is.na(x$comment), c("address", "character","comment")] %>% View()
Comments1 <- x[!is.na(x$comment), c("address", "row", "col", "character","comment")]

Comments1r2 <- Comments1 %>% 
  filter(row == 2) %>% 
  mutate(character = case_when(
    col == 232 ~ "פשיעה ומשפט",
    col == 244 ~ "שימושי קרקע",
    TRUE ~ character
  ))

Comments1r4 <- Comments1 %>% 
  filter(row == 4) %>% 
  mutate(character = case_when(
    col == 76 ~ "אוכלוסיית דיור משותף",
    col == 118 ~ "שכר ממוצע לחודש של שכירים",
    col == 152 ~ "אחוז תלמידים נושרים",
    col == 220 ~ "תאונות דרכים עם נפגעים לפי חומרה",
    col == 152 ~ "מבוגרים תושבי ישראל המורשעים בדין לפי קבוצת עבירה",
    TRUE ~ character
  ))

Comments1r5 <- Comments1 %>% 
  filter(row == 5) %>% 
  mutate(character = case_when(
    col == 129 ~ "ילדים בגנים של משרד החינוך",
    col == 134 ~ "בתי ספר",
    col == 144 ~ "תלמידים",
    #col == 220 ~ "תאונות דרכים עם נפגעים לפי חומרה",
    #col == 152 ~ "מבוגרים תושבי ישראל המורשעים בדין לפי קבוצת עבירה",
    TRUE ~ character
  ))

Comments1r <- bind_rows(Comments1r2, Comments1r4, Comments1r5)
# 
# x[x$row == 4 & ((x$col %in% Comments1$col) & (Comments1$row == 5)),c("address", "character","comment", "row", "col")] %>% drop_na(character)
# Comments1 %>% counts(row)


# 2nd sheet -------------------------------------------------------------

library(HovavLoadPackage5)
library(readxl)

namesHR1 <- tibble(
  N1 = read_excel("p_libud_21.xlsx", 
                  sheet = "סקרי כוח אדם והוצאות משק בית", 
                  skip = 1) %>% names,
  N2 = read_excel("p_libud_21.xlsx", 
                  sheet = "סקרי כוח אדם והוצאות משק בית", 
                  skip = 2) %>% names,
  N3 = read_excel("p_libud_21.xlsx", 
                  sheet = "סקרי כוח אדם והוצאות משק בית", 
                  skip = 3) %>% names,
  N4 = read_excel("p_libud_21.xlsx", 
                  sheet = "סקרי כוח אדם והוצאות משק בית", 
                  skip = 4) %>% names,
) %>% 
  mutate(
    N1 = ifelse(str_detect(N1, "\\.\\.\\."), NA, N1),
    N2 = ifelse(str_detect(N2, "\\.\\.\\."), NA, N2),
    N3 = ifelse(str_detect(N3, "\\.\\.\\."), NA, N3),
    N4 = ifelse(str_detect(N4, "\\.\\.\\."), NA, N4)
  ) %>% 
  fill(N1) %>% 
  fill(N2) %>% 
  fill(N3) %>% 
  fill(N4) %>% 
  mutate(N5 = paste0(N1, ": ",
                     N3, ", ",
                     N4
                     )) %>%
  mutate(N6 = paste0(N1, "<br>",
                     N3, "<br>",
                     N4
  )) %>% 
  mutate(across(c(N5, N6), ~ str_replace_all(., "NA", ""))) %>% 
  #mutate(N6 = paste0(N6, "<br>נתונים עבור ערים המונות 50,000 תושבים ויותר")) %>% 
  mutate(across(c(N5, N6), ~ ifelse(row_number() == 1, "שם הרשות", .))) 

SekerHR <- read_excel("p_libud_21.xlsx", 
                                   sheet = "סקרי כוח אדם והוצאות משק בית", 
                                   skip = 6) #%>% janitor::clean_names(case = "none", ascii = F)
names(SekerHR) <- namesHR1$N5
SekerHR <- SekerHR %>% mutate(across(-1, as.numeric))
Pop_and_Physical2021 %>% select_if(function(x) !is.numeric(x)) %>% View

Pop_and_Physical2021 <- left_join(Pop_and_Physical2021, SekerHR %>% select(-2))

names3b <- namesHR1 %>% 
  select(N5, N6) %>% 
  rename(N3 = N5, N4 = N6) %>% 
  mutate(N0 = "", N1 = "", N2 = "")

names3 <- bind_rows(names3, names3b)


# 3rd sheet נתוני תקציב-------------------------------------------------------------

library(HovavLoadPackage5)
library(readxl)

namesBDG1 <- tibble(
  N1 = read_excel("p_libud_21.xlsx", 
                  sheet = "נתוני תקציב", 
                  skip = 2) %>% names %>% str_remove("\\.\\.\\.(.+)"),
  N2 = read_excel("p_libud_21.xlsx", 
                  sheet = "נתוני תקציב", 
                  skip = 3) %>% names %>% str_remove("\\.\\.\\.(.+)"),
  
) %>% 
  # mutate(
  #   N1 = ifelse(str_detect(N1, "\\.\\.\\."), NA, N1),
  #   N2 = ifelse(str_detect(N2, "\\.\\.\\."), NA, N2),
  #   N3 = ifelse(str_detect(N3, "\\.\\.\\."), NA, N3),
  #   N4 = ifelse(str_detect(N4, "\\.\\.\\."), NA, N4)
  # ) %>% 
  mutate(across(c(N1, N2), ~ ifelse(. == "", NA, .))) %>% 
  fill(N1) %>% 
  fill(N2) %>% 
  # fill(N3) %>% 
  # fill(N4) %>% 
  mutate(N5 = paste0(
    "נתוני תקציב - ",
    N1, ": ",
                     N2
  )) %>%
  mutate(N6 = paste0(  "נתוני תקציב - ", N1, "<br>",
                     N2
  )) %>% 
  mutate(across(c(N5, N6), ~ str_replace_all(., "NA", ""))) %>% 
  #mutate(N6 = paste0(N6, "<br>נתונים עבור ערים המונות 50,000 תושבים ויותר")) %>% 
  mutate(across(c(N5, N6), ~ ifelse(row_number() == 1, "שם הרשות", .))) 

Budget <- read_excel("p_libud_21.xlsx", 
                      sheet = "נתוני תקציב", 
                      skip = 3) #%>% janitor::clean_names(case = "none", ascii = F)
names(Budget) <- namesBDG1$N5
Budget <- Budget %>% mutate(across(-1, as.numeric))

#Pop_and_Physical2021 %>% select_if(function(x) !is.numeric(x)) %>% View

Pop_and_Physical2021 <- left_join(Pop_and_Physical2021, Budget %>% select(-2))

names3c <- namesBDG1 %>% 
  select(N5, N6) %>% 
  rename(N3 = N5, N4 = N6) %>% 
  mutate(N0 = "", N1 = "", N2 = "")

names3 <- bind_rows(names3, names3c)

# read comments  Budget ----------------------------------------------------------

library(tidyxl)

# Import the Excel file
df1 <- read_excel("p_libud_21.xlsx", sheet = "נתוני תקציב")
x1 <- xlsx_cells("p_libud_21.xlsx", sheets = "נתוני תקציב")
x[!is.na(x$comment), c("address", "character","comment")] %>% View()
Comments2 <- x1[!is.na(x1$comment), c("address", "row", "col", "character","comment")]

Comments2r4 <- Comments2 %>% 
  filter(row == 4)

Comments1r <- bind_rows(Comments1r2, Comments1r4, Comments1r5, Comments2r4)
# 
# x[x$row == 4 & ((x$col %in% Comments1$col) & (Comments1$row == 5)),c("address", "character","comment", "row", "col")] %>% drop_na(character)
# Comments1 %>% counts(row)


# save data ---------------------------------------------------------------

save(Pop_and_Physical2021, Comments1r, names3, file = "MunicipalData.rda")

