####----Link for APP webscrapes data Monday night---####
gitlink <- "https://gitlab.com/coolsoftwaredev/drug_checking_data_scraper/-/jobs/artifacts/master/raw/dcbc.csv?job=run"

#Read in the file
dcbc <- read.csv(gitlink, na.strings=c("N/A",""), stringsAsFactors=FALSE)
####---Libraries & Functions---####
library(tidyverse)
library(lubridate)
library(igraph)
library(RColorBrewer)
#install.packages("plyr") #I use this for count because I'm too lazy to learn tidyverse
`%notin%` <- Negate(`%in%`) #For laziness
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})     #Just for ggplot
} 

####---Dates----####
#Creates a data frame of date ranges to be used later for making sliding scale in network graph and line chart
everydate <- seq(ymd('2019-12-29'),floor_date(Sys.Date(), unit = "week", week_start = 1)+days(7),by = '1 day')
poss.w <- data.frame(Days = everydate, 
                     Bottom = floor_date(everydate, unit = "week", week_start = getOption("lubridate.week.start", 1)))
poss.w$Top <- poss.w$Bottom+days(6)

new_dat <- data.frame(value = unique(poss.w$Top)) %>%
  rowid_to_column("ID")
poss.w$ID <- new_dat$ID[match(poss.w$Top, new_dat$value)]
rm(new_dat)
poss.w$Days2 <- paste(month(poss.w$Bottom, label = TRUE)," ", day(poss.w$Bottom),
                      "-\n", month(poss.w$Top, label = TRUE)," ", day(poss.w$Top),"\n", year(poss.w$Top),sep = "")
poss.w$Days2 <- ordered(poss.w$ID, labels = unique(poss.w$Days2))

####----General Cleaning from Scrape----####
dcbc$FTIR.Spectrometer<- gsub("\\Q['\\E|\\Q']\\E|\\Q'\\E", "", dcbc$FTIR.Spectrometer)
dcbc <- separate(dcbc, FTIR.Spectrometer ,
                 into = c("FTIR.1", "FTIR.2", "FTIR.3", "FTIR.4", "FTIR.5", "FTIR.6"),
                 sep = ", ", extra = "merge",
                 fill = "right", remove = FALSE)

###---All opioids----####
op2 <- c(unique(dcbc$Expected.Substance[grep("[Ff]ent", dcbc$Expected.Substance)]),
         unique(dcbc$Expected.Substance[grep("Her", dcbc$Expected.Substance)]),
         "Down (Unknown Opioid)", "Oxycodone", "Methadone", "Morphine", "Heroin HCl",
         "6-Monoacetylmorphine", "Oxycodone HCl", "Fentanyl HCl", "Fentanyl or Analog")

#####----More Cleaning ----####
#This section could be rewritten
dcbc2 <- dcbc #backup, just in case
dcbc2 <- dcbc2[dcbc2$City.Town == "Vancouver" & dcbc2$Date > as.Date("2019-12-28"),]
dcbc2$Date <- as.Date(dcbc2$Date, format = "%Y-%m-%d")
poss.w <- poss.w[poss.w$Days >= min(dcbc2$Date) & poss.w$Days <= date(head(dcbc2$Date, n = 1)),]
dcbc2$fent.p <- 0
dcbc2$fent.p[grepl("[Ff]ent", dcbc2$FTIR.Spectrometer)] <- 1
dcbc2$fent.p[which(dcbc2$Fentanyl.Test.Strip == "True")] <- 1

#Change test strips to binary
dcbc2$Benzo.Test.Strip[dcbc2$Benzo.Test.Strip == "True"] <- "1"
dcbc2$Benzo.Test.Strip[dcbc2$Benzo.Test.Strip == "False"] <- "0"
dcbc2$Benzo.Test.Strip <- as.numeric(dcbc2$Benzo.Test.Strip)


####----Finds all the benzos & fent to create a count----####
benzo.match <- unlist(dcbc2[,c(8:13)]) %>%
  .[grepl("epam|olam", .)]
benzo.match <- unique(benzo.match)
dcbc2$has.benzo <- 0

#Finds all benzo positive strips which do not have positive benzo responses
dcbc2$has.benzo[dcbc2$FTIR.1 %notin% benzo.match & dcbc2$Benzo.Test.Strip ==1 &
                  dcbc2$FTIR.2 %notin% benzo.match & dcbc2$FTIR.3 %notin% benzo.match &
                  dcbc2$FTIR.4 %notin% benzo.match & dcbc2$FTIR.5 %notin% benzo.match &
                  dcbc2$FTIR.6 %notin% benzo.match] <- 1
#loop finds the positive fent and benzo strips where they are not listed
#Answer from stackoverflow: https://stackoverflow.com/a/64994253/7263991
cols_have <- paste("FTIR", 1:6, sep=".")
colidx <- names(dcbc2) %in% cols_have
for(i in 1:2){
  ci <- seq_along(dcbc2[colidx])[max.col(cbind(is.na(dcbc2[colidx]), TRUE), ties.method = "first")]
  if(i == 1){
    ri <- rowSums(sapply(dcbc2[colidx], `%in%`, op2)) == 0 &  dcbc2$fent.p == 1
    dcbc2[colidx][na.omit(cbind(which(ri), ci[ri]))] <- "Fent <5%"
  }
  if(i==2){
    benzo.match2 <- c(benzo.match,"Fent <5%")
    ri <- rowSums(sapply(dcbc2[colidx], `%in%`, benzo.match2)) == 0 &  dcbc2$has.benzo == 1
    dcbc2[colidx][na.omit(cbind(which(ri), ci[ri]))] <- "Benzodiazepine <5%"
  }
}
#In order to compute the data all FTIR.2 cannot be empty - no cuts, represents when no adulterants were found
dcbc2$FTIR.2[which(is.na(dcbc2$FTIR.2))] <- paste("No Cuts\n", dcbc2$FTIR.1[which(is.na(dcbc2$FTIR.2))], sep = "")
dcbc2$FTIR.1[which(grepl("No Cuts", dcbc2$FTIR.2))] <- dcbc2$FTIR.2[which(grepl("No Cuts", dcbc2$FTIR.2))]
dcbc2 <- dcbc2[dcbc2$FTIR.1 != "",] %>%
  mutate(across(where(is.character), str_trim)) %>%
  rowid_to_column("ID")

dcbc2$Week.val <- poss.w$Days2[match(dcbc2$Date, poss.w$Days)]
rownames(dcbc2) <- NULL
####---String replacement!----####

dcbc2 <- dcbc2 %>%
  mutate(Expected.Substance = gsub("Down \\Q(Unknown Opioid)\\E|Fentanyl","Fentanyl/Down",
                                   Expected.Substance)) %>%
  pivot_longer(cols = c(FTIR.1:FTIR.6),
               names_to = "name",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = str_replace_all(value,c("Fentanyl HCl" = "Fentanyl or Analog", 
                                         "Dextromethorphan" = "DXM",
                                         "6-Monoacetylmorphine" = "6-MAM", "Surcrose" = "Sucrose", 
                                         "Polyethylene Glycol" = "PEG", "Lysergic Acid Diethylamide" = "LSD", 
                                         "Cocaine Base" = "Crack Cocaine", "etizolam" = "Etizolam"
  ))) %>%
  mutate(value = str_replace(value,"Sugar Possibly [A-Za-z]+", "Sugar Uncertain")) %>%
  mutate(value = str_replace(value, "Heroin \\Q(Trace)\\E", "Heroin")) %>%
  mutate(value = gsub("Uncertain Oil|Sugar Uncertain|Uncertain Carbohydrate",
                      "Uncertain Oil/Carb/Sugar", value)) %>%
  mutate(value = str_replace(value, " HCl", "")) %>%
  mutate(value = str_replace(value, "Fentanyl$", "Fentanyl or Analog"))

#Remake this variable after the edits
op2 <- c(unique(dcbc2$Expected.Substance[grep("[Ff]ent", dcbc2$Expected.Substance)]),
         unique(dcbc2$Expected.Substance[grep("Her", dcbc2$Expected.Substance)]),
         "Down (Unknown Opioid)", "Oxycodone", "Methadone", "Morphine", "Heroin HCl",
         "6-Monoacetylmorphine", "Oxycodone HCl")

####----Grouped Variables/drugs I care about----####
all_opioids <- dcbc2[dcbc2$Expected.Substance %in% op2,]
all_opioids$Expected.Substance <- "All Opioids (Grouped)"
all_opioids$ID <- all_opioids$ID +1000000

op2 <- op2[op2 != "Fentanyl/Down"]
min_down2 <- dcbc2[which(dcbc2$Expected.Substance %in% op2),]
min_down2$Expected.Substance <- "Opioids Minus Fentanyl (Grouped)"
min_down2$ID <-min_down2$ID + 200000
true_op <- all_opioids %>%
  pivot_wider(names_from=name, values_from=value)
dcbc2 <- rbind(dcbc2, min_down2, all_opioids)
dcbc2 <- dcbc2[order(dcbc2$Week.val),]

#Limits the number of in the dropdown
interest <- c("Fentanyl/Down", "Opioids Minus Fentanyl (Grouped)", "All Opioids (Grouped)", "Methamphetamine",
              "Ketamine", "Cocaine", "Crack Cocaine", "MDMA")

dcbc2 <- dcbc2[dcbc2$Expected.Substance %in% interest,]       #subset so to include a handful
#
#Regrouping variables

####----Stuff for ggraph - same things as ggplot####


#Called here instead to not load into R until needed
#This file was written by hand, and could use some additions
#Categories were based off looking at fentanyl samples
source("Drug Classification.R")
#Need to add, "no cuts" into Drug Classification
type.of.drug2 <- type.of.drug %>%
  mutate(Drug.Name = paste("No Cuts",Drug.Name, sep = " "))

type.of.drug <- rbind(type.of.drug, type.of.drug2)
type.of.drug$Drug.Name <- gsub("(No Cuts) ", "\\1\n", type.of.drug$Drug.Name)

rownames(type.of.drug) <- NULL
rm(type.of.drug2)

###Creates df for classification and the colour palette####
df_sub <- which(names(dcbc2)%in%c("Week.val", "Expected.Substance", "value"))

node_col <- data.frame(Names = unique(dcbc2$value)) %>%
  rowid_to_column("ID")

node_col$Classification <- type.of.drug$Classification[match(node_col$Names,type.of.drug$Drug.Name)]
#Since the dictionary is handwritten  this next line just makes sure it won't throw errors
node_col$Classification[is.na(node_col$Classification)] <- "new_val" 
regrouped <- data.frame(ID = seq(2000, 1999+length(unique(node_col$Classification)),by=1),
                        Names = unique(unique(node_col$Classification)),
                        Classification = unique(unique(node_col$Classification)))
node_col <- rbind(node_col, regrouped)


coul  <- brewer.pal(length(unique(node_col$Classification)), "Set3")
my_colors <- coul[as.numeric(as.factor(unique(node_col$Classification)))]
names(my_colors) <- unique(node_col$Classification)

###-----------------------Notes----------------------------------------------####
#Weight needs to be divided by 2 when "No Cuts" as it counts FTIR.1 & FTIR.2
#
#
#
####-------------------------------------------------------------------------####
interest <- c("Fentanyl/Down", "Opioids Minus Fentanyl (Grouped)", "All Opioids (Grouped)",
              "Methamphetamine",
              "Ketamine", "Cocaine", "Crack Cocaine", "MDMA")

###---BENZOS -----####
benzo <- dcbc2 %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(Week.val, Expected.Substance, fent.p, has.benzo) %>%
  group_by(Week.val, Expected.Substance) %>%
  summarize(Week.val = unique(Week.val),
            Expected.Substance = unique(Expected.Substance),
            fent.count = sum(fent.p), benzo = sum(has.benzo),  tot = n()) %>%
  ungroup()

filler <- data.frame(Expected.Substance = rep(interest, length(poss.w$Days2[poss.w$Days2 %notin% benzo$Week.val])),
                     Week.val = rep(poss.w$Days2[poss.w$Days2 %notin% benzo$Week.val], length(interest)),
                     fent.count = 0, benzo= 0, tot = 0)


filler <- filler %>%
  unite("new",Expected.Substance:Week.val, sep = "AZQ")
benzo <- benzo %>%
  unite("new",Expected.Substance:Week.val, sep = "AZQ")
benzo <- rbind(benzo, filler[filler$new %notin% benzo$new,]) %>%
  separate(new, into = c("Expected.Substance", "Week.val"), sep = "AZQ")

benzo$fent.perc <- benzo$fent.count/benzo$tot*100
benzo$benzo.perc <- benzo$benzo/benzo$tot*100

benzo$Days2 <- poss.w$Days2[match(benzo$Week.val, poss.w$Days2)]
benzo <- benzo %>%
  pivot_longer(c(fent.count, benzo, fent.perc, benzo.perc), names_to = "name",
               values_to = "Percent") %>%
  mutate(name = str_replace_all(name, c("fent.count" = "Fentanyl Count",
                                        "fent.perc"= "% Fentanyl","benzo.perc" = "% Benzo",
                                        "benzo" = "Benzo Count"
  )))

#Days closed due to covid
closure <-  c("Mar 16-\nMar 23\n2020", "Mar 23-\nMar 29\n2020", "Mar 30-\nApr 05\n2020")
closure2 <- poss.w$Days2[poss.w$Days2 %in% closure]
benzo$Percent[grepl("\\Q%\\E", benzo$name) & benzo$tot < 20] <- NA
benzo$Percent[which(benzo$Week.val %in% closure)] <- NA
benzo$Percent[which(is.na(benzo$Percent))] <- NA

benzo <- benzo[order(benzo$Days2),]
poss.w <- poss.w %>%
  select(ID, Days2) %>%
  distinct(.)
get_id <- c(max(poss.w$ID)-1, max(poss.w$ID))


####----TO SHINY---####
library(ggraph)
library(tidygraph)
library(shiny)
library(shinyWidgets)
ui <- fluidPage(
  tabsetPanel(type = "tab",
              #Instruction Page
              tabPanel("Instructions",
                       mainPanel(
                         h1("Vancouver Drug Checking"),
                         p("It has been 5 years since BC first declared its overdose crisis. In 2017, after years of work by activists, small drug checking pilot projects began in Vancouver, where people who bought drugs from the illegal market could have them tested. They would then be relayed these results and told the approximate composition of the drug sample they submitted."),
                         p("In 2020 the team at the BCCSU made this data available here at",
                           a("drugcheckingbc.ca", href = "https://drugcheckingbc.ca/")),
                         p("This work could not have been done without extensive feedback, as well as advice from Karen Ward, who initially asked me for a weekly readout of the drug checking data. She's provided feedback throughout the design of the initial graphs - as well as requested the graphs found on the 'Benzo's and Opioids' page, specifically looking at the effects on supply & cheque day. I've also received some technical (and emotional) support from Gjalt-Jorn Peters, and Adam Palayew."),
                         p("While I've had some help this is a completely independent project I've done in my free time during the pandemic. All mistakes are mine."),
                         h1("How to Use this Drug Checking Tool"),
                         h2("Weekly Drug Checking Network - Network Visualization"),
                         p("If we want to know how many times a drug was tested, we would count the total number of times it was found in the data. For example, between December 30th and January 5th 2020, 40 samples believed to be fentanyl were tested in Vancouver. If we wanted to know how many other drugs were found in those drugs samples, we'd make something like a bar chart. But what if we wanted to know how many times drugs, fillers and adulerants were found in the same week? The network graph on the page 'Weekly Drug Checking Network' attempts to visualize the relationship between all substances found by the FTIR."),
                         p("Like so:"),
                         img(src='explanation_image.png', align = "left", height = "50%", width = "50%"),
                         p("The size of the",strong("node"),"represents the number of times that substance was found. The width of the", strong("edges"),"demonstrates how often they occurred together."),
                         p(strong("This doesn't mean that every sample contained all of these drugs."), "It just means that all these substances have been found when the drug when someone has brought in a sample to be tested."),
                         h3("Currently the app has these features"),
                         p("- A sliding scale to check results by week (you can look up to 3 weeks at a time)"),
                         p("- The Expected Substance, or what someone though they were testing"),
                         p("- The ability to regroup the classifications"),
                         p("- A Bar graph at the bottom that shows the counts for each substance"),
                         p("Currently the network graph works best for Fentanyl - and worse for the others... Such is life."),
                         h4("Notes about the classifications"),
                         p("The classifications are written by hand by me - N/A = Something irrelevant eg. 'water'. new_val = something I haven't coded by hand. In making the choice whether something was 'buff','stimulant' or 'other', caffeine for eg. is included under stimulant because it has stimulant properties. In fentanyl it might be closer to a buff, but in MDMA or methamphetamine it has compounding effects. The 'other' category includes other drugs that do not fall into the classification system. Eg. I don't have a dissasociatives category - so Ketamine is listed as other. Buff includes any non-psychoactive substances that may work as fillers/excipients or inactive adulterants."),
                         br(""),
                         h1("Benzos and Opioids Page"),
                         p("Page 2 is more straightforward. It calculates the % or Count of fentanyl/benzo's by week. The percentages are only available if the median number of tests per week is greater than 30. Percentages at less than 30 samples/week are more likely to be skewed.For example, if 1/5 tests of MDMA in a week were positive for fentanyl the % would be 20%."),
                         h1("Final Thoughts"),
                         p("Any visualization is a choice about how to cut, or a tool for thinking with data. Rather than just having these particular ways of visualizing the data, I'd like to create more options. If you are interested in learning more about R or about network visualization/analysis, I recommend Katherine Ognyanova's tutorial available",a("here", href = "https://kateto.net/netscix2016.html"), "or Jesse Sadler's tutorial", a("here", href = "https://www.jessesadler.com/post/network-analysis-with-r/")),
                           p("This is a first attempt at thinking about this question, but I'd love for others to join and help. I also have spent an incredible amount of time learning R to do this - If you're interested in helping develop this out, you can email me: alex.betsos@gmail.com"),
                         p("This code is made available under the GNU General Public License 3.0. The code is publicly available, but it comes with the stipulation that if you use it, changes you make must too be made available to the public. Drug data is part of our Common - it is made from the knowledge of people who use drugs, and as such, should be available to them. Where I have asked for help on stackoverflow, I have provided the link in the code comments. If you want to see/access the code, my github repo can be found here:", a("My Github Repo", href = "https://github.com/alexbetsos/DC_Shiny"))
                         #Network Graph
                       )),
              tabPanel("Weekly Drug Checking Network",
                       fluidRow(style='padding:0px',
                                column(width = 9,
                                       offset = 0.5, 
                                       sliderTextInput("Change",
                                                       label = NULL,
                                                       choices = as.character(poss.w$Days2),
                                                       selected = as.character(poss.w$Days2[poss.w$ID %in% get_id]),
                                                       width = "1200px",
                                                       force_edges = TRUE,
                                                       dragRange = TRUE)),
                                column(width = 2, 
                                       div(style = "height:10px"),
                                       selectInput("Drug",
                                                   "Expected Substance",
                                                   choices = interest,
                                                   selected = "Fentanyl/Down"))
                       ),
                       fluidRow(style='padding:0px',
                                column(width = 9, 
                                       offset = 0,
                                       plotOutput("net", width = "100%",
                                                  height = "750px")),
                                column(width = 2, offset = 0.5,
                                       checkboxGroupInput("regroup",
                                                          label = "Regroup Variables",
                                                          choices = regrouped$Classification,
                                                          selected = NULL),
                                       tableOutput("tafle"))),
                            
                       fluidRow(style='padding:50px', plotOutput("single")
                       )
              ),
              #Benzo % and Opioid %
              tabPanel("Benzos + Opioids",
                       fluidRow(column(width = 2,
                                       radioButtons("DC", "Select Drug", 
                                                    choices = interest,
                                                    selected = "Fentanyl/Down")),
                                column(width = 1, offset = 0,
                                       radioButtons("BF", "% Benzo or Fent",
                                                    choices = c("% Fentanyl", "% Benzo",
                                                                "Fentanyl Count", 
                                                                "Benzo Count"),
                                                    selected = "Benzo Count")),
                                column(width = 6, offset = 0, tableOutput("sumtable"))),
                       plotOutput("Perc", width = "100%", height = 500)
              )
  ))
#Server still needs to be fixed...
server <- function(input, output, session) {
  #1st makes reactive df for the rest of the project
  missing <- reactive({
    poss.w$ID[as.character(poss.w$Days2) == input$Change[2]]- poss.w$ID[as.character(poss.w$Days2) == input$Change[1]]
    
  })
  got_id <- reactive({as.vector(c(poss.w$ID[as.character(poss.w$Days2) == input$Change[2]],
                                  poss.w$ID[as.character(poss.w$Days2) == input$Change[2]]-3))
  })
  
  observeEvent(missing(), {
    
    new_selected <- c(as.character(poss.w$Days2[poss.w$ID %in% got_id()]))
    #c(as.character(poss.w$Days2[input$Change[2]]), as.character(poss.w$Days2[poss.w$ID == got_id]))
    
    if(missing() > 3){
      updateSliderTextInput(session,inputId = "Change", choices = as.character(poss.w$Days2), selected = new_selected)
      showNotification("Max Distance is 1 month")
      #ObserveEvent: https://stackoverflow.com/questions/45033432/limit-sliderinput-to-a-subrange
    }
    
  })
  df_react <- reactive({
    dcbc2 %>%
      filter(Expected.Substance == input$Drug & Week.val <= input$Change[2] & Week.val >=input$Change[1])
  })
  df_react2 <- reactive({
    if(!is.null(input$regroup)){
      df_react() %>%
        dplyr::rename(Names = value) %>%
        left_join(node_col[,c(2:3)]) %>%
        mutate(Classification2 = ifelse(Classification %in% input$regroup, Classification, Names)) %>%
        rename(value = Classification2)
    } else {
      df_react()
    }
  })
  
  #Nodes for the Social Network Visualization
  nodes <- reactive({
    node <-  df_react2() %>%
      select(value) %>%
      count(value) %>%
      dplyr::rename(Names = value, Weight = n) %>%
      left_join(node_col) %>%
      select(ID, Names, Weight, Classification) %>%
      arrange(desc(Weight))
    node$Weight[grepl("No Cuts", node$Names)] <- node$Weight[grepl("No Cuts", node$Names)]/2
    return(node)
  })
  #Edges for SN
  #The nesting solution was a huge help from a user on stackoverflow
  #This code doesn't work without it: https://stackoverflow.com/a/63083986/7263991
  edges2 <- reactive({
    df_react2() %>%
      select(ID, value) %>%
      nest(data=(value)) %>%
      mutate(pairs=map(data, ~as_tibble(t(combn(.$value, 2))), .name_repair=T, .keep)) %>%
      unnest(pairs) %>%
      select(V1, V2) %>%
      group_by(V1, V2) %>%
      summarise(amount = n()) %>%
      ungroup()
  })
  #3 dimensions
  edges3 <- reactive({
    dcbc3 <- df_react()
    if("FTIR.3" %in% dcbc3$name){
      dcbc3 <- dcbc3 %>%
        select(ID, name, value) %>%
        pivot_wider(names_from = name, values_from = value)
      dcbc3 <- dcbc3 %>%
        mutate(FTIR.3 = replace_na(FTIR.3, "NA")) %>%
        pivot_longer(-ID) %>%
        select(ID, value) %>%
        drop_na() %>%
        nest(data=(value)) %>%
        mutate(pairs=map(data, ~as_tibble(t(combn(.$value, 3))), .name_repair=T, .keep)) %>%
        unnest(pairs) %>%
        select(V1, V2, V3) %>%
        group_by(V1, V2,V3) %>%
        summarise(amount = n()) %>%
        ungroup() %>%
        unite("Drug", V1:V3, sep = ", ") %>%
        arrange(desc(amount))
    } else {
      dcbc3 <- data.frame(Drug = "No 3rd group", amount = 0)
    }
  })
  output$net <- renderPlot({
    edges <- edges2()
    validate(
      need(nrow(edges) >0.9, "Not tested During this Time")
    )
    colnames(edges) <- c("to", "from", "weight")
    edges$from <- nodes()$ID[match(edges$from, nodes()$Names)]
    edges$to <- nodes()$ID[match(edges$to, nodes()$Names)]
    edges <- select(edges, from, to, weight)
    
    g <- graph_from_data_frame(d = edges, vertices = nodes(), directed = FALSE) 
    g <- simplify(g, remove.loops = TRUE)
    if(input$Drug %in% c(V(g)$Names, "Fentanyl/Down", "All Opioids (Grouped)") & 
       nrow(edges) >=10){
      #Checks if there is just one graph or several
      if(is.connected(g) == FALSE){
        #if true then, it splits the main graph from the subgraphs
        c <- clusters(g); cn <- cbind(V(g), c$membership)
        lc <- which(which.max(c$csize)==c$membership);
        gs <- induced.subgraph(g, lc)
        if(input$Drug == "All Opioids (Grouped)"|input$Drug == "Fentanyl/Down"){
          st1 <- layout_as_star(gs, center = V(gs)$Names == "Fentanyl or Analog")
        }else{
          st1 <- layout_as_star(gs, center = V(gs)$Names == input$Drug)
        }
        st1 <- norm_coords(st1, xmin = -0.6, xmax = 0.6, 
                           ymin = -0.6, ymax = +0.6,
                           zmin = -0.6, zmax = +0.6)
        lc2 <- which(!which.max(c$csize)==c$membership)
        gs2 <- induced.subgraph(g, lc2)
        circ <- layout_in_circle(gs2)
        circ <- norm_coords(circ, xmin = -0.8, xmax = 0.8, 
                            ymin = -0.8, ymax = +0.8,
                            zmin = -0.8, zmax = +0.8)
        test2 <- rbind(st1,circ) 
        g <- gs %du% gs2
        t_lay <- create_layout(g, test2)
      }else{
        st1 <- layout_as_star(g, center = V(g)$Names == input$Drug)
        st1 <- norm_coords(st1, xmin = -0.8, xmax = 0.8, 
                           ymin = -0.8, ymax = +0.8,
                           zmin = -0.8, zmax = +0.8)
        t_lay <- create_layout(g, st1)
      }
      #For every other drug sample - still WIP
    } else {
      t_lay <- create_layout(g, layout = "nicely")
      
    }
    
    #Set graph space limits
    x_max <- max(t_lay$x)+0.1
    x_min <- min(t_lay$x)-0.1
    y_min <- min(t_lay$y)-0.1
    y_max <- max(t_lay$y)+0.1
    par(mar = c(0, 0, 0, 0))
    ggraph(t_lay) +
      geom_edge_link0(aes(width = E(g)$weight), colour = "grey") +   # add edges to the plot
      scale_edge_width_continuous(breaks = c(1, 5, 10, 25, 50,100),
                                  label = c(1, 5, 10, 25, 50, 100),
                                  range = c(1,20), name = "Frequency Found Together",
                                  limits = c(0,200),
                                  guide = guide_legend(order = 2, 
                                                       nrow = 1,
                                                       ncol =7)) +
      geom_node_point(aes(size = V(g)$Weight, color = V(g)$Classification)) +
      scale_color_manual(values = my_colors, name = "Class of Drug",
                         guide = guide_legend(order = 3, 
                                              ncol = 4,
                                              nrow = 2)) +
      coord_cartesian(ylim = c(y_min, y_max), xlim = c(x_min, x_max)) +
      geom_node_text(aes(label = V(g)$Names), size = 6) +
      scale_size(breaks = c(1,10,20,40, 60,80, 100), label=scales::number,
                 range = c(1,60), limits = c(1,300), name = "# of Times Drug Found \n in Test Results",
                 guide = guide_legend(order = 1,
                                      nrow = 4,
                                      ncol = 2,
                                      label.hjust =0.5)) +
      theme(legend.position= "right",
            legend.box.background = element_blank(),
            legend.direction = "vertical",
            legend.key = element_blank(),
            legend.background = element_rect(color = "green"),
            legend.text = element_text(size=12, hjust  = 0.4, inherit.blank = TRUE),
            legend.box.just = "top",
            legend.box = "vertical",
            legend.justification = "right",
            legend.box.spacing = unit(0.5,"cm"),
            legend.title.align = 0.2,
            legend.text.align = 0.4,
            legend.title=element_text(size=14),
            legend.key.width = unit(0.5, "cm"),
            legend.key.height = unit(0.2, "cm"),
            legend.spacing = unit(0.5, "cm"),
            panel.background = element_blank(),
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
            legend.margin = margin(0,0, 0, 0, unit = "cm"))+
      guides(color = guide_legend(override.aes = list(size=10),
                                  nrow = 5,
                                  ncol = 2))
    
  })
  
  output$tafle <- renderTable({
    
    top_node <- nodes() 
    validate(
      need(nrow(top_node) >1, "Not tested During this Time")
    )
    top_node <- top_node %>%
      select(Names, Weight)
    colnames(top_node) <- c("Drug", "amount")
    edge2 <- edges2() %>%
      unite("Drug", V1:V2, sep = ", ") %>%
      arrange(desc(amount))
    edge2 <- edge2[!grepl("No Cuts", edge2$Drug),]
    edges33 <- edges3()
    
    graph_table <- rbind(top_node[1,], edge2[1,], edges33[1,])
    
    Category <- c("Top Substance Found", "Top 2 Substance Combinations", "Top 3 Substance Combinations")
    graph_table <- cbind(Category, graph_table)
    xtable::xtable(graph_table, align = rep("c",4))
    
  })
  output$single <- renderPlot({
    date_change <- gsub("\\Q\n\\E", " ", input$Change)
    nodes() %>%
      mutate(Names = str_replace(Names, "No Cuts\\Q\n\\E", "")) %>%
      select(Names, Classification, Weight) %>%
      group_by(Names, Classification) %>%
      summarise(Weight = sum(Weight)) %>%
      ungroup() %>%
      arrange(Classification, Weight) %>%
      mutate(Names = factor(Names, levels = Names)) %>%
      ggplot(., aes(x =Names, y=Weight, fill = Classification))+
      geom_bar(stat = "identity") +
      scale_fill_manual(values = my_colors, name = "Class of Drug",
                        guide = guide_legend(order = 2, 
                                             nrow = 6,
                                             aes.overide = list(hjust = 0.5))) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(nodes()$Weight)+5)) +
      theme_classic() +
      theme(legend.position = "none") +
      labs(title = paste("Count of Drugs Found in", input$Drug, "Samples on",date_change, sep = " "), x = "Drugs",
           y = "Number of Occurrences") +
      coord_flip()
  })
  
  #Second Page data
  shiny.benzo <-reactive({benzo %>%
      filter(Expected.Substance == input$DC & name == input$BF)
  })

  output$Perc <- renderPlot({
    #Creates conditional to not run % if n too small
    a <- "a"
    testing <- median(shiny.benzo()$tot)
    count.perc <- input$BF[grepl("\\Q%\\E|Count", input$BF)]
    if(testing < 31 && str_detect(count.perc, "Count") == FALSE){
      a <- "b"
    } else{
      a <- "a"
    }
    validate(
      need(a == "a",
           "% too unreliable Please Use Count")
    )
    
    if(grepl("Count", input$BF) == FALSE){
      benzo.max <- benzo %>%
        filter(name == input$BF)
      benzo.max <- max(benzo.max$Percent)
    } else{
      benzo.max <- max(shiny.benzo()$Percent)
    }
    
    graph <- ggplot(shiny.benzo(), aes(x =Days2, y = Percent)) +
      geom_path(group = 1, size = 1) + 
      geom_point(size = 2) +
      scale_x_discrete(breaks = every_nth(n = 3)) +
      scale_y_continuous(limits = c(0, benzo.max+5)) +
      labs(title = paste(count.perc, "in", input$DC, "by Week", sep = " "), 
           x = "Weeks", y = count.perc) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 20))
    graph + annotate(geom = "text", x = 13, 
                     y = 12, label = "Services Closed")
  })
  
  #Makes Table on page 2
  output$sumtable <- renderTable({
    new_table <-shiny.benzo() %>%
      mutate(Max.Date = ifelse(length(unique(Days2[which(tot == max(tot))])) ==1,
                               paste(unique(Days2[which(tot == max(tot))])),
                               paste(unique(Days2[which(tot == max(tot))]),collapse = " AND\n"))) %>%
      summarize(Expected.Substance = unique(Expected.Substance),
                Max_Tested = max(tot), Mean =mean(tot), Median =  mean(tot[tot>0]), 
                Max.Date = unique(Max.Date))
    new_table$Max.Date <- gsub("([A-Za-z]+ [0-9]+)-\n([A-Za-z]+ [0-9]+)", 
                               "\\1 \\2 \\3", new_table$Max.Date)
    colnames(new_table) <- c("Expected\nSubstance", "Max Tested\nDuring Period", 
                             "Mean Tests\n(not including 0's)","Median\n(not including 0's)", 
                             "Top Testing Date")
    
    xtable::xtable(new_table, align = rep("c",6))
  })
} 

shinyApp(ui = ui, server = server)



