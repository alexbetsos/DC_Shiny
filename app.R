####----Link for APP---####
gitlink <- "https://gitlab.com/coolsoftwaredev/drug_checking_data_scraper/-/jobs/artifacts/master/raw/dcbc.csv?job=run"


dcbc <- read.csv(gitlink, na.strings=c("N/A",""), stringsAsFactors=FALSE)
####---Libraries & Functions---####
library(tidyverse)
library(lubridate)
library(igraph)
library(RColorBrewer)

`%notin%` <- Negate(`%in%`) #For laziness
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})     #Just for ggplot
} 
#install.packages("plyr") #I use this for count because I'm too lazy to learn tidyverse
####---Dates----####
#Creates a df of date ranges to be used later for making factors for the x-axis
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
dcbc <- separate(dcbc, FTIR.Spectrometer , into = c("FTIR.1", "FTIR.2", "FTIR.3", "FTIR.4", "FTIR.5", "FTIR.6"), sep = ", ",
                 extra = "merge", fill = "right", remove = FALSE)

###----Variables used i
op2 <- c(unique(dcbc$Expected.Substance[grep("[Ff]ent", dcbc$Expected.Substance)]),
         unique(dcbc$Expected.Substance[grep("Her", dcbc$Expected.Substance)]),
         "Down (Unknown Opioid)", "Oxycodone", "Methadone", "Morphine", "Heroin HCl",
         "6-Monoacetylmorphine", "Oxycodone HCl", "Fentanyl HCl", "Fentanyl or Analog")
###---All opioids----####

#####----More Cleaning ----####
#This section could be rewritten
dcbc2 <- dcbc
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
dcbc2 <- dcbc2 %>%
  mutate(is.fent = ifelse(str_detect(FTIR.Spectrometer, "[Ff]ent"), 1,0))

#Finds all benzo positive strips which do not have positive benzo responses
dcbc2$has.benzo[dcbc2$FTIR.1 %notin% benzo.match & dcbc2$Benzo.Test.Strip ==1 &
                  dcbc2$FTIR.2 %notin% benzo.match & dcbc2$FTIR.3 %notin% benzo.match &
                  dcbc2$FTIR.4 %notin% benzo.match & dcbc2$FTIR.5 %notin% benzo.match &
                  dcbc2$FTIR.6 %notin% benzo.match] <- 1
#loop finds the positive fent and benzo strips where they are not listed
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
#To make the graphs I either had to create loops or include "no cuts" - I think it's worked out
dcbc2$FTIR.2[which(is.na(dcbc2$FTIR.2))] <- paste("No Cuts\n", dcbc2$FTIR.1[which(is.na(dcbc2$FTIR.2))], sep = "")
dcbc2$FTIR.1[which(grepl("No Cuts", dcbc2$FTIR.2))] <- dcbc2$FTIR.2[which(grepl("No Cuts", dcbc2$FTIR.2))]
dcbc2 <- dcbc2[dcbc2$FTIR.1 != "",] %>%
  mutate(across(where(is.character), str_trim)) %>%
  rowid_to_column("ID")

dcbc2$Week.val <- poss.w$Days2[match(dcbc2$Date, poss.w$Days)]
rownames(dcbc2) <- NULL
####---String replacement!----####

dcbc2 <- dcbc2 %>%
  mutate(Expected.Substance = gsub("Down \\Q(Unknown Opioid)\\E|Fentanyl","Fentanyl/Down",Expected.Substance)) %>%
  pivot_longer(cols = c(FTIR.1:FTIR.6),
               names_to = "name",
               values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = str_replace_all(value,c("Fentanyl HCl" = "Fentanyl or Analog", "Dextromethorphan" = "DXM",
                                         "6-Monoacetylmorphine" = "6-MAM", "Surcrose" = "Sucrose", 
                                         "Polyethylene Glycol" = "PEG", "Lysergic Acid Diethylamide" = "LSD"
  ))) %>%
  mutate(value = str_replace(value,"Cocaine base", "Crack Cocaine")) %>%
  mutate(value = str_replace(value,"Sugar Possibly [A-Za-z]+", "Sugar Uncertain")) %>%
  mutate(value = str_replace(value, "Heroin \\Q(Trace)\\E", "Heroin")) %>%
  mutate(value = gsub("Uncertain Oil|Sugar Uncertain|Uncertain Carbohydrate",
                      "Uncertain Oil/Carb/Sugar", value)) %>%
  mutate(value = str_replace(value, " HCl", ""))

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

dcbc2 <- rbind(dcbc2, min_down2, all_opioids)
dcbc2 <- dcbc2[order(dcbc2$Week.val),]

interest <- c("Fentanyl/Down", "Opioids Minus Fentanyl (Grouped)", "All Opioids (Grouped)", "Methamphetamine",
              "Ketamine", "Cocaine", "Crack Cocaine", "MDMA")

dcbc2 <- dcbc2[dcbc2$Expected.Substance %in% interest,]       #subset so to include a handful


####----Stuff for ggraph - same things as ggplot####


#Called here instead to not load into R until needed
source("Drug Classification.R")

df_sub <- which(names(dcbc2)%in%c("Week.val", "Expected.Substance", "value"))

down.node <- plyr::count(dcbc2[,df_sub]) %>%
  dplyr::rename(Expected = Expected.Substance, Names = value, Weight = freq)
unique_id <- data.frame(value = unique(down.node$Names)) %>%
  rowid_to_column("ID")
down.node$ID <- unique_id$ID[match(down.node$Names, unique_id$value)]



#Creates class and colour so they're consistent every time shiny runs
down.node$Classification <- type.of.drug$Classification[match(down.node$Names,type.of.drug$Drug.Name)]
#Since the dictionary is handwritten  this next line just makes sure it won't throw errors
down.node$Classification[is.na(down.node$Classification)] <- "new_val" 
new_vals <- down.node[grepl("No Cuts\n", down.node$Names),]
new_vals$Names <- gsub("No Cuts\n", "", new_vals$Names)

new_vals$Classification <- type.of.drug$Classification[match(new_vals$Names,type.of.drug$Drug.Name)]
new_vals$Classification[is.na(new_vals$Classification)] <- "new_val"
new_vals <- new_vals[!duplicated(new_vals$ID), ]
down.node$Classification2 <- new_vals$Classification[match(down.node$ID, new_vals$ID)]
down.node$Classification[which(!is.na(down.node$Classification2))] <- down.node$Classification2[which(!is.na(down.node$Classification2))]
coul  <- brewer.pal(length(unique(down.node$Classification)), "Set3")
my_colors <- coul[as.numeric(as.factor(unique(down.node$Classification)))]
names(my_colors) <- unique(down.node$Classification)


interest <- c("Fentanyl/Down", "Opioids Minus Fentanyl (Grouped)", "All Opioids (Grouped)", "Methamphetamine",
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
  mutate(name = str_replace_all(name, c("fent.count" = "Count Fentanyl",
                                        "fent.perc"= "% Fentanyl","benzo.perc" = "% Benzo",
                                        "benzo" = "Count Benzo"
  )))

#Days closed due to covid
closure <-  c("Mar 16-\nMar 23\n2020", "Mar 23-\nMar 29\n2020", "Mar 30-\nApr 05\n2020")
closure2 <- poss.w$Days2[poss.w$Days2 %in% closure]
benzo$Percent[grepl("\\Q%\\E", benzo$name) & benzo$tot < 20] <- NA
benzo$Percent[which(benzo$Week.val %in% closure)] <- -1
benzo$Percent[which(is.na(benzo$Percent))] <- -1

benzo <- benzo[order(benzo$Days2),]
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
                         h1("What is this? - Intro to Online Drug Checking app"),
                         p("It has been 5 years since BC first declared its overdose crisis. In 2017, after years of work by activists, small drug checking pilot projects began in Vancouver, where people who bought drugs from the illegal market could have them tested. They would then be relayed these results and told the approximate composition of the drug sample they submitted. In 2020 the team at the BCCSU made this data available here at", a("drugcheckingbc.ca", href = "https://drugcheckingbc.ca/")),
                         p("This work could not have been done without extensive feedback, as well as advice from Karen Ward, who initially asked me for a weekly readout of the drug checking data. She's provided feedback throughout the design of the initial graphs - as well as requested the graphs found on the 'Benzo's and Opioids' page, specifically looking at the effects on supply & cheque day."),
                         h1("How to Read the Data"),
                         p("There are currently 2 pages here. The first is a network graph, with a sliding scale that shows drug checking results by Week, and where you can pick a handful of drugs to explore what other drugs they're found with. The size of the node is based on the number of times that substance was found, and the line between them, or the \' edges \' demonstrates how often they occurred together. Like so:"),
                         img(src='explanation_image.png', align = "left", height = "50%", width = "50%"),
                         p(strong("This doesn't mean that every sample contained all of these drugs."), "It just means that all these substances have been found with the one in question. Currently the network graph works best for Fentanyl - and worse for the others... Such is life."),

                         p("This has taken quite a bit of work, as a qualitative researcher this is not my forte - I hope that others who may be interested will look to develop and think about visualizing this data in new ways. In the future I might make these graphs more interactive."),
                         br(""),
                         h1("Benzos and Opioids Page"),
                         p("Page 2 is more straightforward. It calculates the % or Count of fentanyl/benzo's in certain drugs by week. The percentages are only available if the median sample size per week is greater than 30. Percentages at less than 30 samples/week are more likely to be skewed. If for example 1/5 tests of MDMA in a week were positive for fentanyl the % would be 20%."),
                         h1("Final Thoughts"),
                         p("Any visualization is really just a choice about how to cut, or a tool for thinking with data. Rather than just having these particular ways of visualizing the data, I'd like to be able to create different representations or visualizations, which could facilitate our understanding of what the drug market looks like. This is a first attempt at thinking about this question, but I'd love for others to join and help.")
                         #Network Graph
                       )),
              tabPanel("Weekly Drug Checking Network",
                       fluidRow(style='padding:0px',
                                column(width = 9,
                                       offset = 0.5, 
                                       sliderTextInput("Change",
                                                       label = NULL,
                                                       choices = as.character(poss.w$Days2),
                                                       selected = as.character(poss.w$Days2[tail(poss.w$Days2, n = 1)]),
                                                       grid = TRUE,
                                                       width = "1200px",
                                                       force_edges = TRUE)),
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
                                       plotOutput("net", width = "85%",
                                                  height = "700px")),
                                
                                column(width = 2, offset = 0.5,tableOutput("tafle")))),
              #Benzo % and Opioid %
              tabPanel("Benzos + Opioids",
                       fluidRow(column(width = 2,
                                       radioButtons("DC", "Select Drug", 
                                                    choices = interest,
                                                    selected = "Fentanyl/Down")),
                                column(width = 1, offset = 0,
                                       radioButtons("BF", "% Benzo or Fent",
                                                    choices = c("% Fentanyl", "% Benzo",
                                                                "Count Fentanyl", 
                                                                "Count Benzo"),
                                                    selected = "Count Benzo")),
                                column(width = 3, offset = 0, tableOutput("sumtable"))),
                       plotOutput("Perc", width = "100%", height = 500)
              )
  ))
#Server still needs to be fixed...
server <- function(input, output) {
  #1st makes reactive df for the rest of the project
  df_react <- reactive({
    dcbc2 %>%
      filter(Expected.Substance == input$Drug & Week.val == input$Change)
  })
  #Nodes for the Social Network Visualization
  nodes <- reactive({
    down.node %>%
      filter(Expected == input$Drug & as.character(Week.val) == input$Change) %>%
      select(ID, Names, Weight, Classification) %>%
      arrange(desc(Weight))
  })
  #Edges for SN
  edges2 <- reactive({
    req(df_react() != 0)
    df_react() %>%
      select(ID, value) %>%
      nest(data=(value)) %>%
      mutate(pairs=map(data, ~as_tibble(t(combn(.$value, 2))), .name_repair=T, .keep)) %>%
      unnest(pairs) %>%
      select(V1, V2) %>%
      group_by(V1, V2) %>%
      summarise(amount = n()) %>%
      ungroup()
  })
  #3 dimensional
  edges3 <- reactive({
    req(df_react() != 0)
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
      need(nrow(edges) >1, "Not tested During this Time")
    )
    
    colnames(edges) <- c("to", "from", "weight")
    edges$from <- nodes()$ID[match(edges$from, nodes()$Names)]
    edges$to <- nodes()$ID[match(edges$to, nodes()$Names)]
    edges <- select(edges, from, to, weight)
    
    g <- graph_from_data_frame(d = edges, vertices = nodes(), directed = FALSE) 
    g <- simplify(g, remove.loops = TRUE)
    if(grepl("Fentanyl/Down", input$Drug) == TRUE){
      #Checks if there is just one graph or several
      if(is.connected(g) == FALSE){
        #if true then, it splits the main graph from the subgraphs
        c <- clusters(g); cn <- cbind(V(g), c$membership)
        lc <- which(which.max(c$csize)==c$membership);
        gs <- induced.subgraph(g, lc)
        st1 <- layout_as_star(gs, center = V(gs)$Names == "Fentanyl or Analog")
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
        st1 <- layout_as_star(g, center = V(g)$Names == "Fentanyl or Analog")
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
      scale_edge_width_continuous(breaks = c(1, 5, 10,15, 20, 25),
                                  label = c(1, 5, 10,15, 20, 25),
                                  range = c(1,7), name = "Frequency Found\nTogether",
                                  limits = c(0,40),
                                  guide = guide_legend(order = 3, nrow = 3, ncol =3)) +
      geom_node_point(aes(size = V(g)$Weight, color = V(g)$Classification)) +
      scale_color_manual(values = my_colors, name = "Class of Drug",
                         guide = guide_legend(order = 2, 
                                              nrow = 6,
                                              aes.overide = list(hjust = 0.5))) +
      coord_cartesian(ylim = c(y_min, y_max), xlim = c(x_min, x_max)) +
      geom_node_text(aes(label = V(g)$Names), size = 6) +
      scale_size(breaks = c(1,10,20,40, 60), label=scales::number,
                 range = c(1,40), limits = c(1,100), name = "Size of Node",
                 guide = guide_legend(order = 1,
                                      nrow = 8,
                                      label.hjust =0.5)) +
      theme(legend.position= "right",
            legend.box.background = element_blank(),
            legend.direction = "vertical",
            legend.key = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(size=12, hjust  = 0.4, inherit.blank = TRUE),
            legend.box.just = "top",
            legend.box = "vertical",
            legend.justification = "left",
            legend.box.spacing = unit(0,"cm"),
            legend.title.align = 0.2,
            legend.text.align = 0.4,
            legend.title=element_text(size=15),
            legend.key.width = unit(0.5, "cm"),
            legend.key.height = unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            panel.background = element_blank(),
            legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
            legend.margin = margin(0,0, 0, 0, unit = "cm"))+
      guides(color = guide_legend(override.aes = list(size=10)))
    
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
    edges33 <- edges3()
    
    graph_table <- rbind(top_node[1,], edge2[1,], edges33[1,])
    
    Category <- c("Top Substance Found", "Top 2 Substance Combinations", "Top 3 Substance Combinations")
    graph_table <- cbind(Category, graph_table)
    xtable::xtable(graph_table, align = rep("c",4))
    
  })
  
  #Second Page data
  shiny.benzo <-reactive({benzo %>%
      filter(Expected.Substance == input$DC & name == input$BF)
  })
  shiny.benzo2 <-reactive({benzo %>%
      filter(Expected.Substance == input$DC & name == input$BF)
  })
  output$Perc <- renderPlot({
    
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
      geom_line(group = 1, size = 1) + 
      geom_point(size = 2) +
      scale_x_discrete(breaks = every_nth(n = 3)) +
      scale_y_continuous(limits = c(0, benzo.max+5)) +
      labs(title = paste(count.perc, "in", input$DC, "by Week", sep = " "), 
           x = "Weeks", y = count.perc) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 20))
    graph + annotate(geom = "text", x = closure2[2], 
                     y = max(benzo.max)/5, label = "Services Closed")
  })
  
  
  output$sumtable <- renderTable({
    new_table <-shiny.benzo2() %>%
      mutate(Max.Date = ifelse(length(unique(Days2[which(tot == max(tot))])) ==1,
                               paste(unique(Days2[which(tot == max(tot))])),
                               paste(unique(Days2[which(tot == max(tot))]),collapse = "\nAND\n"))) %>%
      summarize(Expected.Substance = unique(Expected.Substance),
                Max_Tested = max(tot), Mean =mean(tot), Median =  mean(tot[tot>0]), 
                Max.Date = unique(Max.Date))
    colnames(new_table) <- c("Expected\nSubstance", "Max Tested\nDuring Period", 
                             "Mean Tests\n(not including 0's)","Median\n(not including 0's)", 
                             "Top Testing Date")
    xtable::xtable(new_table, align = rep("c",6))
  })
} 

shinyApp(ui = ui, server = server)

