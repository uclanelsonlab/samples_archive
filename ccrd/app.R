#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl);

path <- 'data/2024-06-11_UDN-Gateway_families_filter.xlsx';
excel_sheets(path = path);
data <- read_excel(path = path, sheet = '2024-06-11_UDN-Gateway_families');
data <- data[data$Status %in% c('Solved - KP', 'Solved - ND', 'Solved - PE', 'Unsolved - CG', 'Unsolved - CS'),];
colnames(data)[colnames(data) %in% c("primaryRelative.udnId")] <- 'Cases';
colnames(data)[colnames(data) %in% c("List of Genes\r\n(red = NOT in Gateway)")] <- 'gene.of.interest';
data <- data[!is.na(data$gene.of.interest),];
data$referred.by <- paste(data$'Referring physician', data$'Referring institution', sep = '|');

# data.participants <- data.frame(UDN.id = rep(NA,3), relationship = c('Proband', 'Father', 'Mother'), affected = c('Yes', 'No', 'No'), available = c('Yes', 'Yes', 'Yes'));
path <- 'data/main.xlsx';
excel_sheets(path = path)
data.participants <- read_excel(path = path, sheet = 'participant');
data.participants <- merge(x = data, y = data.participants, by.x = 'Cases', by.y = 'family_id*');
colnames(data.participants)[colnames(data.participants) %in% c("participant_id*")] <- 'Participants';
colnames(data.participants)[colnames(data.participants) %in% c("proband_relationship*")] <- 'Relationship';
colnames(data.participants)[colnames(data.participants) %in% c("affected_status*")] <- 'Affected?';
colnames(data.participants)[colnames(data.participants) %in% c("Deceased")] <- 'Available?';
data.participants[!is.na(data.participants$"Available?"),"Available?"] <- 'No';
data.participants[is.na(data.participants$"Available?"),"Available?"] <- 'Yes';

path <- 'data/main.xlsx';
excel_sheets(path = path)
data.analyte <- read_excel(path = path, sheet = 'analyte');
data.analyte <- merge(x = data.participants, y = data.analyte, by.x = 'Participants', by.y = 'participant_id*');

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("California Center for Rare Diseases"),
  
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
  #   sidebarPanel = sidebarPanel(
  textInput(inputId = 'gene', label = 'Gene name: ', value = 'RBBP5', width = '400px', placeholder = 'RBBP5'),
  # actionButton(inputId = 'search', label = 'Search', icon = NULL, width = '400px', disabled = FALSE),
  #   ),
  # mainPanel = mainPanel(
  tableOutput(outputId = 'cases'),
  tableOutput(outputId = 'participants'),
  tableOutput(outputId = 'biospecimens'),
  'E: Whole Exome Sequencing; G: Whole Genome Sequencing; R: RNA-Seq'
  #     )
  # )
);

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$cases <- renderTable(expr = {
    if(!is.null(x = input$gene)){
      return(data[data$gene.of.interest == input$gene,c('Cases', 'Status', 'referred.by')]);
    }
    return(data,c('Cases', 'Status', 'referred.by'));
  });
  output$participants <- renderTable(expr = {
    if(!is.null(x = input$gene)){
      return(data.participants[data.participants$gene.of.interest == input$gene,c('Participants', 'Relationship', 'Affected?', 'Available?')]);
    }
    return(data.participants[,c('Participants', 'Relationship', 'Affected?', 'Available?')])
  });
  output$biospecimens <- renderTable(expr = {
    .data.analyte <- data.analyte[data.analyte$gene.of.interest == input$gene,];
    Biospecimens <- unique(sort(data.analyte$`primary_biosample*`));
    collected.P <- c(
      ifelse(test = sum(.data.analyte$Relationship == 'Self' & .data.analyte$`primary_biosample*` == Biospecimens[1], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Self' & .data.analyte$`primary_biosample*` == Biospecimens[2], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Self' & .data.analyte$`primary_biosample*` == Biospecimens[3], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Self' & .data.analyte$`primary_biosample*` == Biospecimens[4], na.rm = TRUE) > 0, yes = 'Yes', no = 'No')
    );
    collected.F <- c(
      ifelse(test = sum(.data.analyte$Relationship == 'Father' & .data.analyte$`primary_biosample*` == Biospecimens[1], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Father' & .data.analyte$`primary_biosample*` == Biospecimens[2], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Father' & .data.analyte$`primary_biosample*` == Biospecimens[3], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Father' & .data.analyte$`primary_biosample*` == Biospecimens[4], na.rm = TRUE) > 0, yes = 'Yes', no = 'No')
    );
    collected.M <- c(
      ifelse(test = sum(.data.analyte$Relationship == 'Mother' & .data.analyte$`primary_biosample*` == Biospecimens[1], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Mother' & .data.analyte$`primary_biosample*` == Biospecimens[2], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Mother' & .data.analyte$`primary_biosample*` == Biospecimens[3], na.rm = TRUE) > 0, yes = 'Yes', no = 'No'),
      ifelse(test = sum(.data.analyte$Relationship == 'Mother' & .data.analyte$`primary_biosample*` == Biospecimens[4], na.rm = TRUE) > 0, yes = 'Yes', no = 'No')
    );
    available.P <- rep('?', 4);
    available.F <- rep('?', 4);
    available.M <- rep('?', 4);
    sequencing.P <- c('R', 'X', 'G', 'R');
    sequencing.F <- c('X', 'X', 'G', 'X');
    sequencing.M <- c('X', 'X', 'G', 'X');
    contact <- c('CCRD', 'CCRD', 'CCRD', 'CDMD');
    data.biospecimens <- data.frame(
      Biospecimens, rep('',4), collected.P, collected.F, collected.M, rep('',4), available.P,
      available.F, available.M, rep('',4), sequencing.P, sequencing.F, sequencing.M, contact);
    colnames(data.biospecimens) <- c(
      'Biospecimens', 'Collected?', 'P', 'F', 'M', 'Available?', 'P', 'F', 'M', 'Sequenced?', 'P',
      'F', 'M', 'Contact');
    return(data.biospecimens);
  });
}

# Run the application 
shinyApp(ui = ui, server = server)
