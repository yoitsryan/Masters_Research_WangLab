#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Tabsets reference (6.3): https://mastering-shiny.org/action-layout.html
# Another reference: https://rstudio.github.io/shiny/reference/tabsetPanel.html

# Show a tabset that includes a plot, summary, and
# table view of the generated distribution
#
# "M107983.csv"

# library(shinydashboard)
library(igraph)
library(visNetwork) # specifically for filtering nodes and zooming in/out
library(DT) # for putting borders around the figures
# https://stackoverflow.com/questions/75149954/add-black-border-to-sidebarpanel-and-mainpanel-r-shiny
library(bslib) # this package is needed for an accordion panel
library(shinyGizmo)
library(ggvenn)

# Selectize input
# https://github.com/yihanwu/Nutrient_Calculator/blob/master/app.R

ui <- fluidPage(
  headerPanel("Personalized single-cell functional genomic maps for 1,495 human brains"),
  tabsetPanel(
    tabPanel(title = 'Overview',
             br(),
             p(h3('Abstract')),
             br(),
             p("Alzheimer’s disease (AD) is known to affect around 6.7 million Americans age 65 or older, with a prevalence of 1 in 9 people. Complex brain diseases like AD are heterogeneous, where individuals with the same diagnosis have different disease mechanisms and clinical phenotypes and, hence, react to therapeutic interventions differently. For example, AD individuals can have different neuropsychiatric symptoms (NPS), like insomnia, mood disorder, and weight loss. Functional genomics is the key contributor to the heterogeneity in the molecular and cellular mechanisms, which leads to the differences in the disease mechanisms across individuals. A personalized functional genomic map provides a complete view of an individual’s genomic profile, linking cell-type gene regulatory networks, cell-cell interactions, and genetic variants to clinical phenotypes. To this end, we perform an integrated analysis using the PsychAD large-scale population multimodal data to understand the critical differences in the functional genomic maps of different individuals. The PsychAD cohort contains 1,495 individuals covering eight diverse neurodegenerative and neuropsychiatric diseases with 19 NPSs. Firstly, we construct personalized functional genomic maps for 1,495 individuals linking hundreds of TFs and thousands of genes for 28 cell types, including neuronal and glial cell types, and>500 interactions among those cell types. We can thus characterize the variations of functional genomics across individuals. Secondly, we apply an interpretable knowledge-guided graph neural network model i-BrainMap that inputs these personalized functional genomic maps for predicting phenotypes and prioritizing personalized phenotypic genes, networks, and cell types. Our i-BrainMap model can also incorporate prior knowledge, such as known disease genes or other genes of interest, through diffusion mechanisms to discover novel phenotype-related genes at the individual level. By inputting an individual’s functional genomic map, i-BrainMap outputs graph embeddings to classify individuals into known phenotypes or stratify them into potentially novel subgroups. It also outputs personalized edge-based attention scores (e.g., TF-target gene, cell-type interactions) to prioritize personalized subnetworks for various phenotypes. Thirdly, we create iBrainMapDB, a resource that includes personalized functional genomic maps, novel disease and phenotypic genes, and prioritized subnetworks. Finally, i-BrainMap is an open-source tool for general usage. Such a resource and tool can provide the community a foundation for further analyses, like comparative network analysis, providing potential deeper insights into disease mechanisms and personalized therapeutic strategies."),
             br(),
             p(h3('Contact Information')),
             p('Daifeng Wang (daifeng.wang@wisc.edu) at ', a(href = 'https://daifengwanglab.org/', 'Wang research group at Waisman Center at UW-Madison', .noWS = "outside")),
             br(),
             # img(src='myImage.png', align = "right")
             imageOutput("home_img")),

    tabPanel("PFG Maps",
             sidebarLayout(
               sidebarPanel(
                 # Multiple data tables in collapsible tabs (accordion)
                 # https://stackoverflow.com/questions/70010817/can-you-add-shinydashboardplus-accordion-items-using-lapply-or-a-loop
                 accordion("extras",
                           accordionItem("metadata", "Patient Metadata", DTOutput('meta'), active = T),
                           accordionItem("graph_data", "Interactions and Scores", DTOutput('tbl'), active = T)
                 ),
                 
                 # This is the only way to divide the sidebar panel
                 # https://stackoverflow.com/questions/43592163/horizontal-rule-hr-in-r-shiny-sidebar
                 # https://stackoverflow.com/questions/21921665/how-to-make-hr-full-width
                 tags$hr(style="margin: 30px -20px 20px; border: 0; border-top: 3px solid #C0C0C0;"),
                 
                 # remove .csv from the choices' text
                 selectInput("file", "Select sample:", choices = gsub(".csv", "", list.files("./NewFiles"), fixed=T)),
                 radioButtons("weights", "Choose edge importance score:",
                              choiceNames = c("AD", "SCZ", "Data-Driven", "Combined"),
                              choiceValues = c("AD_avg_imp", "SCZ_avg_imp", "data_avg_imp", "combined_avg_imp")),
                 sliderInput("range","Adjust importance threshold:",min=0,max=1,value=0),
               ),

               # Show a plot of the generated distribution
               mainPanel(
                 tags$head(
                    # Note the wrapping of the string in HTML()
                    tags$style(HTML(".well {
                        border: 3px solid #C0C0C0;
                      }")),
                    tags$style(HTML(".col-sm-8 {
                        border: 3px solid #C0C0C0;
                      }")),
                  ),
                 visNetworkOutput("graphPlot", height = "750px", width="100%")
               )
             )
    ),

    tabPanel("Compare genes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("file2", "Choose file:", choices = list.files("./NewFiles"))
               ),
               mainPanel(plotOutput("venn"),DTOutput('venn_table'))
             ))
  )
)


# SERVER START
server <- function(input, output, session) {

  output$home_img <- renderImage({
    list(src = "Pramod_Figure1_v2.png", width = "50%", align='center')
  }, deleteFile = F)

  current_file = reactive(paste0(input$file, ".csv")) # which graph to display... add .csv back on, we temporarily took it off while populating the dropdown menu
  current_weight = reactive(input$weights) # weight category to be displayed
  current_data = reactive({read.csv(paste0("./NewFiles/", current_file()))})
  print("current_weight")
  # print(current_weight)

  # Output the contents of the CSV
  # https://rdrr.io/cran/DT/man/dataTableOutput.html
  # https://rstudio.github.io/DT/shiny.html
  # SCROLLABLE: https://stackoverflow.com/questions/30765338/how-to-make-the-horizontal-scrollbar-visible-in-dtdatatable
  output$tbl <- DT::renderDataTable({
    DT::datatable(data = current_data()[c(1, 2, 4, 5, 6, 7)],
                  options = list(scrollX = TRUE)
    )
  })
  
  # adding metadata, hard-coded for now
  meta_data = reactive({read.csv("sample_meta_ryan.csv")})
  
  output$meta <- DT::renderDataTable({
    DT::datatable(data = meta_data(), options = list(scrollX = TRUE)
    )
  })

  # Start of renderVisNetwork
  output$graphPlot <- renderVisNetwork({
    # print("Current weight")
    # print(current_weight())
    # print("Current file")
    # print(paste(current_file(), ".csv", sep=""))
    NE = current_data() # copy
    #print("What's this do?")
    #print(input$node_names)
    #print("Current weight")
    #print({current_weight})

    # Which shapes are the nodes going to be?
    # String splitting involved here
    # The result should either be celltype, TF, or TG
    NE$start_type = do.call("c", lapply(strsplit(NE$edge_type, "_", fixed = T), function(x) x[1]))
    NE$end_type = do.call("c", lapply(strsplit(NE$edge_type, "_", fixed = T), function(x) x[3]))

    # around here we have to remove the edges with weights out of bounds
    NE = NE[(NE[input$weights] >= input$range),]

    # also remove the edges with the same node on both ends (lOOPS)
    NE = NE[(NE["from"] != NE["to"]),]
    # print("NE")
    # print(dim(NE))

    # Which set of weights do we want to display?
    NE$weight = round(NE[[current_weight()]], digits = 5)
    #print("!!!!!!!!!")
    print("NE")
    print(NE)

    # Create the graph
    g = graph_from_data_frame(NE)
    v = V(g) # gets the individual nodes
    print("Object G")
    print(g)

    print("A")
    celltype = unique(c(subset(NE, start_type=="celltype")$from, subset(NE, end_type=="celltype")$to)) # get the names of the cells
    print('B')
    TF = unique(c(subset(NE, start_type=="TF")$from, subset(NE, end_type=="TF")$to)) # get the names of the transcription factors
    print("C")
    TG = unique(c(subset(NE, end_type=="TG")$to)) # get the names of the target genes
    print("D")
    # Considering the number of each type, and that v$name organizes the nodes by their type...
    # How many of each type do we see?
    # This will be turned into a shape vector that is fed to plot(vertex.shape)
    shape.vec = c(rep("hexagon", length(celltype)), rep("triangle", length(TF)), rep("circle", length(TG)))
    print('E')
    # print(v$name)
    # Adding weights?
    # https://stackoverflow.com/questions/51856706/weighted-graph-from-a-data-frame

    nodes<-igraph::as_data_frame(id=v$name, label=v$name)
    print('F')
    print("Made it to here")
    nodes$group<-ifelse(nodes$id %in% celltype, "cell types", ifelse(nodes$id %in% TF,"transcription\nfactors","target genes"))
    edges=as_data_frame(g,what=c("edges"))
    # print(current_weight())
    #print("TF")
    #print(TF)

    # For the edges to vary in thickness, we need this line
    edges$value=edges$weight
    print("Edges")
    print(edges)

    visNetwork(nodes=nodes, edges=edges, height="500px",width="500px") %>%
      visNodes(
        shape = shape.vec,
        color = list(
          border = "black",
          background = "orange"
        ),
        font = list(size = 30)
      ) %>%
      visEdges(
        color = list(color = "lightblue", highlight="magenta", hover="magenta", inherit="from"),
        arrows = "to", # which direction should the arrows point?
        arrowStrikethrough = FALSE # if TRUE, some arrowheads are placed before the ends of their edges
      ) %>%
      visIgraphLayout(layout = "layout_with_fr", randomSeed = 555) %>%
      visGroups(groupname="cell types",shape="hexagon", size = 35) %>%
      visGroups(groupname="transcription\nfactors",shape="triangle", size = 25) %>%
      visGroups(groupname="target genes",shape="dot", size = 25) %>%
      visOptions(selectedBy = "group", # filter by node type
                 nodesIdSelection=list(enabled=T,
                                       useLabels=T), # this will allow for a dropdown menu for nodes
                 highlightNearest=list(enabled=T,
                                       hover=T,
                                       labelOnly=F,
                                       hideColor="lightblue",
                                       degree=1)) %>%
      visLegend(width = 0.1, zoom=FALSE)
    # visOptions allows you to pick the node itself!
    # can set sizes of hexagons and triangles manually, but can't get labels inside of them
    # dot is a type of circle that can ensure consistent size
    # circle would vary in size based on how long the text inside is
    # Legend: https://www.rdocumentation.org/packages/visNetwork/versions/2.1.2/topics/visLegend

    # De-highlight secondary edges? Still stuck on that...
    # https://stackoverflow.com/questions/50532361/visnetwork-highlightnearest-show-only-connected-edges-to-a-selected-node
    }
  )
  
  current_file2 = reactive(input$file2)
  
  current_data2 = reactive({read.csv(paste0("./NewFiles/", current_file2()))})
  
  output$venn = renderPlot({
    listA = unique(current_data2()$from)
    listB = unique(current_data2()$to)
    
    out_venn = list("From" = listA,
                    "To" = listB)
    
    ggvenn(out_venn, show_elements = F)
  })

  output$venn_table<-renderDT({
	  curr_data<-current_data2()
	  cat_vals<-sort(unique(c(unique(curr_data$from),unique(curr_data$to))))
	  result<-data.frame(category=cat_vals,
						 in_cat1 = cat_vals %in% curr_data$from,
						 in_cat2 = cat_vals %in% curr_data$to
						 )
	  result$in_both<- result$in_cat1==T & result$in_cat2==T
	  result$where <- apply( result,1, function(x){
						 ifelse(x[4]==T,'both',
					     ifelse(x[2]==T & x[3]==F,'cat1',
						 ifelse(x[2]==F & x[3]==T,'cat2','None')))
						 })
     result_dt<-datatable(result[c('category','where')]) |>
		 formatStyle(c('category','where'),valueColumns=2,backgroundColor=styleEqual(c('cat1','cat2','both'),values=c('blue','yellow','green')))
     return(result_dt)
  })


}

if (interactive()) {
  shinyApp(ui, server)
}

# Need a legend dictating what the shapes are
# Metadata about the patients themselves
