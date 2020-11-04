# Author: Guillaume Patoine
# Contact: guillaume.patoine@idiv.de
# Last update: 2020-06-12

# Description: use the functions plot_psem (package piecewiseSEM) and plot_lavaan
# (package lavaan) to get a quick rendering of you model.
# The functions need to be changed if you want a specific output format (color, shapes, etc.).

library(magrittr)
library(dplyr)
library(DiagrammeR)
library(stringr)

plot_psem <- function(model, layout = "dot", render = TRUE){

  #function defaults for plotting
  node_attrs = data.frame(shape = "rectangle", color = "black",
                          fillcolor = "white")
  edge_attrs = data.frame(style = "solid", color="black")
  alpha=0.05
  digits = 3

  # get variables for plot --------------------------------------------------------------------

  #get the coefficients table
  ctab <- coefs(model) #is the same as summary(x)$coefficients
  ctab$Response <- as.character(ctab$Response)
  ctab$Predictor <- as.character(ctab$Predictor)

  #define type based on ~~ symbol
  ctab$type <- "regression"
  ctab$type[str_detect(ctab$Response, "^~~*")] <- "correlation"

  #clean names
  ctab$Response <- gsub("~~", "", ctab$Response)
  ctab$Predictor <- gsub("~~", "", ctab$Predictor)


  # make a nodes DF ---------------------------------------------------------

  unique_nodes <- unique(c(ctab$Response, ctab$Predictor))
  nodes <- create_node_df(n = length(unique_nodes),
                          nodes = unique_nodes,
                          type = "lower",
                          label = unique_nodes)
  nodes <- cbind(nodes, node_attrs)
  nodes[] <- lapply(nodes, as.character)
  nodes$id <- as.numeric(nodes$id)
  #nodes$cluster <- ifelse(str_detect(nodes$label, "^lml"), "lml", NA)

  #choose position for neato layout
  #nodes$x <- as.character(c(2,2,3,3,4,rep(1,6))*3)
  #nodes$y <- as.character(c(4.5,2.5,4,3,3.5,6:1))

  #funny node properties to show off
  #nodes$fillcolor <- c(rep("coral:goldenrod", 2), rep("brown:grey",2), "gold", "magenta:cyan", rep("chartreuse:forestgreen", 5))
  #nodes$shape[nodes$label == "cmic_avg"] <- "star"
  #nodes$shape[6:11] <- "ellipse"


  # make an edges DF --------------------------------------------------------

  edges <- create_edge_df(
    from = match(ctab$Predictor, unique_nodes),
    to = match(ctab$Response, unique_nodes),
    type = ctab$type,
    label = round(ctab$`Std.Estimate`, digits)
  )

  edges <- data.frame(edges, edge_attrs)
  edges$color <- as.character(edges$color)
  edges$color[as.numeric(edges$label) < 0] <- "red"
  edges[] <- lapply(edges, as.character)
  edges$id <- as.numeric(edges$id)
  edges$from <- as.numeric(edges$from)
  edges$to <- as.numeric(edges$to)
  edges$style[which(ctab$P.Value>alpha)] <- "dashed"
  edges$color[which(ctab$P.Value>alpha)] <- "grey"
  edges$label <- round(ctab$`Std.Estimate`, digits)

  edges$dir <- "forward"
  edges$dir[edges$type == "correlation"] <- "both"

  #arrow thickness
  edges$penwidth <- 1

  vals <- edges$label[edges$style == "solid"] %>% as.numeric %>% abs #for significant values
  min_pen <- 1
  max_pen <- 8 #choose max line with
  penw <- (vals - min(vals)) * (max_pen - min_pen)/(max(vals) - min(vals)) + min_pen
  edges$penwidth[edges$style == "solid"] <- penw


  # put graph together ------------------------------------------------------

  sem_graph <- create_graph(nodes, edges, directed=TRUE, attr_theme = "default")
  render_graph(sem_graph)

  neato_graph <- sem_graph %>%
    add_global_graph_attrs("fixedsize", "false", "node") %>%
    add_global_graph_attrs("width", "1.5", "node") %>%
    add_global_graph_attrs("fontcolor", "black", "node") %>%
    add_global_graph_attrs("len", "5", "edge") %>%
    add_global_graph_attrs("splines", "true", "graph")

  if (layout == "neato") {

    if (render == FALSE) {
      neato_graph
    } else {
      render_graph(neato_graph)
    }

    #generate_dot(neato_graph) %>% writeClipboard()

    #save_file
    #export_graph(neato_graph, file_name = "SEM_neato.pdf")

  } else if (layout == "dot"){
    dot_graph <- neato_graph %>%
      add_global_graph_attrs(attr = "layout",
                             value = "dot",
                             attr_type = "graph") %>%
      #add_global_graph_attrs("rankdir", "TB", "graph") %>% #left to right
      #add_global_graph_attrs("ranksep", "2", "graph") %>% #bit more space horizontally
      add_global_graph_attrs("fontsize", "16", "node") %>%
      add_global_graph_attrs("fontsize", "12", "edge") %>%
      delete_global_graph_attrs("len", "edge")

    if (render == FALSE) {
      dot_graph
    } else {
      render_graph(dot_graph)
    }

    #render_graph(dot_graph) #position doesn't work with dot, so we need to use ranks directly in the code
    #generate_dot(dot_graph) %>% writeClipboard()
    #generate_dot(dot_graph) %>% cat #copy-paste the DOT code into a grViz function and add the rank statements at the end
  }
}

plot_lavaan <- function(model, layout = "dot", render = TRUE){
  library(DiagrammeR)

  #function defaults for plotting
  node_attrs = data.frame(shape = "rectangle", color = "black",
                          fillcolor = "white")
  edge_attrs = data.frame(style = "solid", color="black")
  alpha=0.05
  digits = 3

  # get variables for plot --------------------------------------------------------------------

  #get the coefficients table
  ctab <- model$PE
  names(ctab)[c(1,3)] <- c("Response", "Predictor")

  ctab <- ctab %>% filter(!Response == Predictor)

  #define type based on ~~ symbol
  ctab$type <- ifelse(ctab$op == "~", "regression", "correlation")

  # make a nodes DF ---------------------------------------------------------

  unique_nodes <- unique(c(ctab$Response, ctab$Predictor))
  nodes <- create_node_df(n = length(unique_nodes),
                          nodes = unique_nodes,
                          type = "lower",
                          label = unique_nodes)
  nodes <- cbind(nodes, node_attrs)
  nodes[] <- lapply(nodes, as.character)
  nodes$id <- as.numeric(nodes$id)

  #choose position for neato layout
  #nodes$x <- as.character(c(2,2,3,3,4,rep(1,6))*3)
  #nodes$y <- as.character(c(4.5,2.5,4,3,3.5,6:1))

  #funny node properties to show off
  #nodes$fillcolor <- c(rep("coral:goldenrod", 2), rep("brown:grey",2), "gold", "magenta:cyan", rep("chartreuse:forestgreen", 5))
  #nodes$shape[nodes$label == "cmic_avg"] <- "star"
  #nodes$shape[6:11] <- "ellipse"


  # make an edges DF --------------------------------------------------------

  edges <- create_edge_df(
    from = match(ctab$Predictor, unique_nodes),
    to = match(ctab$Response, unique_nodes),
    type = ctab$type,
    label = round(ctab$std.all, digits)
  )

  edges <- data.frame(edges, edge_attrs)
  edges$color <- as.character(edges$color)
  edges$color[as.numeric(edges$label) < 0] <- "red"
  edges[] <- lapply(edges, as.character)
  edges$id <- as.numeric(edges$id)
  edges$from <- as.numeric(edges$from)
  edges$to <- as.numeric(edges$to)
  edges$style[which(ctab$pvalue>alpha)] <- "dashed"
  edges$color[which(ctab$pvalue>alpha)] <- "grey"
  edges$color[which(is.na(ctab$pvalue))] <- "blue"
  edges$label <- round(ctab$std.all, digits)

  edges$dir <- "forward"
  edges$dir[edges$type == "correlation"] <- "both"

  #arrow thickness
  edges$penwidth <- 1

  vals <- edges$label[edges$style == "solid"] %>% as.numeric %>% abs #for significant values
  min_pen <- 1
  max_pen <- 8 #choose max line with
  penw <- (vals - min(vals)) * (max_pen - min_pen)/(max(vals) - min(vals)) + min_pen
  edges$penwidth[edges$style == "solid"] <- penw


  # put graph together ------------------------------------------------------

  sem_graph <- create_graph(nodes, edges, directed=TRUE, attr_theme = "default")
  render_graph(sem_graph)

  neato_graph <- sem_graph %>%
    add_global_graph_attrs("fixedsize", "false", "node") %>%
    add_global_graph_attrs("width", "1.5", "node") %>%
    add_global_graph_attrs("fontcolor", "black", "node") %>%
    add_global_graph_attrs("len", "5", "edge") %>%
    add_global_graph_attrs("splines", "true", "graph")

  if (layout == "neato") {
    if (render == FALSE) {
      neato_graph
    } else {
      render_graph(neato_graph)
    }

    #generate_dot(neato_graph) %>% writeClipboard()

    #save_file
    #export_graph(neato_graph, file_name = "SEM_neato.pdf")

  } else if (layout == "dot"){
    dot_graph <- neato_graph %>%
      add_global_graph_attrs(attr = "layout",
                             value = "dot",
                             attr_type = "graph") %>%
      #add_global_graph_attrs("rankdir", "TB", "graph") %>% #left to right
      #add_global_graph_attrs("ranksep", "2", "graph") %>% #bit more space horizontally
      add_global_graph_attrs("fontsize", "16", "node") %>%
      add_global_graph_attrs("fontsize", "12", "edge") %>%
      delete_global_graph_attrs("len", "edge")

    if (render == FALSE) {
      dot_graph
    } else {
      render_graph(dot_graph)
    }

    #render_graph(dot_graph) #position doesn't work with dot, so we need to use ranks directly in the code
    #generate_dot(dot_graph) %>% writeClipboard()
    #generate_dot(dot_graph) %>% cat #copy-paste the DOT code into a grViz function and add the rank statements at the end

  }

}


#helps to transform psem code to lavaan
psemtolav <- function(string, data){
  string %>%
    str_remove_all(pattern = "lm\\(") %>%
    str_remove_all(paste0(", ", data, "\\),")) %>%
    str_remove_all(",") %>%
    str_replace_all("%", " ")
}


# construction site -------------------------------------------------------

# #TODO one function that determines plot for piecewise and lavaan: 3 functions
# plot_sem <- function(model, layout = "dot"){
#   #check class
#   model %>% class
#
#
#   #if psem
#   plot_psem(model, layout = layout)
#
#
#   #if lavaan
#   plot_lavaan(model, layout = layout)
# }
#
#
#
# #TODO create function to add a rank string to adjust the output.
# add_rank <- function(graph_code, rank_string){
#   #
#   graph_code %>% generate_dot
#
#   #remove last }
#
#
#
#   #merge with rank string
#   #e.g. "{rank = 'same'; 5;4;3;}"
#
#   #TODO create string from list of variable names, extract node ID, etc.
#
# }
