library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)


#----extract_steps_and_depends----
extract_steps_and_depends <- function(wdl_file) {
  wdl_script <- readLines(wdl_file)
  wdl_workflow <- wdl_script[(grep("^workflow ", wdl_script)[1] - 1):(grep("^task ", wdl_script)[1] - 1)]
  wdl_calls <- wdl_workflow[grep("call ", wdl_workflow)]
  wdl_calls <- wdl_calls[!(grepl("^#", wdl_calls))]
  
  sub(".*call", "", wdl_calls) %>%
    sub("\\{.*", "", .) %>%
    sub(".* as ", "", .) %>%
    sub(".*\\.", "", .) %>%
    str_trim(.) -> steps
  
  input_list <- vector("list", length(steps))
  for (n in 1:length(steps)) {
    wf_length <- length(wdl_workflow)
    step_loc <- grep(paste0("call .*", steps[n], ".*\\{"), wdl_workflow)[1]
    #print(n)
    #print(grep("}", wdl_workflow[step_loc:wf_length])[1])
    next_close_brace <- sort(grep("}", wdl_workflow[step_loc:wf_length]))[1]
    #print((step_loc):(step_loc + next_close_brace - 2))
    input_list[[n]] <- wdl_workflow[(step_loc):(step_loc + next_close_brace - 2)]
    #print(input_list[[n]])
  }
  
  names(input_list) <- steps
  
  lapply(input_list, function(x) {
    sub(".*=", "", x[grep(paste(steps, collapse = "|"), x)[-1]]) %>%
      sub("\\..*", "", .) %>%
      str_trim() %>%
      unique() -> dependson
    #print(dependson)
    dependson <- dependson[(dependson %in% steps)] # use only those depends that exist in list of calls
  }) -> depends_list
  
  return(list(steps, depends_list))
}

#----make_graph----
make_graph <- function(depends_list) {
  graph <- create_graph()
  for (each in names(depends_list)) {
    graph <- add_node(graph, label = each)
  }
  for (each in names(depends_list)) {
    if (length(depends_list[[each]])) {
      for (eachsup in depends_list[[each]]) {
        #print(paste0("---", each))
        #print(eachsup)
        graph <- add_edge(graph, from = eachsup, to = each)
      }
    }
  }
  return(graph)
}





#----SVG function----

draw_svg_flowchart <- function(steps_and_depends_list, filename) {
  steps <- steps_and_depends_list[[1]]
  depends_list <- steps_and_depends_list[[2]]
  digraph_edges <- vector()
  for (each in names(depends_list)) {
    if (length(depends_list[[each]])) {
      for (eachsup in depends_list[[each]]) {
        digraph_edges <- c(digraph_edges, paste0(eachsup, "->", each))
      }
    }
  }
  
  digraph_text <- paste0(
    "digraph boxes_and_circles {
    # a 'graph' statement
    graph [overlap = true, fontsize = 10]
                          
    # several 'node' statements
    node [shape = box, style = filled, fontname = Helvetica, fillcolor = LightBlue1]\n", 
    paste(steps, collapse = ";"), 
    "\n# several 'edge' statements\n",
    paste(digraph_edges, collapse = " "),
    "\n}")
  #cat(digraph_text)
  
  svg <- export_svg(grViz(digraph_text))
  writeLines(svg, filename)
  return(digraph_text)
}

#----run----
wdl_file <- "workflows/cnv_somatic_pair_workflow.wdl"

extract_steps_and_depends(wdl_file)[[2]] %>%
  make_graph() %>%
  #graph %>% 
  set_graph_directed() %>%
  render_graph(layout = "nicely", output = "visNetwork")

extract_steps_and_depends(wdl_file) %>%
  draw_svg_flowchart(filename = "cnv_somatic_pair_workflow.svg") %>%
  grViz()


