library(DiagrammeR)
library(dplyr)

chart <- create_graph() %>%
  
  # Nodes  main relationship (1:3)
  add_node(label = "Cause", node_aes = node_aes(shape = "ellipse", color = "black", fontcolor = "black", fillcolor = "white", fontsize = 8, x = 0, y = 0, penwidth = 1, width =  0.75, height = 0.5)) %>%
  add_node(label = "Effect", node_aes = node_aes(shape = "ellipse", color = "black", fontcolor = "black", fillcolor = "white", fontsize = 8, x = 2, y = 0, penwidth = 1, width =  0.75, height = 0.5)) %>%
  add_node(label = "Moderator", node_aes = node_aes(shape = "ellipse", color = "black", fontcolor = "black", fillcolor = "white", fontsize = 8, x = 1, y = 1, penwidth = 1, width =  0.75, height = 0.5)) %>%
  
  # Edges main relationships
  add_edge(from = 1, to = 2, edge_aes = edge_aes(style = "solid", color = "black", fontcolor = "black", fontsize = 16, penwidth = 1, arrowhead = "normal")) %>%
  
  # Nodes moderations (4)
  add_node(label = "", node_aes = node_aes(shape = "ellipse", color = "black", fontcolor = "black", fillcolor = "black", fontsize = 0, x = 1, y = 0 , penwidth = 0, width = 0, height = 0)) %>%
  
  # Edges moderations
  add_edge(from = 3, to = 4, edge_aes = edge_aes(style = "solid", color = "black", fontcolor = "black", fontsize = 16, penwidth = 1, arrowhead = "normal")) %>%
  
  
  # Hypotheses
  add_node(label = "H1: +", node_aes = node_aes(shape = "ellipse", color = "white", fontcolor = "black", fillcolor = "#00000000", fontsize = 6, x = 1, y = -0.25 , penwidth = 0, width = 1, height = 0.25)) %>%
  add_node(label = "H2: -", node_aes = node_aes(shape = "ellipse", color = "white", fontcolor = "black", fillcolor = "#00000000", fontsize = 6, x = 0.75, y = 0.5 , penwidth = 0, width = 1, height = 0.25)) %>%
  render_graph() #(width = 400, height = 400)

