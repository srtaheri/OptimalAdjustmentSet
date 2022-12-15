gprep = "Z3 [latent]
         Z1 -> Z2
         Z3 -> Y
         Z3 -> Z2
         Z1 -> X
         X -> Y
         Z4 -> X"
glines = lapply(strsplit(gprep, "\n"), trimws)

e_from = c()
e_to = c()
latent_nodes = c()
for (line in glines[[1]]) {
  if (grepl("->", line)) {
    edge_splitted = lapply(strsplit(line, "->"), trimws)
    e_from = c(e_from, edge_splitted[[1]][1])
    e_to = c(e_to, edge_splitted[[1]][2])
  }
  if (grepl("latent", line)) {
    latent_nodes = c(latent_nodes, strsplit(line, " ")[[1]][1])
  }
}

reg_edge_df = data.frame("from"=e_from, "to"=e_to)

lt_edge_df = data.frame(matrix(nrow = 0, ncol = 3))
colnames(lt_edge_df) = c("from", "to")

for (lt in latent_nodes) {
  lt_indices = which(reg_edge_df[,"from"] == lt)
  lt_edges = reg_edge_df[lt_indices,]
  lt_children = lt_edges[,"to"]
  new_edges = t(as.data.frame(combn(lt_children, 2)))
  colnames(new_edges) = c("from", "to")
  lt_edge_df = rbind(lt_edge_df, new_edges)
  reg_edge_df = reg_edge_df[-lt_indices,]
}
rownames(reg_edge_df) = seq(1:nrow(reg_edge_df))
rownames(lt_edge_df) = seq(1:nrow(lt_edge_df))

lt_edges_str = paste(apply(lt_edge_df, 1, function(x) paste0(x[1],"<->",x[2])), collapse=";\n")

reg_edges_str = paste(apply(reg_edge_df, 1, function(x) paste0(x[1],"->",x[2])), collapse = ";\n")

dagitty_input_str = paste("dag {", reg_edges_str, ";\n", lt_edges_str, ";\n}", sep="")


mydag = dagitty(dagitty_input_str)










reg_edge_df = cbind(reg_edge_df,data.frame("type" = rep("->", nrow(reg_edge_df))))
lt_edge_df = cbind(lt_edge_df,data.frame("type" = rep("<->", nrow(lt_edge_df))))

edge_df = rbind(reg_edge_df, lt_edge_df)

tibble_edge_df = tibble(edge_df)
# https://stackoverflow.com/questions/69134313/parsing-through-dataframe-to-create-a-string-in-r
# convert_dag = function(df,V,W,E) {
#   tibble_edge_df %>% mutate(
#     txt = ifelse(v==V & w==W, paste(V,E,W), paste(v,e,w))
#   ) %>% pull(txt) %>% paste(.,collapse = " ") %>%  paste('dag{',.,'}')
# }
# convert_dag(df, "d", "f", "->")