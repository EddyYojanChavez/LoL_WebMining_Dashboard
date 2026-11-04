library(igraph)
library(tidygraph)
library(ggraph)

# 1. Definir los nodos (Páginas clave de la Wiki de LoL)
nodos <- c(
  "https://wiki.leagueoflegends.com/Campeones",
  "https://wiki.leagueoflegends.com/Ahri",
  "https://wiki.leagueoflegends.com/Zed",
  "https://wiki.leagueoflegends.com/Items",
  "https://wiki.leagueoflegends.com/Runas",
  "https://wiki.leagueoflegends.com/Parches"
)

# 2. Definir los enlaces dirigidos (origen -> destino)
enlaces <- data.frame(
  origen = c(
    "https://wiki.leagueoflegends.com/Campeones",
    "https://wiki.leagueoflegends.com/Campeones",
    "https://wiki.leagueoflegends.com/Campeones",
    "https://wiki.leagueoflegends.com/Items",
    "https://wiki.leagueoflegends.com/Runas"
  ),
  destino = c(
    "https://wiki.leagueoflegends.com/Ahri",
    "https://wiki.leagueoflegends.com/Zed",
    "https://wiki.leagueoflegends.com/Items",
    "https://wiki.leagueoflegends.com/Ahri",
    "https://wiki.leagueoflegends.com/Zed"
  ),
  stringsAsFactors = FALSE
)

# 3. Crear el objeto grafo dirigido con igraph
grafo <- graph_from_data_frame(d = enlaces, vertices = nodos, directed = TRUE)

# 4. Calcular métricas de centralidad (PageRank e HITS)
hits_res <- hits_scores(grafo)

# 5. Convertir a tidygraph integrando todas las métricas de forma limpia
grafo_tbl <- as_tbl_graph(grafo) %>%
  mutate(
    pagerank = centrality_pagerank(),
    authority = hits_res$authority,
    hub = hits_res$hub,
    name = as.character(name)
  )

# 6. Visualización del Grafo basado en PageRank
set.seed(42)
ggraph(grafo_tbl, layout = "fr") +
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')), end_cap = circle(3, 'mm'), edge_colour = "gray70") +
  geom_node_point(aes(size = pagerank), color = "steelblue", alpha = 0.8) +
  geom_node_text(aes(label = basename(name)), vjust = 1.8, size = 4) +
  scale_size_continuous(range = c(5, 12)) +
  theme_void() +
  ggtitle("Grafo de Minería de Estructura - PageRank en LoL")