
# These need to be run after the BVSP_s and FTAS_s have been shortened to the short term

BVSP_graph = ggplot(BVSP_s, aes(x = date, y = open)) +
    geom_line(color = "blue") +
    labs(x = "Date", y = "Price") +
    ggtitle("BVSP Price Over Time")

ggsave("bvsp_graph.png", plot = BVSP_graph, height = 6, width = 8, units = "in", dpi = 300)


# Time series plot for price_close over time for FTAS
ftas_graph = FTAS_graph = ggplot(FTAS_s, aes(x = date, y = close)) +
    geom_line(color = "blue") +
    labs(x = "Date", y = "Price") +
    ggtitle("FTAS Price Over Time")

ggsave("ftas_graph.png", plot = ftas_graph, height = 6, width = 8, units = "in", dpi = 300)