#' Land and Water Plot
#' 
#' Plots farm and soil related stats
#' 
#' @param schema name of the database schema to use. Default: "soil_survey_data"
#' @param max maximum number of records to limit the resultst to. Default: 20, max: 100
#' @author Ashish Singh https://github.com/git-ashish
#' @import DBI RPostgres ggplot2 plotly htmlwidgets widgetframe
#' @export
#' @examples \dontrun{
#' leafplot(u = "db_user", pw = "db_password", port = db_port)
#' leafplot(u = "db_user", pw = "db_password", port = db_port, usePlotly = TRUE)
#' }
leafplot <- function (u, pw, port, usePlotly = FALSE) {
  
  # Create Connection
  # - provide the schema to connect to via "options"
  
  con <- DBI::dbConnect(RPostgres::Postgres(),
                        host = "owncloud.landandwater.com.au",   
                        port = port,   
                        dbname = "soil_survey",   
                        user = u,   
                        password = pw,
                        options = "-c search_path=soil_survey_data")
  
  # set query
  query <- "SELECT
  	ls.survey_date AS date,
  	fb.block_name AS block_name,
  	lsa.leafsample_attribute_id AS order_seq,
  	lsa.leaf_sample_attribute_name AS attr_name,
  	leaf_sample_attribute_value AS leaf_value,
  	lsa.range_max,
  	lsa.range_min
  FROM
  	leaf_survey_results lsr
  INNER JOIN leaf_sample_attributes lsa ON lsr.leaf_sample_attribute_id = lsa.leafsample_attribute_id
  INNER JOIN farms.blocks fb ON lsr.farm_block_id = fb.gid
  INNER JOIN leaf_surveys ls ON ls.leaf_survey_id = lsr.leaf_survey_id
  WHERE lsr.leaf_survey_id = 1 AND 
  lsa.leafsample_attribute_id IN (1,2,3,4,5,6,13,10,15)
  ORDER BY survey_date DESC, block_name ASC, order_seq ASC"
  
  # Send the query
  res <- dbSendQuery(con, query)
  
  # Fetch the resultset
  leafData <- dbFetch(res)
  
  # create ggplot object
  
  # 1. Labelling vectors
  leaf_attr_names <- c(
    '1'="Nitrogen",
    '2'="Phosphorus",
    '3'="Potassium",
    '4'="Sulphur",
    '5'="Calcium",
    '6'="Magnesium",
    '10'="Zinc",
    '13'="Boron",
    '15'="Chlorine"
  )
  
  # 2. Create ggplot graph object
  dataplot_leafdashbd <- ggplot(data = leafData, mapping = aes(x= date ,y = leaf_value, color=block_name)) +
    theme_light() +
    geom_point() +
    geom_hline(data = leafData, linetype="dotted", colour="#ff710c",alpha = 0.5, aes(yintercept = range_min)) +
    geom_hline(data = leafData, linetype="dotted", colour="#ff710c", alpha = 0.5, aes(yintercept = range_max))
  
  # 3. Add facet and theme options
  
  if(usePlotly){
    theleafplot <- 
      dataplot_leafdashbd +
      theme(
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x=element_blank()
      )+
      facet_wrap(order_seq ~ ., scales = "free_y", ncol=3, labeller = labeller(order_seq = leaf_attr_names))
    
    # 4. Generate Ploty object
    g <- ggplotly(theleafplot)
    
    htmlwidgets::saveWidget(widgetframe::frameableWidget(g), "mygraph.html", selfcontained = TRUE)
    data <- readLines('mygraph.html')
    return(list(text = paste(data, collapse = "\n")))
    
  }else{
    dataplot_leafdashbd +
      theme(
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x=element_blank()
      )+
      facet_wrap(order_seq ~ ., scales = "free_y", ncol=3, labeller = labeller(order_seq = leaf_attr_names))
  }
  
}