#' Land and Water Plot
#' 
#' Plots farm and soil related stats
#' 
#' @param schema name of the database schema to use. Default: "soil_survey_data"
#' @param max maximum number of records to limit the resultst to. Default: 20, max: 100
#' @author Ashish Singh https://github.com/git-ashish
#' @import DBI RPostgres ggplot2 plotly
#' @export
#' @examples \dontrun{
#' soilDashboardPlot(u = "db_user", pw = "db_password", port = db_port)
#' soilDashboardPlot(u = "db_user", pw = "db_password", port = db_port, usePlotly = TRUE)
#' }
soilDashboardPlot <- function (u, pw, port, usePlotly = FALSE) {
  
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
  	bs.block_name AS block_name,
  	ssa.soil_sample_attribute_name AS attrName,
  	ROUND(
  		REPLACE (
  			ssd.soil_sample_attribute_value,
  			',',
  			''
  		) :: NUMERIC,
  		2
  	) AS attr_value,
  	ssa.range_min AS range_min,
  	ssa.range_max,
  	ssa.plot_order AS order_seq,
  	ss.survey_date AS DATE
  FROM
  	soil_survey_data.soil_survey_results AS ssd
  JOIN soil_survey_data.soil_sample_attributes AS ssa ON ssd.soil_sample_attribute_id = ssa.soilsample_attribute_id
  JOIN farms.blocks_soil AS bs ON bs.gid = ssd.farm_block_id
  JOIN soil_survey_data.soil_surveys AS ss ON ss.soil_survey_id = ssd.survey_id
  WHERE
  	ssd.soil_sample_attribute_id IN (
  		2,
  		3,
  		8,
  		26,
  		27,
  		25,
  		44,
  		60,
  		108,
  		64,
  		65,
  		86,
  		73,
  		74,
  		75,
  		90
  	)
  AND ssd.survey_id IN (8, 9)
  ORDER BY
  	survey_date DESC,
  	bs.block_name ASC,
  	ssa.plot_order ASC"
  
  # Send the query
  res <- dbSendQuery(con, query)
  
  # Fetch the resultset
  soilData <- dbFetch(res)
  
  # create ggplot object
  
  # 1. Labelling vectors
  soilsurv_attr_names <- c(
    '1'="pH (CaCl)",
    '2'="E.C. (ms)",
    '3'="Organic Carbon %",
    '4'="Available Nitrogen",
    '6'="Available Potassium",
    '5'="Available Phosphorus",
    '8'="C:N Ratio",
    '9'="Active Biology",
    '7'="Ca:Mg Ratio"
  )
  
  
  # 2. Create ggplot graph object
  dataplot_dashbd <- ggplot(data = soilData, mapping = aes(x= date, y = attr_value, colour=block_name)) +
    theme_light() +
    geom_point() +
    geom_hline(data = soilData, linetype="dotted", colour="#ff710c",alpha = 0.5, aes(yintercept = range_min)) +
    geom_hline(data = soilData, linetype="dotted", colour="#ff710c",alpha = 0.5, aes(yintercept = range_max))
  
  
  # 3. Add facet and theme options
  
  if(usePlotly){
    
    theplot <- (dataplot_dashbd +
                  theme(
                    axis.ticks = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    legend.title = element_blank(),
                    panel.grid.major.x=element_blank()
                  )+
                  facet_wrap(order_seq ~ ., scales = "free_y", ncol=3, labeller = labeller(order_seq = soilsurv_attr_names)))
    
    # 4. Generate Ploty object
    ggplotly(theplot)
    
  }else{
    dataplot_dashbd +
      theme(
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x=element_blank()
      )+
      facet_wrap(order_seq ~ ., scales = "free_y", ncol=3, labeller = labeller(order_seq = soilsurv_attr_names))
    
  }
  
}