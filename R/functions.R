#' Index function
#'
#' This function allows you to calculate the levels of a variable in contrast to group provided by levels of another variable
#'
#' @param data The dataset for which you will be calculating the indexes
#' @param grouping_var The variable for whose levels you will calculate the index
#' @param index_var The variable against which you will be comparing the levels of the grouping variable
#' @return A dataframe with the indexes
#' @export
#get_index()

get_index <- function(data = survey, grouping_var, index_var) {

  index_df <- data %>%
    unlabelled () %>%
    mutate(across(where(is.factor), ~str_replace(., '\\s+\\(([^()]+)\\)', ''))) %>%
    group_by({{ grouping_var }}, {{ index_var }})%>%
    count(wt = weights) %>%
    ungroup() %>%
    group_by({{ grouping_var }}) %>%
    mutate(total_group = sum(n),
           perc_group = n/ total_group) %>%
    ungroup() %>%
    group_by({{ index_var }})%>%
    mutate(total_index = sum(n)) %>%
    ungroup() %>%
    mutate(total = sum(n),
           perc_pob = total_index/total,
           index = 100 *round (perc_group/perc_pob, 2)) %>%
    select({{ grouping_var }}, {{ index_var }}, index) 

  return(index_df)

}


#' Bar plot graph function
#'
#' This function allows you to produce a ggplot graph plot
#'
#' @param data A haven labelled dataset
#' @param variable The variable you want to plot
#' @param title  The name of the plot
#' @return A ggplot bar plot graph
#' @export
#get_index()



get_bar_chart_labels <- function(data, variable, title = "Plot_title") {    
  
  
  pars <- as.list(match.call()[-1])
  plot_labels <- names(attributes(data[[as.character(pars$variable)]])$labels)
  plot_labels <- str_replace(plot_labels, '\\s+\\(([^()]+)\\)', '')
  
  
  
  plot <- data %>%
    group_by({{ variable }}) %>%
    count(wt = weights) %>%
    ungroup() %>%
    mutate(total = sum(n),
           perc = round(100*n/ total,2)) %>%
    ggplot(aes(factor({{ variable }}), perc)) +
    geom_col(fill = "#2171B5") +
    geom_text(aes(label = perc), position = position_stack(vjust = 0.7), color = "white") +
    ggtitle(title)  +
    ylab("Relative frequency") +
    scale_x_discrete("", labels = plot_labels) +
    coord_flip()
  
  
  return(plot) 
  
}


#' Plot graph function
#'
#' This function allows you to produce a ggplot graph plot
#'
#' @param data A dataset
#' @param variable The variable you want to plot
#' @return A ggplot bar plot graph
#' @export
#get_index()

get_bar_simple <- function(data, variable) {
  ggplot(data, aes(x = .data[[variable]] )) +
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#2171B5") +
    coord_flip() +
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())
}


#' Answer index graph function
#'
#' This function allows you to produce a ggplot graph plot
#'
#' @param data A dataset with the distribution of answers per level of the grouping variable
#' @param variable The variable you want to plot
#' @return A ggplot bar plot graph
#' @export
#get_index()


get_bar_plot_index <- function(data, grouping_var, title = "Plot_title") {
  
  
  levels_ans = c("Very likely", "Somewhat likely", "Somewhat unlikely", "Very unlikely")
  
  data %>%
    mutate(answer = factor(answer, levels = levels_ans)) %>%
    ggplot(aes(fill =  answer, y = index, x = {{grouping_var}})) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle(title) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    scale_fill_brewer(palette="Blues", direction = -1) +
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) 
  
  
  }



