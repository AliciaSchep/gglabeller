
#' gglabeller_example
#'
#' Example output from gglabeller function
#'
#' @format list with three elements: the plot, the indices for rows of data
#' that are labelled, and the code to regenerate the plot
"gglabeller_example"

#' @import shiny miniUI ggrepel ggplot2
gglabeller_ui <- function(geom, width, height, ...){
  
  if (geom == "label"){
    defaults <- modifyList(formals(geom_label_repel), list(...))
  } else{
    defaults <- modifyList(formals(geom_text_repel), list(...))
  }

  miniPage(
    gadgetTitleBar("gglabeller: Label points by selecting or brushing"),
    miniTabstripPanel(
      miniTabPanel("Plot", icon = icon("area-chart"),
                   miniContentPanel(
                     plotOutput("plot", click = "plot_click",
                                brush = brushOpts("plot_brush",resetOnNew = TRUE),
                                width = width,
                                height = height)
                   ),
                   miniButtonBlock(actionButton("all", "Label All"),
                                   actionButton("remove", "Remove All"))
      ),
      miniTabPanel("Parameters", icon = icon("sliders"),
                   miniContentPanel(
                     if (geom == "label"){
                      selectizeInput("params",
                                    "geom_label_repel parameter",
                                    choices = c("nudge_x",
                                                "nudge_y",
                                                "stat",
                                                "parse",
                                                "segment.color",
                                                "segment.size",
                                                "segment.alpha",
                                                "min.segment.length",
                                                "label.size",
                                                "force",
                                                "max.iter",
                                                "xlim",
                                                "ylim",
                                                "box.padding",
                                                "point.padding",
                                                "label.padding",
                                                "label.r",
                                                "direction"))
                       } else{
                         selectizeInput("params",
                                        "geom_label_repel parameter",
                                        choices = c("nudge_x",
                                                    "nudge_y",
                                                    "stat",
                                                    "parse",
                                                    "segment.color",
                                                    "segment.size",
                                                    "segment.alpha",
                                                    "min.segment.length",
                                                    "force",
                                                    "max.iter",
                                                    "xlim",
                                                    "ylim",
                                                    "box.padding",
                                                    "point.padding",
                                                    "direction"))
                                                },
                     conditionalPanel("input.params == 'nudge_x'",
                                      numericInput("nudge_x",NULL,value = defaults$nudge_x)),
                     conditionalPanel("input.params == 'nudge_y'",
                                      numericInput("nudge_y",NULL,value = defaults$nudge_y)),
                     conditionalPanel("input.params == 'stat'",
                                      textInput("stat",NULL,value = defaults$stat)),
                     conditionalPanel("input.params == 'parse'",
                                      checkboxInput("parse",NULL,value = defaults$parse)),
                     conditionalPanel("input.params == 'segment.color'",
                                      textInput("segment.color", NULL, value = defaults$segment.color)),
                     conditionalPanel("input.params == 'segment.size'",
                                      numericInput("segment.size", NULL, value = defaults$segment.size)),
                     conditionalPanel("input.params == 'segment.alpha'",
                                      numericInput("segment.alpha", NULL, value = defaults$segment.alpha)),
                     conditionalPanel("input.params == 'min.segment.length'",
                                      numericInput("min.segment.length",NULL,value = as.numeric(eval(defaults$min.segment.length))),
                                      selectizeInput("min.segment.length_units", "units",
                                                     choices = c("lines","npc",
                                                                 "cm","inches",
                                                                 "points","bigpts",
                                                                 "grobwidth","grobheight",
                                                                 "native"),
                                                     selected = attr(eval(defaults$min.segment.length), "unit"))),
                     if (geom == "label") conditionalPanel("input.params == 'label.size'",
                                      numericInput("label.size", NULL, value = defaults$label.size)) else NULL,
                     conditionalPanel("input.params == 'force'",
                                      numericInput("force", NULL, value = defaults$force)),
                     conditionalPanel("input.params == 'max.iter'",
                                      numericInput("max.iter", NULL, value = defaults$max.iter)),
                     conditionalPanel("input.params == 'xlim'",
                                      numericInput("xlim_min", "min", value = defaults$xlim[1]),
                                      numericInput("xlim_max", "max", value = defaults$xlim[2])),
                     conditionalPanel("input.params == 'ylim'",
                                      numericInput("ylim_min", "min", value = defaults$ylim[1]),
                                      numericInput("ylim_max", "max", value = defaults$ylim[2])),
                     conditionalPanel("input.params == 'label.size'",
                                      numericInput("label.size",NULL, value = defaults$label.size)),
                     conditionalPanel("input.params == 'box.padding'",
                                      numericInput("box.padding",NULL,value = as.numeric(eval(defaults$box.padding))),
                                      selectizeInput("box.padding_units", "units",
                                                     choices = c("lines","npc",
                                                                 "cm","inches",
                                                                 "points","bigpts",
                                                                 "grobwidth","grobheight",
                                                                 "native"),
                                                     selected = attr(eval(defaults$box.padding), "unit"))),
                     conditionalPanel("input.params == 'point.padding'",
                                      numericInput("point.padding",NULL,value = as.numeric(eval(defaults$point.padding))),
                                      selectizeInput("point.padding_units", "units",
                                                     choices = c("lines","npc",
                                                                 "cm","inches",
                                                                 "points","bigpts",
                                                                 "grobwidth","grobheight",
                                                                 "native"),
                                                     selected = attr(eval(defaults$point.padding), "unit"))),
                     if (geom == "label") conditionalPanel("input.params == 'label.r'",
                                      numericInput("label.r",NULL, value = as.numeric(eval(defaults$label.r))),
                                      selectizeInput("label.r_units", "units",
                                                     choices = c("lines","npc",
                                                                 "cm","inches",
                                                                 "points","bigpts",
                                                                 "grobwidth","grobheight",
                                                                 "native"),
                                                     selected = attr(eval(defaults$label.r), "unit"))) else NULL,
                     if (geom == "label") conditionalPanel("input.params == 'label.padding'",
                                      numericInput("label.padding",NULL, value = as.numeric(eval(defaults$label.padding))),
                                      selectizeInput("label.padding_units", "units",
                                                     choices = c("lines","npc",
                                                                 "cm","inches",
                                                                 "points","bigpts",
                                                                 "grobwidth","grobheight",
                                                                 "native"),
                                                     selected = attr(eval(defaults$label.padding), "unit"))) else NULL,
                     conditionalPanel("input.params == 'direction'",
                                      selectizeInput("direction",NULL,
                                                     choices = c("both","x","y"),
                                                     selected = match.arg(eval(defaults$direction),c("both","y","x"))))
                   ))
    ))
}

#' gglabeller
#'
#' Shiny gadget for selecting points to label in a ggplot object
#'
#' Click on a point or brush over a set of points to label them. Clicking
#' on a labelled point or brushing over a set of points that are all labelled
#' will remove the labels. Brushing over some points that are labelled and some
#' that are not will add labels to the ones that are not.
#'
#' gglabeller sses ggrepel for controlling the position of labels. The parameters tab
#' can be used to adjust ggrepel parameters.
#' @param gg ggplot object
#' @param mapping aesthetics mapping, from \code{\link[ggplot2]{aes}}
#' @param data data.frame with data for plotting
#' @param geom geom to use -- "text" or "label"
#' @param ... additional arguments to pass to \code{\link[ggrepel]{geom_text_repel}} or
#' \code{\link[ggrepel]{geom_label_repel}}
#' @param width width of plot, as percent or pixels
#' @param height height of plot, as percent or pixels
#' @return Upon clicking the "Done" button, the gadget returns a list with three elements:
#' \describe{
#' \item{plot}{The plot itself}
#' \item{ix}{The indices of the rows of the data used for labelling}
#' \item{code}{a character vector with a code snippet for recreating the plot reproducibly}
#' }
#' @export
#' @author Alicia Schep
#' @import ggrepel ggplot2 shiny miniUI
#' @importFrom stats runif
#' @importFrom utils modifyList
#' @examples
#'
#' if (interactive()){
#'
#'   p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#'   plabelled <- gglabeller(p, mapping = aes(label = rownames(mtcars)))
#'
#' }
gglabeller <- function(gg,
                       mapping = gg$mapping,
                       data = gg$data,
                       geom = c("text","label"),
                       ...,
                       width = "100%",
                       height = "100%"){

  thecall <- match.call()
  call_list <- as.list(thecall)
  geom <- match.arg(geom)
  stopifnot(inherits(mapping,"uneval"))
  stopifnot("label" %in% names(mapping))
  if ("data" %in% names(call_list)){
    recode <- paste0("gglabeller_data <- ",call_list$data,"; ")
  } else{
    recode <- paste0("gglabeller_data <- ",call_list$gg,"$data; ")
  }
  
  if (!(rlang::quo_text(mapping$label) %in% colnames(data))) {
     recode <- paste0(recode, "gglabeller_data$gglabeller_labels <- ", rlang::quo_text(mapping$label),"; ")
  } else {
    recode <- paste0(recode, "gglabeller_data$gglabeller_labels <- gglabeller_data$", rlang::quo_text(mapping$label),"; ")
  }
  
  labels <- rlang::eval_tidy(mapping$label, data = data)
  data$gglabeller_labels <- labels
  label <- "gglabeller_labels"
  mapping$label <- rlang::new_quosure(rlang::sym("gglabeller_labels"), 
                                      env = parent.frame())
  
  # random seed
  seed <- as.integer(runif(1) * 10e6)

  ui <- gglabeller_ui(geom,width,height,...)
  
  dot_args <- list(...)
  if (geom == "label"){
    dot_dot_args <- dot_args[which(!(names(dot_args) %in% names(formals(geom_label_repel))))]
  } else {
    dot_dot_args <- dot_args[which(!(names(dot_args) %in% names(formals(geom_text_repel))))]
  }
  

  # Server side
  server <- function(input, output, session) {


    ix <- reactiveValues(rows = seq_len(nrow(data)))

    subset_data <- reactive({
      data[ix$rows,label] <- ""
      data
    })


    repel_geom <- reactive({
      if (geom == "label"){
        geom_label_args <- c(alist(mapping = mapping,
                       data = subset_data(),
                       nudge_x = input$nudge_x,
                       nudge_y = input$nudge_y,
                       segment.color = if (nchar(input$segment.color) > 0)
                         input$segment.color else NULL,
                       segment.size = input$segment.size,
                       segment.alpha = input$segment.alpha,
                       min.segment.length = unit(input$min.segment.length,
                                                 units = input$min.segment.length_units),
                       parse = input$parse,
                       force = input$force,
                       max.iter = input$max.iter,
                       label.size = input$label.size,
                       stat = input$stat,
                       xlim = c(input$xlim_min,input$xlim_max),
                       ylim = c(input$ylim_min,input$ylim_max),
                       box.padding = unit(input$box.padding,
                                          units = input$box.padding_units),
                       point.padding = unit(input$point.padding,
                                            units = input$point.padding_units),
                       label.padding = unit(input$label.padding,
                                            units = input$label.padding_units),
                       label.r = unit(input$label.r, units = input$label.r_units),
                       direction = input$direction),
                       dot_dot_args)
        do.call(geom_label_repel, geom_label_args)
      } else{
        geom_text_args <- c(alist(mapping = mapping,
                        data = subset_data(),
                        nudge_x = input$nudge_x,
                        nudge_y = input$nudge_y,
                        segment.color = if (nchar(input$segment.color) > 0)
                          input$segment.color else NULL,
                        segment.size = input$segment.size,
                        segment.alpha = input$segment.alpha,
                        min.segment.length = unit(input$min.segment.length,
                                                  units = input$min.segment.length_units),
                        parse = input$parse,
                        force = input$force,
                        max.iter = input$max.iter,
                        stat = input$stat,
                        xlim = c(input$xlim_min,input$xlim_max),
                        ylim = c(input$ylim_min,input$ylim_max),
                        box.padding = unit(input$box.padding,
                                           units = input$box.padding_units),
                        point.padding = unit(input$point.padding,
                                             units = input$point.padding_units),
                        direction = input$direction),
                        dot_dot_args)
        do.call(geom_text_repel, geom_text_args)
      }

    })

    # Render the plot
    output$plot <- renderPlot({
      set.seed(seed)
      out = gg
      if (length(ix$rows) < nrow(data)){
        out <- out + repel_geom()
      }
      
      out
    })

    observeEvent(input$plot_click,{
      point_ix <- which(nearPoints(data, input$plot_click, maxpoints = 1,
                                   allRows = TRUE)$selected_)
      if (length(point_ix) == 1){
        if (point_ix %in% ix$rows){
          ix$rows <- ix$rows[ix$rows != point_ix]
        } else{
          ix$rows <- c(ix$rows, point_ix)
        }
      }

    })

    observeEvent(input$plot_brush,{
      point_ix <- which(brushedPoints(data, input$plot_brush,
                                   allRows = TRUE)$selected_)
      if (length(point_ix) >= 1){
        already_labelled <- !(point_ix %in% ix$rows)
        if (all(already_labelled)){
          ix$rows <-
            c(ix$rows, point_ix[already_labelled])
        } else{
          ix$rows <- ix$rows[!(ix$rows %in% point_ix)]
        }
      }

    })

    observeEvent(input$remove,{
      ix$rows <- seq_len(nrow(data))
    })

    observeEvent(input$all,{
      ix$rows <- c()
    })


    observeEvent(input$done, {
      set.seed(seed)
      out = gg

      if (length(ix$rows) < nrow(data)){
        out <- out + repel_geom()
      }

      if (geom == "text"){
        defaults <- formals_to_string(formals(geom_text_repel))
        parameters <- list(nudge_x = input$nudge_x,
                           nudge_y = input$nudge_y,
                           segment.color = if (nchar(input$segment.color) > 0) single_q(input$segment.color) else "NULL",
                           segment.size = input$segment.size,
                           segment.alpha = if (!is.na(input$segment.alpha)) input$segment.alpha else "NULL",
                           min.segment.length = if (input$min.segment.length_units == "lines") input$min.segment.length 
                                else unit_to_string(input$min.segment.length, input$min.segment.length_units),
                           parse = input$parse,
                           force = input$force,
                           max.iter = input$max.iter,
                           stat = paste0("'",input$stat,"'"),
                           xlim = lim_to_string(input$xlim_min,input$xlim_max),
                           ylim = lim_to_string(input$ylim_min,input$ylim_max),
                           box.padding = if (input$box.padding_units == "lines") input$box.padding 
                                else unit_to_string(input$box.padding, input$box.padding_units),
                           point.padding = if (input$point.padding_units == "lines") input$point.padding 
                                else unit_to_string(input$point.padding, input$point.padding_units),
                           direction = single_q(input$direction))
      } else{
        defaults <- formals_to_string(formals(geom_label_repel))
        parameters <- list(nudge_x = input$nudge_x,
                           nudge_y = input$nudge_y,
                           segment.color = if (nchar(input$segment.color) > 0) single_q(input$segment.color) else "NULL",
                           segment.size = input$segment.size,
                           segment.alpha = if (!is.na(input$segment.alpha)) input$segment.alpha else "NULL",
                           min.segment.length = if (input$min.segment.length_units == "lines") input$min.segment.length 
                                else unit_to_string(input$min.segment.length, input$min.segment.length_units),
                           parse = input$parse,
                           force = input$force,
                           max.iter = input$max.iter,
                           label.size = input$label.size,
                           stat = paste0("'",input$stat,"'"),
                           xlim = lim_to_string(input$xlim_min,input$xlim_max),
                           ylim = lim_to_string(input$ylim_min,input$ylim_max),
                           box.padding = if (input$box.padding_units == "lines") input$box.padding 
                              else unit_to_string(input$box.padding, input$box.padding_units),
                           point.padding = if (input$point.padding_units == "lines") input$point.padding 
                              else unit_to_string(input$point.padding, input$point.padding_units),
                           label.padding = if (input$label.padding_units == "lines") input$label.padding 
                              else unit_to_string(input$label.padding, input$label.padding_units),
                           label.r = if (input$label.r_units == "lines") input$label.r 
                              else unit_to_string(input$label.r, input$label.r_units),
                           direction = single_q(input$direction))

      }
      non_default <- sapply(names(parameters), function(x) parameters[[x]] != defaults[[x]])
      parameters <- c(parameters[non_default], formals_to_string(dot_dot_args))
      if ("direction" %in% names(parameters) && parameters$direction == "'both'")
        parameters$direction <- NULL

      params_string <- paste(names(parameters), parameters,
                             sep = " = ", collapse = ",")

      if (nchar(params_string) > 0) params_string <- paste0(", ", params_string)

      mapping_string <- paste0("mapping = aes(",
                               paste(names(mapping), vapply(mapping, rlang::quo_text, ""),
                                     sep = " = ", collapse = ", "),
                               ")")
      recode <- paste0("set.seed(",
                       seed,
                       ");",
                       recode,
                       "gglabeller_data[",
                       condense_ix(ix$rows),
                       ",'",
                       label,
                       "'] <- ''; ",
                       call_list$gg,
                       " + ",
                       "geom_",
                       geom,
                       "_repel(data = gglabeller_data,",
                       mapping_string,
                       params_string,
                       ")")

      ix_out <- seq_len(nrow(data))
      ix_out <- ix_out[!(ix_out %in% ix$rows)]
      stopApp(list(plot = out,
                   ix = ix_out,
                   code = recode))
    })
  }

  runGadget(ui, server)

}

unit_to_string <- function(x, units){
  paste0("unit(",x,", '",units,"')")
}

lim_to_string <- function(x1,x2){
  paste0("c(",x1,", ",x2,")")
}

single_q <- function(x){
  paste0("'",x,"'")
}

formals_to_string <- function(l){
  lapply(l,function(x) gsub("\\\"","'",deparse(x) ))
}


condense_ix <- function(ix){
  if (length(ix) < 2) return(paste0("c(",ix,")"))
  ix <- sort(ix)
  d <- diff(ix)
  jumps <- which(d > 1)
  out <- paste(ix[c(1,jumps+1)],ix[c(jumps,length(ix))],sep = ":")
  out <- gsub("^([[:digit:]]+):(\\1)$","\\1",out)
  out <- paste(out, collapse = ", ")
  out <- paste0("c(",out,")")
  out
}





