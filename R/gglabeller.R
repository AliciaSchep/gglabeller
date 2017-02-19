



#' @import shiny miniUI ggrepel ggplot2
gglabeller_ui <- function(geom, width, height,...){

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


text_ui <- function(width, height, ...){
  defaults <- modifyList(formals(geom_label_repel), list(...))

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
                     selectizeInput("params",
                                    "geom_text_repel parameter",
                                    choices = c("nudge_x",
                                                "nudge_y",
                                                "direction")),
                     conditionalPanel("input.params == 'nudge_x'",
                                      numericInput("nudge_x",NULL,value = defaults$nudge_x)),
                     conditionalPanel("input.params == 'nudge_y'",
                                      numericInput("nudge_y",NULL,value = defaults$nudge_y)),
                    conditionalPanel("input.params == 'direction'",
                                      selectizeInput("direction",NULL,
                                                     choices = c("both","x","y"),
                                                     selected =  match.arg(defaults$direction,c("both","x","y"))))
                   ))
    ))}


#' gglabeller
#'
#' Shiny gadget for selecting points to label in a ggplot object
#'
#' Uses ggrepel for controlling position of labels. Parameters tab of
#' widget can be used to adjust ggrepel parameters.
#' @param gg ggplot object
#' @param mapping
#' @param data
#' @param geom geom to use -- "text" or "label"
#' @param ... additional arguments to pass to \code{\link[ggrepel]{geom_text_repel}} or
#' \code{\link[ggrepel]{geom_label_repel}}
#' @param width width of plot
#' @param height height of plot
#' @export
#' @author Alicia Schep
#' @import ggrepel ggplot2 shiny miniUI
#' @examples
#'
#' if (interactive()){
#'
#'   p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#'   gglabeller(p, mapping = aes(label = rownames(mtcars)))
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
  if (is.name(mapping$label)){
    if (as.character(mapping$label) %in% colnames(data)){
      label = as.character(mapping$label)
    } else{
      recode <- paste0(recode, "gglabeller_data$gglabeller_labels <- ",
                               mapping$label,
                               "; ")
      labels <- eval(mapping$label)
      if (length(labels) != nrow(data)){
        stop("labels don't match number of rows of data")
      }
      data$gglabeller_labels <- labels
      label <- "gglabeller_labels"
      mapping$label <- as.name(label)
    }
  } else if (is.call(mapping$label)){
    recode <- paste0(recode, "gglabeller_data$gglabeller_labels <- ",
                     gsub("\\\"","'",deparse(mapping$label)),
                           "; ")
    labels <- eval(mapping$label)
    if (length(labels) != nrow(data)){
      stop("labels don't match number of rows of data")
    }
    data$gglabeller_labels <- labels
    label <- "gglabeller_labels"
    mapping$label <- as.name(label)
  } else if (is.atomic(mapping$label)){
    recode <- paste0(recode, "gglabeller_data$gglabeller_labels <- c(",
                           paste(mapping$label,sep = ", "),
                           "); ")
    labels <- mapping$label
    if (length(labels) != nrow(data)){
      stop("labels don't match number of rows of data")
    }
    data$gglabeller_labels <- labels
    label <- "gglabeller_labels"
    mapping$label <- as.name(label)
  } else{
    stop("Invalid mapping argument")
  }

  # random seed
  seed <- as.integer(runif(1) * 10e6)

  ui <- gglabeller_ui(geom,width,height,...)

  # Server side
  server <- function(input, output, session) {


    ix <- reactiveValues(rows = seq_len(nrow(data)))

    subset_data <- reactive({
      data[ix$rows,label] <- ""
      data
    })


    repel_geom <- reactive({
      if (geom == "label"){
        geom_label_repel(mapping = mapping,
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
                       direction = input$direction)
      } else{
        geom_text_repel(mapping = mapping,
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
                        direction = input$direction)
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
        defaults <- lapply(formals(geom_text_repel),
                           function(x) gsub("\\\"","'",deparse(x) ))
        parameters <- list(nudge_x = input$nudge_x,
                           nudge_y = input$nudge_y,
                           segment.color = if (nchar(input$segment.color) > 0) input$segment.color else "NULL",
                           segment.size = input$segment.size,
                           segment.alpha = if (!is.na(input$segment.alpha)) input$segment.alpha else "NULL",
                           min.segment.length = unit_to_string(input$min.segment.length,input$min.segment.length_units),
                           parse = input$parse,
                           force = input$force,
                           max.iter = input$max.iter,
                           stat = paste0("'",input$stat,"'"),
                           xlim = lim_to_string(input$xlim_min,input$xlim_max),
                           ylim = lim_to_string(input$ylim_min,input$ylim_max),
                           box.padding = unit_to_string(input$box.padding, input$box.padding_units),
                           point.padding = unit_to_string(input$point.padding, input$point.padding_units),
                           direction = single_q(input$direction))
      } else{
        defaults <- lapply(formals(geom_label_repel),
                           function(x) gsub("\\\"","'",deparse(x) ))
        parameters <- list(nudge_x = input$nudge_x,
                           nudge_y = input$nudge_y,
                           segment.color = if (nchar(input$segment.color) > 0) input$segment.color else "NULL",
                           segment.size = input$segment.size,
                           segment.alpha = if (!is.na(input$segment.alpha)) input$segment.alpha else "NULL",
                           min.segment.length = unit_to_string(input$min.segment.length,input$min.segment.length_units),
                           parse = input$parse,
                           force = input$force,
                           max.iter = input$max.iter,
                           label.size = input$label.size,
                           stat = paste0("'",input$stat,"'"),
                           xlim = lim_to_string(input$xlim_min,input$xlim_max),
                           ylim = lim_to_string(input$ylim_min,input$ylim_max),
                           box.padding = unit_to_string(input$box.padding, input$box.padding_units),
                           point.padding = unit_to_string(input$point.padding, input$point.padding_units),
                           label.padding = unit_to_string(input$label.padding, input$label.padding_units),
                           label.r = unit_to_string(input$label.r, input$label.r_units),
                           direction = single_q(input$direction))

      }
      non_default <- sapply(names(parameters), function(x) parameters[[x]] != defaults[[x]])
      parameters <- parameters[non_default]
      if ("direction" %in% names(parameters) && parameters$direction == "'both'")
        parameters$direction <- NULL

      params_string <- paste(names(parameters), parameters,
                             sep = " = ", collapse = ",")

      if (nchar(params_string) > 0) params_string <- paste0(", ", params_string)

      mapping_string <- paste0("mapping = aes(",
                               paste(names(mapping), mapping,
                                     sep = " = ", collapse = ","),
                               ")")
      recode <- paste0(recode,
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

      stopApp(list(plot = out,
                   parameters = parameters,
                   ix = ix$rows,
                   seed = seed,
                   call = thecall,
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





