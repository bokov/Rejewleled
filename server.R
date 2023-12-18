library(shiny);
library(DT);
library(dplyr);
source('bejewered.R');

dtbjw_options=list(paging=F,searching=F,dom='t',columnDefs = list(
  list(targets = '_all',visible = TRUE,title = '',orderable=F)));


function(input, output, session) {
  rvbjw <-createBJW(20,8,reactive=T);
  rvbjw$plotstate <- 'readytomatch';
  rvbjw$refresh <- 0;

  output$bjwdt <- DT::renderDataTable({
      bjwdat <- isolate(rvbjw$data);
      message('printing update for: ',rvbjw$plotstate);
      rvbjw$obsstate <- rvbjw$plotstate;
      DT::datatable(bjwdat,options=dtbjw_options,selection = 'none')
    },server = F);

  observeEvent(rvbjw$refresh, {
    output$score <- renderText(paste('Score: ', totalscore.bjw(rvbjw)))
    if(rvbjw$refresh <= 2) return()
    if(rvbjw$obsstate == 'baseline'){
      if(length(rvbjw$selected1) == 1){
        selectedrc <- parse_bjw_id(rvbjw$selected1, 'rc')
        setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected1')", selectedrc[1], selectedrc[2])
        runjs(setbjwcellclass)
      }
      return()
    }
    if(rvbjw$obsstate == 'swapped'){
      selected1rc <- parse_bjw_id(rvbjw$selected1, 'rc')
      selected2rc <- parse_bjw_id(rvbjw$selected2, 'rc')
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected1')", selected1rc[1], selected1rc[2])
      runjs(setbjwcellclass)
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected2')", selected2rc[1], selected2rc[2])
      runjs(setbjwcellclass)
      postswap_result <- match.bjw(rvbjw, update_bjw = FALSE)
      if(any(is.na(postswap_result$data))){
        rvbjw$data <- postswap_result$data
        rvbjw$score <- postswap_result$score
        rvbjw$selected2 <- rvbjw$selected1 <- c()
        rvbjw$plotstate <- 'readytocompact'
      } else {
        rvbjw$data <- rvbjw$preswap_data
        rvbjw$selected2 <- rvbjw$preswap_data <- NULL
        rvbjw$plotstate <- 'baseline'
      }
      return()
    }
    nas <- which(is.na(rvbjw$data), arr.ind = TRUE)
    apply(nas, 1, function(xx) runjs(sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_filled')", xx[1], xx[2])))
    if(rvbjw$obsstate == 'readytomatch'){
      if(any(is.na(match.bjw(rvbjw)$data))){
        rvbjw$plotstate <- 'readytocompact'
      } else {
        rvbjw$plotstate <- 'baseline'; return()
      }
    } else if(rvbjw$obsstate == 'readytocompact'){
      pre_compact_data <- rvbjw$data
      post_compact_data <- compact.bjw(rvbjw)$data
      if(!identical(post_compact_data, pre_compact_data)){
        rvbjw$plotstate <- 'readytomatch'; return()
      } else if(any(is.na(post_compact_data))) {
        rvbjw$plotstate <- 'readytofill'; return()
      } else {
        rvbjw$plotstate <- 'baseline'; return()
      }
    } else if(rvbjw$obsstate == 'readytofill'){
      refill.bjw(rvbjw)
      rvbjw$plotstate <- 'readytomatch'; return()
    }
  })

  observeEvent(input$bjwdt_cell_clicked,{
    if(isolate(rvbjw$state=='readytocheck')) return();
    clicked <-input$bjwdt_cell_clicked;
    if(length(clicked)==0) return();
    clicked <- with(clicked,isolate(parse_bjw_id(c(row,col+1),bjw=rvbjw)));
    selectedid <- isolate(rvbjw$selected1);
    if(identical(selectedid,clicked$id)){
      rvbjw$selected1 <- c();
      runjs("$('.bw_selected1').removeClass('bw_selected1')");
      return();
    } else if(length(selectedid)==0||isolate(!isadjacent_bjw_id(clicked$id,bjw=rvbjw))){
      rvbjw$selected1 <- clicked$id;
      runjs("$('.bw_selected1').removeClass('bw_selected2')");
      runjs("$('.bw_selected1').removeClass('bw_selected1')");
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected1')"
                                 ,clicked$rc[1],clicked$rc[2]);
      runjs(setbjwcellclass);
      return();
    } else if(isolate(isadjacent_bjw_id(clicked$id,bjw=rvbjw))){
      message('swapping ',clicked$id,' and ',selectedid);
      selectedrc <- parse_bjw_id(selectedid,'rc');
      rvbjw$preswap_data <- isolate(rvbjw$data);
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected2')"
                                 ,clicked$rc[1],clicked$rc[2]);
      runjs(setbjwcellclass);
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected1')"
                                 ,selectedrc[1],selectedrc[2]);
      runjs(setbjwcellclass);
      isolate(swap.bjw(rvbjw,clicked$id));
      rvbjw$selected2 <- clicked$id;
      rvbjw$plotstate <- 'swapped';
      return();
    }
  })

  observe({
    invalidateLater(50, session);  # Invalidate and re-trigger after 500 milliseconds (0.5 seconds)
    rvbjw$refresh <- isolate(rvbjw$refresh + 1);
    output$debugoutput <- renderText({
      paste(Sys.time(),': plotstate=',rvbjw$plotstate,', obsstate=',rvbjw$obsstate,'
            , selected1=',rvbjw$selected1,', selected2=',coalesce(rvbjw$selected2,'NULL'))
    });
  })


  observeEvent(input$debug,browser());
}
