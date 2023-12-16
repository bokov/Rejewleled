library(shiny);
library(DT);
source('bejewered.R');

# Define server logic required to draw a histogram
function(input, output, session) {
  rvbjw <-createBJW(20,8,reactive=T);

  output$bjwdt <- DT::renderDataTable({
    req(rvbjw$state);
    bjwdat <- isolate(rvbjw$data);
    DT::datatable(bjwdat
                  ,options=list(paging=F,searching=F,dom='t'
                                ,columnDefs = list(list(targets = '_all'
                                                        ,visible = TRUE
                                                        ,title = ''
                                                        ,orderable=F)))
                  #,selection=list(mode='single',target='cell',selected=cbind(3,3))
                  ,selection = 'none'
                  )
  },server = F);

  observeEvent(input$bjwdt_cell_clicked,{
    clicked <-input$bjwdt_cell_clicked;
    runjs("$('.bw_selected1').removeClass('bw_selected1')");
    if(length(clicked)>0){
      setbjwcellclass <- sprintf("$('#bjwdt table.dataTable tbody tr:nth-child(%s) td:nth-child(%s)').addClass('bw_selected1')"
                        ,clicked$row,clicked$col+1);
      runjs(setbjwcellclass);
    }
    # TODO: looks like we can manage first and likely second selection via JS
    #       so the DT needs to only be re-drawn when the contents of cells
    #       change. Adapt the below to reactive envrionment accordingly. Also,
    #       will likely not need to use while loops... rather, run every step
    #       once conditional on value of bjwdf$state
    # # if there is no bjw$selected1 that means this is the first half of a paired
    # # selection, so update accordingly and return
    # if(length(selected1)==0){bjw$selected1 <-id; return(bjw)};
    # # if the selection is repeated that's a signal to cancel the selection, select
    # # nothing
    # if(selected1 == id) {bjw$selected1 <- c(); return(bjw)};
    # # if the selection is not adjacent, that's a signal to select the new cell
    # # instead as the first half of the selection
    # if(!isadjacent_bjw_id(id,bjw=bjw)){bjw$selected1 <-id; return(bjw)};
    # # only if none of the above conditions are met, can we swap
  })

  observeEvent(input$debug,browser());
}
