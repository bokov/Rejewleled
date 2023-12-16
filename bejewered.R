# functions ----

createBJW <- function(nr=sample(3:8,1),nc=sample(3:8,1)
                      ,colors=sample(1:4,nr*nc,rep=T),reactive=F){
  out <- if(reactive) reactiveValues() else new.env();
  out$data <- matrix(colors,nr,nc);
  out$selected1 <- c();
  out$state <- '0sel';
  out$score <- data.frame(length=factor(),type=factor(),Freq=integer(),rc=character());
  if(!reactive) class(out) <- c('bjw','list','environment');
  out;
}

parse_bjw_id <- function(identifier=bjw$selected1,retval=c('both','rc','id')
                       ,prefix=getOption('bjw.cell.prefix','bjwtd_')
                         ,bjw=NA,rcmax){
  out <- list();
  if(missing(rcmax)) rcmax <- if(!is(bjw,'bjw')) c(8,8) else dim(bjw$data);
  if(is.numeric(identifier) && length(identifier) == 2){
    out<-list(rc=identifier,id=paste0(prefix,row,'_',col));
  } else if(is.character(identifier) && length(identifier) == 1 &&
            grepl('.*_[0-9]*_[0-9]*$',identifier)){
    out<-list(rc=gsub('^[^_]*_','',identifier) %>% strsplit('_') %>% unlist %>%
                as.numeric()
              ,id = identifier);
  } else stop('The selected2 argument to the select.bjw() function must be either a character of length 1 or numeric of length 2, specifying either the ID or the coordinates of the cell you wish to select');
  if(!all(between(out$rc,1,rcmax))){
    msg <- c('parse_bjw_id as given an identifier (',out$id,' or ',paste0(out$rc,collapse=', '),') which might map to a cell that does not exist, proceed with caution');
    if(!is(bjw,'bjw')) warning(msg) else stop(msg);
  };
  switch(match.arg(retval),rc=out$rc,id=out$id,out);
};

get_rc.bjw <- function(bjw){
  if(is.null(bjw$selected1)) return(NULL);
  rbind(parse_bjw_id(bjw=bjw,retval='rc'))};

isadjacent_bjw_id <- function(ident2,ident1=bjw$selected1,bjw){
  delta<-abs(parse_bjw_id(ident1,'rc')-parse_bjw_id(ident2,'rc'));
  all(0:1 %in% delta);
  };

replace_contents.bjw <- function(bjw,data,score,selected1,sync_to=NA){
  if(!missing(data)){
    bjw$data <- data;
    if(!missing(sync_to)) sync_to$data <- data;
  };
  if(!missing(score)){
    bjw$score <- score;
    if(!missing(sync_to)) sync_to$score <- score;
  };
  if(!missing(selected1)){
    bjw$selected1 <- selected1;
    if(!missing(sync_to)) sync_to$selected1 <- selected1;
  };
  bjw;
}

select.bjw <- function(bjw,selected2,selected1=bjw$selected1){
  id <- parse_bjw_id(selected2,'id',bjw=bjw);
  # if there is no bjw$selected1 that means this is the first half of a paired
  # selection, so update accordingly and return
  if(length(selected1)==0){bjw$selected1 <-id; return(bjw)};
  # if the selection is repeated that's a signal to cancel the selection, select
  # nothing
  if(selected1 == id) {bjw$selected1 <- c(); return(bjw)};
  # if the selection is not adjacent, that's a signal to select the new cell
  # instead as the first half of the selection
  if(!isadjacent_bjw_id(id,bjw=bjw)){bjw$selected1 <-id; return(bjw)};
  # only if none of the above conditions are met, can we swap
  # TODO: only permit swaps that would result in scoring!
  # the below lines are not yet tested
  preswap_snapshot <- bjw$data;
  swap.bjw(ident2=selected2, bjw=bjw);
  postswap_result <- match.bjw(bjw,update_bjw =F);
  if(any(is.na(postswap_result$data))){
    bjw$data <- postswap_result$data;
    bjw$score <- postswap_result$score;
    bjw$selected1 <- c();
    outerUpdate.bjw(bjw);
  } else {
    bjw$data <- preswap_snapshot;
    message('This swap would not produce any matches. Reverting.')
  }
  bjw;
};

get.bjw <- function(bjw,id=bjw$selected1){
  sapply(id,function(xx) with(parse_bjw_id(xx,bjw=bjw)
                              ,bjw$data[rc[1],rc[2]]));
  };


set.bjw <- function(bjw,id=bjw$selected1,values){
  if(length(values)!=length(id) && length(values)>1){
    stop('In set.bjw(), the values argument must be either length 1 or the same length as the id argument')};
  out <- get.bjw(bjw,id);
  mapply(function(xx,vv) with(parse_bjw_id(xx,bjw=bjw),bjw$data[rc[1],rc[2]]<-vv),id,values);
  message('set.bjw() is returning the old values it has just replaced');
  out;
  };

swap.bjw <- function(bjw,ident2,ident1=bjw$selected1){
  id2 <- parse_bjw_id(ident2,'id',bjw=bjw);
  id1 <- parse_bjw_id(ident1,'id',bjw=bjw);
  set.bjw(bjw,id2,set.bjw(bjw,id1,get.bjw(bjw,id2)));
  bjw;
}

# TODO:
compact.bjw <- function(bjw){
  bjw$data <- apply(bjw$data,2,function(xx) c(rep_len(NA,sum(is.na(xx))),na.omit(xx)));
  bjw;
}

row_bjw_analyze <- function(xx){
  scoretemplate <- data.frame(length=factor(),type=factor(),Freq=integer());
  rl <- rle(xx);
  idx <- with(rl,which(lengths>=3 & !is.na(values)));
  score <- with(rl,table(length=lengths[idx],type=values[idx]) %>%
                  as.data.frame() %>% rbind(scoretemplate,.));
  rl$values[idx] <- NA;
  return(list(score=score,resultrow=inverse.rle(rl)));
}

match.bjw <- function(bjw,update_bjw=T){
  # for each row, look for runs of 3, 4, 5, etc.
  result_x <- apply(bjw$data,1,row_bjw_analyze);
  result_y <- apply(bjw$data,2,row_bjw_analyze);
  # update the counts of individual color and score combos separately (e.g. red triples, red quadruples, etc. then move on to green)
  score_x <-lapply(result_x,function(xx) xx$score) %>% do.call(bind_rows,.) %>% mutate(rc=if(n()>0) 'r' else character());
  score_y <-lapply(result_y,function(xx) xx$score) %>% do.call(bind_rows,.) %>% mutate(rc=if(n()>0) 'r' else character());
  score_xy <- rbind(bjw$score,score_x,score_y) %>% group_by(rc,length,type) %>% summarise(Freq=sum(Freq));
  # after recording the score for each, set those values to NA
  nas_x<-lapply(result_x,function(xx) xx$resultrow) %>% do.call(rbind,.);
  nas_y<-lapply(result_y,function(xx) xx$resultrow) %>% do.call(rbind,.) %>% t;
  nas_xy <- pmax(nas_x,nas_y);
  if(update_bjw){
    # update bjw
    bjw$data <- nas_xy;
    bjw$score <- score_xy;
    bjw;
  } else list(score=score_xy,data=nas_xy);
}

refill.bjw <- function(bjw,refillwith=unique(na.omit(c(bjw$data)))){
  # randomly fill with colors
  nas <- is.na(bjw$data);
  bjw$data[nas] <- sample(refillwith,sum(nas),rep=T);
  bjw;
}

totalscore.bjw <- function(bjw){with(bjw$score,sum(as.numeric(as.character(length))*Freq))};


plot.bjw <- function(x,printscore=F,...){
  message('current score:',totalscore.bjw(x));
  rc<-get_rc.bjw(x);
  out<-x$data;
  if(!is.null(rc)){
    out[rc[1],rc[2]] <- sprintf('[%s]',out[rc[1],rc[2]]);
  } else out[1,1] <- as.character(out[1,1]);
  print(datatable(out
                  ,options=list(paging=F,searching=F
                                ,columnDefs = list(list(targets = '_all'
                                                        , visible = TRUE
                                                        , title = ''
                                                        ,orderable=F))
                                ,dom='t')
  ));
}


innerUpdate.bjw <- function(bjw,verbose=0){
  converged <- F;
  if(verbose) plot.bjw(bjw);
  while(!converged){
    previousstate <- bjw$data;
    compact.bjw(bjw);
    if(verbose) plot.bjw(bjw);
    match.bjw(bjw);
    if(verbose) plot.bjw(bjw);
    converged <- identical(bjw$data,previousstate);
  }
};

outerUpdate.bjw <- function(bjw,verbose=0){
  converged <- F;
  if(verbose) plot.bjw(bjw);
  while(!converged){
    previousstate <- bjw$data;
    refill.bjw(bjw);
    if(verbose) plot.bjw(bjw);
    innerUpdate.bjw(bjw,max(verbose-1,0));
    converged <- identical(bjw$data,previousstate);
  }
}


# TODO:
validate.bjw <- function(bjw){};

# Basic loops:
# bjwdf <- createBJW();
# bjwdf$data;
# repeat the following forever...
# prompt the user for id or rc and convert it to id using parse_bjw_id()
# select.bjw(id,bjw=bjwdf);
# outerUpdate(bjwdf);
# bjwdf$data;