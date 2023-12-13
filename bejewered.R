# functions ----

createBJW <- function(nr=sample(3:8,1),nc=sample(3:8,1)
                      ,colors=sample(palette(),nr*nc,rep=T)){
  out <- new.env();
  out$data <- matrix(colors,nr,nc);
  out$selected1 <- c();
  class(out) <- c('bjw','list','environment');
  #out$score <- list();
  out;
}

parse_bjw_id <- function(identifier,retval=c('both','rc','id')
                         ,prefix=getOption('bjw.cell.prefix','bjwtd_')
                         ,bjw,rcmax){
  out <- list();
  if(missing(rcmax)) rcmax <- if(missing(bjw)) c(8,8) else dim(bjw$data);
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
    if(missing(bjw)) warning(msg) else stop(msg);
  };
  switch(match.arg(retval),rc=out$rc,id=out$id,out);
};

isadjacent_bjw_id <- function(ident2,ident1=bjw$selected,bjw){
  delta<-abs(parse_bjw_id(ident1,'rc')-parse_bjw_id(ident2,'rc'));
  all(0:1 %in% delta);
  };

select.bjw <- function(bjw,selected2,selected1=bjw$selected1){
  id <- parse_bjw_id(bjw,selected2,'id');
  # if there is no bjw$selected1 that means this is the first half of a paired
  # selection, so update accordingly and return
  if(length(selected1)==0){bjw$selected1 <-id; return(bjw)};
  # if the selection is repeated that's a signal to cancel the selection, select
  # nothing
  if(selected1 == id) {bjw$selected1 <- c(); return(bjw)};
  # if the selection is not adjacent, that's a signal to select the new cell
  # instead as the first half of the selection
  if(!isadjacent_bjw_id(bjw,id)){bjw$selected1 <-id; return(bjw)};
  # only if none of the above conditions are met, can we swap
  # swap.bjw(bjw,selected2, selected1);
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

# TODO:
swap.bjw <- function(bjw,ident2,ident1=bjw$selected1){
  id2 <- parse_bjw_id(ident2,'id',bjw=bjw);
  id1 <- parse_bjw_id(ident1,'id',bjw=bjw);
  set.bjw(bjw,id2,set.bjw(bjw,id1,get.bjw(bjw,id2)));
  bjw;
}

# TODO:
compact.bjw <- function(bjw){
  # for each column, move all the non-NA values to the bottom, and fill in the top with NAs
  bjw;
}

# TODO:
match.bjw <- function(bjw){
  # for each row, look for runs of 3, 4, 5, etc.
  # update the counts of individual color and score combos separately (e.g. red triples, red quadruples, etc. then move on to green)
  # after recording the score for each, set those values to NA
  bjw;
}

# TODO:
refill.bjw <- function(bjw){
  # for each column identify the maximum series of NAs that form an uninterrupted path to the top edge
  # randomly fill with colors
  bjw;
}

# TODO:
innerUpdate.bjw <- function(bjw){
  converged <- F;
  while(!converged){
    previousstate <- bjw$data;
    compact.bjw(bjw);
    match.bjw(bjw);
    converged <- identical(bjw$data,previousstate);
  }
};

outerUpdate.bjw <- function(bjw){
  converged <- F;
  while(!converged){
    refill.bjw(bjw);
    innerUpdate.bjw(bjw);
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