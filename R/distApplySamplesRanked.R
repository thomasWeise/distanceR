dist.apply.samples.ranked <- function(X, FUN=distance.euclidean,
                                      sampler=identity,
                                      ranker=rank,
                                      aggregate=mean,
                                      FUN.VALUE=+Inf) {
  n <- length(X);
  stopifnot(n > 1L);

  # pre-allocate the sizes and samples structures
  sizes   <- vector(mode="integer", length=n);
  samples <- vector(mode="list", length=n);

  # cache all the samples
  totalSize <- 0L;
  for(i in seq_len(n)) {
    i <- force(i);
    samp <- lapply(X=sampler(X[[i]]),
                   FUN=function(x) { attr(x, "i") <- i; x });
    samples[[i]] <- samp;
    s <- length(samp);
    sizes[[i]] <- s;
    totalSize <- (totalSize + s);
  }
  # flatten the list of samples
  samples <- unlist(samp, recursive = FALSE, use.names = FALSE);
  stopifnot(identical(length(samples), totalSize));

  # create the distances storage: for a pair i, j, we need sample_count(i) + sample_count(j) entries
  results.sizes <- dist.apply.samples(X=sizes, FUN=`+`);
  results <- lapply(X=results.sizes,
                    FUN=function(i) vector(mode="numeric", length=i));

  # now we compute each and every distance
  computed <- NULL;
  for(i in seq_len(totalSize)) {
    # allocate the vector if necessary
    n.cur <- (totalSize-sizes[[i]]);
    if(!identical(n.cur, length(computed))) {
      computed <- vector(mode="numeric", length=n.cur);
    }

    # get the sample
    a   <- samples[[i]];
    a.i <- attr(a, "i");

    # compute all distances
    index <- 0L;
    for(j in seq_len(totalSize)) {
      b <- samples[[j]];
      b.i <- attr(b, "i");
      if(!(identical(a.i, b.i))) {
        index <- index + 1L;
        computed[[index]] <- FUN(a.i, b.i);
      }
    }
    stopifnot(identical(index, n.cur));

    # rank the distances
    computed <- ranker(computed);

    # store back the results
    index <- 0L;
    for(j in seq_len(totalSize)) {
      b.i <- attr(samples[[j]], "i");
      if(!(identical(a.i, b.i))) {
        index <- index + 1L;
        didx <- dist.index(a.i, b.i, n);
        s <- results.sizes[[didx]];
        results.sizes[[didx]] <- (s - 1L);
        results[[didx]][s] <- computed[[index]];
      }
    }
  }
  stopifnot(all(results.sizes == 0L));

  # collapse the results into a distance matrix data list
  return(vapply(X=results, FUN=aggregate, FUN.VALUE=FUN.VALUE));
}
