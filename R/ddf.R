ddf <-
function(new, old = NULL,
         by = NULL,
         ignore.case = FALSE,
         ignore.ws = FALSE,
         ignore.headers = FALSE,
         ignore = NULL,
         ignore.rows = NULL,
         ignore.columns = NULL,
         only.columns = NULL,
         ... ) {


    if (!ignore.headers &&
        length(colnames(old)) == length(colnames(new)) &&
        all(  sort(colnames(old)) ==   sort(colnames(new)))) {

        if (!all(colnames(old) == colnames(new))) {
            if (verbose)
                message("orders of columns differ")
            old <- old[, colnames(new)]
        }
    }


    key.new <- if (identical(by, 0))
                   row.names(new)
               else if (!is.null(by))
                   do.call(paste, new[, by, drop = FALSE])
               else
                   do.call(paste, new)

    key.old <- if (identical(by, 0))
                   row.names(old)
               else if (!is.null(by))
                   do.call(paste, old[, by, drop = FALSE])
               else
                   do.call(paste, old)



    ## check for changes

    m <- match(key.new, key.old, nomatch = 0L)

    new. <- new[m > 0, ]
    old. <- old[m, ]
    key.new. <- key.new[m > 0]


    ch.cols <- only.columns

    if (is.null(only.columns))
        ch.cols <- setdiff(colnames(new.), ignore.columns)
    digest.new <- apply(new.[, ch.cols, drop = FALSE],
                        1,
                        function(x) paste(x, collapse = "--"))

    if (is.null(only.columns))
        ch.cols <- setdiff(colnames(old.), ignore.columns)
    digest.old <- apply(old.[, ch.cols, drop = FALSE],
                        1,
                        function(x) paste(x, collapse = "--"))

    changes <- which(digest.new != digest.old)

    ans.changes <- list()
    for (ch in changes) {
        same <- (is.na(new.[ch, ch.cols])  &  is.na(old.[ch, ch.cols])) |
                 new.[ch, ch.cols] == old.[ch, ch.cols]
        ch.col <- setdiff(ch.cols[!same], ignore.columns)
        o.n <- cbind(old = t(old.[ch, ch.col]),
                     new = t(new.[ch, ch.col]))
        row.names(o.n) <- ch.col
        colnames (o.n) <- c("old", "new")
        ans.changes[[as.character(key.new.[ch])]] <- o.n
    }

    ans <- list(
        added   = new[!key.new %in% key.old, ],
        removed = old[!key.old %in% key.new, ],
        changed = ans.changes)

    attr(ans, "new.key") <- key.new
    attr(ans, "old.key") <- key.old

    attr(ans, "new.changed.rows") <- seq_len(nrow(new))[m > 0][changes]
    attr(ans, "old.changed.rows") <- seq_len(nrow(old))[m    ][changes]

    ans
}
