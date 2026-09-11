# okay-r shim, version 2 (specs/r.md). One JSON object per line each
# way; functions are ADDRESSED as pkg::name (or a base name) and
# looked up, never eval'd from source. A failing call answers a
# condition and the process survives; only a broken wire ends it.
#
# jsonlite is a NAMED prerequisite, refused at the handshake: base R
# has no JSON reader, and our own parser at the trust boundary is a
# worse thing to own than one package every R installation has.

SHIM <- 2

say <- function(x) {
  cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA), "\n", sep = "")
  flush(stdout())
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat('{"shim":', SHIM, ',"fatal":"okay-r needs the jsonlite package and it is not installed - ',
      'install.packages(\\"jsonlite\\"), or your distribution\\u0027s r-cran-jsonlite"}\n', sep = "")
  flush(stdout())
  quit(status = 1)
}

# ---- the wire ---------------------------------------------------------

# R -> wire. NULL is null; everything JSON cannot say is TAGGED.
enc <- function(v) {
  if (is.null(v)) return(NULL)
  if (is.raw(v)) return(list(t = "raw", b64 = jsonlite::base64_enc(v)))
  if (is.data.frame(v) || (is.list(v) && !is.null(names(v)) && all(names(v) != ""))) {
    cols <- lapply(names(v), function(n) enc_column(n, v[[n]]))
    return(list(t = "frame", v = 2L, cols = cols))
  }
  if (is.list(v)) return(lapply(v, enc))
  enc_col(v)
}

# A COLUMN, columnar (r-frame-columnar-wire): one type for the whole
# column, a PLAIN array of values, and the absences as index lists.
# jsonlite serialises an atomic vector on its fast C path; the
# per-cell form below made a 100k-row frame hundreds of thousands of
# little objects, which is where the 13.7 s went. The placeholder at
# an absent position is the type's zero and not null on purpose: a
# null would force a list and undo the whole point. A column the four
# atomic types cannot carry keeps the per-cell form under `cells`.
enc_column <- function(name, v) {
  short <- if (is.logical(v)) "l"
    else if (is.integer(v)) "i"
    else if (is.double(v)) "d"
    else if (is.character(v)) "s"
    else NULL
  if (is.null(short) || is.list(v))
    return(list(name = name, cells = enc_col(v)))
  nan_ix <- if (is.double(v)) which(is.nan(v)) else integer(0)
  na_ix <- setdiff(which(is.na(v)), nan_ix)
  vals <- v
  zero <- switch(short, l = FALSE, i = 0L, d = 0, s = "")
  if (length(na_ix) > 0 || length(nan_ix) > 0) vals[c(na_ix, nan_ix)] <- zero
  out <- list(name = name, type = short,
              values = unname(vals),
              na = as.integer(na_ix - 1L))
  if (short == "d") out$nan <- as.integer(nan_ix - 1L)
  out
}

# an atomic vector -> a list of encoded scalars, one per element,
# so an NA keeps the vector's own type on the way back (the per-cell
# form, kept for columns the four atomic types cannot carry)
enc_col <- function(v) {
  ty <- if (is.logical(v)) "logical"
    else if (is.integer(v)) "integer"
    else if (is.double(v)) "double"
    else if (is.character(v)) "character"
    else "character"
  lapply(seq_along(v), function(i) {
    x <- v[[i]]
    if (is.na(x) && !(is.double(x) && is.nan(x))) return(list(t = "na", of = ty))
    if (is.integer(x)) return(list(t = "i", v = unname(x)))
    if (is.double(x) && is.nan(x)) return(list(t = "nan"))
    unname(x)
  })
}

# one column off the wire: the columnar shape (a type, a plain array
# and the absence indices) or the per-cell `cells`, or v1's [name,
# cells] pair — a reader accepts all three, an encoder writes one
dec_column <- function(c) {
  if (!is.null(c$values)) {
    xs <- unlist(c$values, use.names = FALSE)
    if (length(xs) == 0)
      xs <- switch(c$type, l = logical(0), i = integer(0), d = double(0), s = character(0))
    xs <- switch(c$type, l = as.logical(xs), i = as.integer(xs),
                 d = as.double(xs), s = as.character(xs))
    if (!is.null(c$na) && length(c$na) > 0) xs[as.integer(c$na) + 1L] <- NA
    if (!is.null(c$nan) && length(c$nan) > 0) xs[as.integer(c$nan) + 1L] <- NaN
    return(xs)
  }
  cells <- if (!is.null(c$cells)) c$cells else c[[2]]
  simplify_col(lapply(cells, dec))
}

# wire -> R. A tagged object becomes the value it names; an array
# becomes a LIST, and a caller wanting a vector says so with unlist().
dec <- function(v) {
  if (is.null(v)) return(NULL)
  if (is.list(v) && !is.null(v$t)) {
    t <- v$t
    if (t == "na") return(switch(v$of,
      logical = NA, integer = NA_integer_, double = NA_real_, character = NA_character_, NA))
    if (t == "nan") return(NaN)
    if (t == "i") return(as.integer(v$v))
    if (t == "raw") return(jsonlite::base64_dec(v$b64))
    if (t == "frame") {
      fv <- if (is.null(v$v)) 1L else as.integer(v$v)
      if (fv > 2L) stop(sprintf("frame format v%d: this shim reads up to v2", fv))
      cols <- lapply(v$cols, dec_column)
      names(cols) <- vapply(v$cols, function(c)
        if (!is.null(c$name)) c$name else c[[1]], character(1))
      return(as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE))
    }
    stop(sprintf("unknown tagged value: %s", t))
  }
  if (is.list(v)) {
    # a wire array becomes an ATOMIC VECTOR when every element is a
    # scalar, and a list otherwise: sum(c(1, 2, NA)) is the call an
    # analyst writes, and sum(list(1, 2, NA)) is an error about types
    xs <- lapply(v, dec)
    if (length(xs) > 0 &&
        all(vapply(xs, function(x) !is.null(x) && is.atomic(x) && length(x) == 1L, logical(1))))
      return(simplify_col(xs))
    return(xs)
  }
  # a PLAIN number on this wire is always a double: an R integer is
  # tagged "i" on the way in, and jsonlite would otherwise read 3 as
  # an integer and hand R a different type than the caller sent
  if (is.numeric(v)) return(as.numeric(v))
  v
}

# a list of scalars back to an atomic vector, keeping the NA's type
simplify_col <- function(xs) {
  if (length(xs) == 0) return(logical(0))
  do.call(c, xs)
}

resolve <- function(fn) {
  parts <- strsplit(fn, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) {
    if (!requireNamespace(parts[1], quietly = TRUE))
      stop(sprintf("package '%s' is not installed", parts[1]))
    return(getExportedValue(parts[1], parts[2]))
  }
  if (length(parts) != 1L) stop(sprintf("a function is addressed as pkg::name, got '%s'", fn))
  f <- get0(fn, mode = "function")
  if (is.null(f)) stop(sprintf("no function named '%s'", fn))
  f
}

say(list(shim = SHIM, r = paste(R.version$major, R.version$minor, sep = ".")))

# ---- the loop ---------------------------------------------------------

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1L, warn = FALSE)) > 0) {
  if (!nzchar(trimws(line))) next
  req <- jsonlite::fromJSON(line, simplifyVector = FALSE)
  rid <- req$id
  out <- tryCatch({
    op <- req$op
    if (op == "call") {
      f <- resolve(req$fn)
      list(id = rid, ok = enc(do.call(f, lapply(req$args, dec))))
    } else if (op == "frame") {
      f <- resolve(req$fn)
      res <- do.call(f, c(list(dec(req$`in`)), lapply(req$args, dec)))
      if (!is.data.frame(res) && !is.list(res))
        stop(sprintf("a frame function must answer a data.frame, got %s", class(res)[1]))
      list(id = rid, ok = enc(as.data.frame(res, stringsAsFactors = FALSE)))
    } else if (op == "verify") {
      pkgs <- list()
      for (name in req$packages) {
        pkgs[[name]] <- if (requireNamespace(name, quietly = TRUE))
          as.character(utils::packageVersion(name)) else NULL
      }
      list(id = rid, ok = list(r = paste(R.version$major, R.version$minor, sep = "."),
                               packages = pkgs))
    } else stop(sprintf("unknown op '%s'", op))
  }, condition = function(c) {
    list(id = rid, condition = list(kind = class(c)[1], message = conditionMessage(c)))
  })
  say(out)
}
