# okay-r shim, version 4 (specs/r.md; v3 = foreign-typed-calls: a
# named list that is not a data.frame is a RECORD on the wire; v4 =
# foreign-callbacks: `start`/`resume` and `okay_call`; v5 =
# foreign-object-handles: `hold`/`release`, refs as values; v6 =
# foreign-module-trait: `okay_describe`; v7 = remote-foreign: programs as
# data, `program`/`continue`/`forget`, continuations kept by id; v8 =
# r-arrow: a frame request/answer may cross as ONE Arrow IPC stream when
# the `arrow` package is installed, okay-py's twin; v9 = foreign-one-value:
# the SHARED value tags every far side speaks — an integer is a plain
# number, an integral double {"t":"f"}, a named list {"t":"dict"}, raw
# {"t":"bytes"}, an integer past 2^53 {"t":"int"} — and frames announced
# columnar; the old tags are still read; v10 = foreign-one-program:
# `start`/`resume` fold into `program`/`continue`, a direct function's
# okay_call a node marked `once`). One JSON object per
# line each way; functions are ADDRESSED as pkg::name (or a base name) and
# looked up, never eval'd from source. A failing call answers a
# condition and the process survives; only a broken wire ends it.
#
# jsonlite is a NAMED prerequisite, refused at the handshake: base R
# has no JSON reader, and our own parser at the trust boundary is a
# worse thing to own than one package every R installation has. `arrow`
# is OPTIONAL: announced when installed, and frames work exactly as
# before where it is not.

SHIM <- 10

say <- function(x) {
  cat(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = I(17)), "\n", sep = "")
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
  if (is.raw(v)) return(list(t = "bytes", b64 = jsonlite::base64_enc(v)))
  if (is.data.frame(v)) {
    cols <- lapply(names(v), function(n) enc_column(n, v[[n]]))
    return(list(t = "frame", v = 2L, cols = cols))
  }
  # a named list that is not a data.frame is a record: its elements, in
  # order (v3). The frame op still answers a frame: it coerces first.
  if (is.list(v) && !is.null(names(v)) && all(names(v) != "")) {
    kv <- lapply(names(v), function(n) list(n, enc(v[[n]])))
    return(list(t = "dict", kv = kv))
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
  # a raw vector's cells keep their tag: `v[[i]]` of a raw is a raw(1),
  # which jsonlite would write as a bare base64 string, and the host
  # would read a character column back (found by r-arrow-verify, 2026-09-25)
  if (is.raw(v))
    return(lapply(seq_along(v), function(i) list(t = "bytes", b64 = jsonlite::base64_enc(v[i]))))
  ty <- if (is.logical(v)) "logical"
    else if (is.integer(v)) "integer"
    else if (is.double(v)) "double"
    else if (is.character(v)) "character"
    else "character"
  lapply(seq_along(v), function(i) {
    x <- v[[i]]
    if (is.na(x) && !(is.double(x) && is.nan(x))) return(list(t = "na", of = ty))
    # the shared tags: an integer is a plain number, and a double that
    # LOOKS integral is tagged, so the two stay two on a JSON wire
    if (is.integer(x)) return(unname(x))
    if (is.double(x) && is.nan(x)) return(list(t = "nan"))
    if (is.double(x) && is.finite(x) && x == floor(x) && abs(x) < 1e15) return(list(t = "f", v = unname(x)))
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
    if (t == "f") return(as.double(v$v))
    # an integer past 2^53: R has no 64-bit integer, and a Long in R's
    # vocabulary is its digits there (RCodec.long)
    if (t == "int") return(as.character(v$v))
    if (t == "i") return(as.integer(v$v))   # shim v8's tag, still read
    if (t == "raw" || t == "bytes") return(jsonlite::base64_dec(v$b64))
    if (t == "ref") return(okay_held(v$id))
    if (t == "named" || t == "dict") {
      out <- lapply(v$kv, function(p) dec(p[[2]]))
      names(out) <- vapply(v$kv, function(p) p[[1]], character(1))
      return(out)
    }
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
  # a PLAIN integral number is an integer (the shared tags: an integral
  # DOUBLE arrives tagged "f"), within R's 32 bits; anything else a double
  if (is.numeric(v)) {
    if (is.integer(v)) return(v)
    if (is.finite(v) && v == floor(v) && abs(v) <= .Machine$integer.max) return(as.integer(v))
    return(as.numeric(v))
  }
  v
}

# a list of scalars back to an atomic vector, keeping the NA's type
simplify_col <- function(xs) {
  if (length(xs) == 0) return(logical(0))
  do.call(c, xs)
}

resolve <- function(fn) {
  parts <- strsplit(fn, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L && exists(parts[1], envir = .okay_modules, inherits = FALSE)) {
    f <- get0(parts[2], envir = get(parts[1], envir = .okay_modules), inherits = FALSE, mode = "function")
    if (is.null(f)) stop(sprintf("module '%s' has no function '%s'", parts[1], parts[2]))
    return(f)
  }
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

# ---- callbacks into okay (v4) ------------------------------------------
#
# okay_call("name", ...) inside a function okay started with callbacks: the
# ask goes to the host and THIS frame waits for the resume, serving any
# request that arrives meanwhile (a callback may call R again). A callback
# that failed in okay is an R condition of class `okay_error`, with the
# okay condition's `kind` beside its message — tryCatch-able.

.okay_direct <- list()
.okay_next_k <- 0L

okay_call <- function(name, ...) {
  n <- length(.okay_direct)
  if (n == 0L)
    stop(sprintf("okay_call('%s') outside a program okay started", name))
  frame <- .okay_direct[[n]]
  if (!(name %in% frame$offered))
    stop(sprintf("okay_call('%s'): this call was offered %s", name, paste(frame$offered, collapse = ", ")))
  k <- .okay_next_k + 1L
  assign(".okay_next_k", k, envir = globalenv())
  # the one callback message (foreign-one-program): a node, continued ONCE
  say(list(id = frame$id, ok = list(perform = name, args = unname(lapply(list(...), enc)), k = k, once = TRUE)))
  repeat {
    req <- read_msg()
    if (is.null(req)) quit(status = 0)
    if (identical(req$op, "continue") && identical(as.numeric(req$run), as.numeric(frame$run)) &&
        identical(as.integer(req$k), k)) {
      # what this program answers next answers THIS continue
      stack <- .okay_direct
      stack[[n]]$id <- req$id
      assign(".okay_direct", stack, envir = globalenv())
      if (!is.null(req$condition))
        stop(structure(class = c("okay_error", "error", "condition"),
                       list(message = req$condition$message, call = NULL,
                            kind = req$condition$kind)))
      return(dec(req$answer))
    }
    say(serve(req))
  }
}

# ---- held objects (v5) --------------------------------------------------

.okay_objects <- new.env()
.okay_next_ref <- 0L

okay_hold <- function(obj) {
  i <- .okay_next_ref + 1L
  assign(".okay_next_ref", i, envir = globalenv())
  assign(as.character(i), obj, envir = .okay_objects)
  list(t = "ref", id = i, type = class(obj)[1])
}

okay_held <- function(i) {
  key <- as.character(i)
  if (!exists(key, envir = .okay_objects, inherits = FALSE))
    stop(sprintf("ref %s is not held by this process (released, or held by a process that is gone)", key))
  get(key, envir = .okay_objects, inherits = FALSE)
}

# ---- describing a module or a package (v6) ------------------------------
# what okay's RFacade writes a Scala object from: each function's name and
# its formals. R has no annotations, so only the SHAPE is described.

okay_describe <- function(target) {
  env <- if (exists(target, envir = .okay_modules, inherits = FALSE)) get(target, envir = .okay_modules)
    else if (requireNamespace(target, quietly = TRUE)) asNamespace(target)
    else stop(sprintf("no module or package named '%s'", target))
  fns <- if (isNamespace(env)) getNamespaceExports(target) else ls(env)
  fns <- sort(fns[!startsWith(fns, ".")], method = "radix")
  out <- list()
  for (n in fns) {
    f <- get0(n, envir = env, inherits = FALSE)
    if (!is.function(f)) next
    fs <- formals(f)
    # as.character of the formals: "" where there is no default, which
    # never evaluates the missing-argument symbol itself
    has <- nzchar(as.character(fs))
    params <- list()
    for (i in seq_along(fs)) {
      p <- names(fs)[i]
      if (p != "...") params[[length(params) + 1L]] <- list(name = p, default = has[i])
    }
    out[[length(out) + 1L]] <- list(name = n, params = params)
  }
  out
}

# ---- programs as data (v7, remote-foreign) -------------------------------
# okay_done(v), okay_perform(name, ...) and okay_then(p, f) build a program
# whose continuations are R closures. The shim hands okay one node at a
# time and keeps each continuation under an id, so okay may continue the
# same one more than once (a Choice handler does).

okay_done <- function(value) structure(list(value = value), class = "okay_done")
okay_perform <- function(name, ...)
  structure(list(name = name, args = list(...), k = okay_done), class = "okay_step")
okay_then <- function(p, f) {
  if (inherits(p, "okay_done")) return(f(p$value))
  k <- p$k
  p$k <- function(x) okay_then(k(x), f)
  p
}

.okay_runs <- new.env()
.okay_next_kont <- 0L

okay_node <- function(run, p) {
  if (inherits(p, "okay_done")) return(list(done = enc(p$value)))
  if (!inherits(p, "okay_step"))
    stop(sprintf("a program answers okay_done(v) or okay_perform(name, ...), got %s", class(p)[1]))
  k <- .okay_next_kont + 1L
  assign(".okay_next_kont", k, envir = globalenv())
  key <- as.character(run)
  table <- if (exists(key, envir = .okay_runs, inherits = FALSE)) get(key, envir = .okay_runs) else list()
  table[[as.character(k)]] <- p$k
  assign(key, table, envir = .okay_runs)
  list(perform = p$name, args = unname(lapply(p$args, enc)), k = k)
}

# the direct-style programs running now, innermost last: their run, the
# callbacks offered, and the id of the request each answers next
okay_push <- function(run, cbs, id)
  assign(".okay_direct", c(.okay_direct, list(list(run = run, offered = unlist(cbs), id = id))), envir = globalenv())
okay_pop <- function()
  assign(".okay_direct", .okay_direct[-length(.okay_direct)], envir = globalenv())

serve <- function(req) {
  rid <- req$id
  tryCatch({
    op <- req$op
    if (op == "call") {
      f <- resolve(req$fn)
      list(id = rid, ok = enc(do.call(f, lapply(req$args, dec))))
    } else if (op == "program") {
      # ONE program protocol (foreign-one-program): the function RETURNS a
      # program as data (okay_done / okay_then(okay_perform(...), f)), or is
      # ordinary R whose okay_call's are nodes marked `once`
      f <- resolve(req$fn)
      okay_push(req$run, req$callbacks, rid)
      n <- length(.okay_direct)
      out <- tryCatch(list(value = do.call(f, lapply(req$args, dec))), error = function(e) list(err = e))
      id <- .okay_direct[[n]]$id
      okay_pop()
      if (!is.null(out$err))
        list(id = id, condition = list(kind = class(out$err)[1], message = conditionMessage(out$err)))
      else if (inherits(out$value, "okay_done") || inherits(out$value, "okay_step"))
        list(id = id, ok = okay_node(req$run, out$value))
      else list(id = id, ok = list(done = enc(out$value)))
    } else if (op == "continue") {
      key <- as.character(req$run)
      table <- if (exists(key, envir = .okay_runs, inherits = FALSE)) get(key, envir = .okay_runs) else list()
      k <- table[[as.character(req$k)]]
      if (is.null(k))
        stop(sprintf("continuation %s of run %s is not held here (forgotten, continued once already, or another process)", req$k, req$run))
      list(id = rid, ok = okay_node(req$run, k(dec(req$answer))))
    } else if (op == "forget") {
      key <- as.character(req$run)
      if (exists(key, envir = .okay_runs, inherits = FALSE)) rm(list = key, envir = .okay_runs)
      list(id = rid, ok = NULL)
    } else if (op == "hold") {
      f <- resolve(req$fn)
      list(id = rid, ok = okay_hold(do.call(f, lapply(req$args, dec))))
    } else if (op == "release") {
      key <- as.character(req$ref)
      if (exists(key, envir = .okay_objects, inherits = FALSE)) rm(list = key, envir = .okay_objects)
      list(id = rid, ok = NULL)
    } else if (op == "frame") {
      f <- resolve(req$fn)
      # an Arrow-carried request already IS a data.frame (r-arrow):
      # `dec` is for the wire's tagged JSON/CBOR frame form only
      input <- if (isTRUE(req$.okay_arrow_in)) req$`in` else dec(req$`in`)
      res <- do.call(f, c(list(input), lapply(req$args, dec)))
      if (!is.data.frame(res) && !is.list(res))
        stop(sprintf("a frame function must answer a data.frame, got %s", class(res)[1]))
      if (isTRUE(req$.okay_arrow_in)) okay_arrow_reply(rid, res) else
        list(id = rid, ok = enc(as.data.frame(res, stringsAsFactors = FALSE)))
    } else if (op == "verify") {
      pkgs <- list()
      for (name in req$packages) {
        pkgs[[name]] <- if (requireNamespace(name, quietly = TRUE))
          as.character(utils::packageVersion(name)) else NULL
      }
      list(id = rid, ok = list(r = paste(R.version$major, R.version$minor, sep = "."),
                               packages = pkgs))
    } else if (op == "configure") {
      f <- req$format; z <- req$compress
      fr <- if (is.null(req$frames)) "json" else req$frames
      if (!(identical(f, "json") || identical(f, "cbor")))
        stop(sprintf("this R shim speaks the formats json, cbor; not '%s'", format(f)))
      if (!(identical(z, "none") || identical(z, "zlib")))
        stop(sprintf("this R shim speaks the compressions none, zlib; not '%s'", format(z)))
      if (!(identical(fr, "json") || (identical(fr, "arrow") && .okay_has_arrow)))
        stop(sprintf("this R shim speaks the frames %s; not '%s'",
                     paste(c("json", if (.okay_has_arrow) "arrow"), collapse = ", "), format(fr)))
      # takes effect AFTER this answer is written (see say)
      .okay_wire$switch_to <- list(format = f, compress = z, frames = fr)
      list(id = rid, ok = list(format = f, compress = z))
    } else stop(sprintf("unknown op '%s'", op))
  }, condition = function(c) {
    list(id = rid, condition = list(kind = class(c)[1], message = conditionMessage(c)))
  })
}

# ---- inline modules (foreign-inline-modules) -----------------------------
# each `name=path` in OKAY_R_MODULES is sourced into its OWN environment,
# where `name::fn` finds it; a module that does not load refuses at the
# handshake, naming itself, rather than failing a call later

.okay_modules <- new.env()
local({
  mods <- Sys.getenv("OKAY_R_MODULES", "")
  if (nzchar(mods)) for (entry in strsplit(mods, ";", fixed = TRUE)[[1]]) {
    at <- regexpr("=", entry, fixed = TRUE)
    name <- substr(entry, 1L, at - 1L)
    path <- substr(entry, at + 1L, nchar(entry))
    e <- new.env(parent = globalenv())
    tryCatch(sys.source(path, envir = e), error = function(err) {
      say(list(shim = SHIM, fatal = sprintf("okay-r: module '%s' did not load: %s",
                                            name, conditionMessage(err))))
      quit(status = 1)
    })
    assign(name, e, envir = .okay_modules)
  }
})

# ---- the wire's encoding (polyglot-one-wire, wire-givens-r) --------------
# JSON lines until the host configures otherwise; then FRAMES (a 4-byte
# big-endian length, then the message), the same tree as JSON or as CBOR,
# optionally zlib-compressed (RFC 1950: memCompress's "gzip" IS zlib, and
# memDecompress checks its adler32 - raw DEFLATE has no safe road in base
# R). Binary both ways: one "rb" stdin, read by lines and then by frames,
# and /dev/stdout opened raw.

.okay_wire <- new.env()
.okay_wire$format <- "json"
.okay_wire$compress <- "none"
.okay_wire$frames <- "json"
.okay_wire$switch_to <- NULL

# arrow is OPTIONAL: checked once, announced when present, never imported
# otherwise (r-arrow, okay-py's `_HAS_ARROW` twin)
.okay_has_arrow <- requireNamespace("arrow", quietly = TRUE)

okay_framed <- function() .okay_wire$format != "json" || .okay_wire$compress != "none" || .okay_wire$frames == "arrow"

cbor_head <- function(major, n) {
  m <- major * 32
  if (n < 24) as.raw(m + n)
  else if (n < 256) as.raw(c(m + 24, n))
  else if (n < 65536) as.raw(c(m + 25, n %/% 256, n %% 256))
  else if (n < 4294967296) as.raw(c(m + 26, (n %/% 16777216) %% 256, (n %/% 65536) %% 256, (n %/% 256) %% 256, n %% 256))
  else as.raw(c(m + 27, (n %/% 256^(7:0)) %% 256))
}

# CBOR of the tree jsonlite parses back (scalars, lists, named lists, NULL):
# encoding THAT tree, rather than R's own objects, keeps every rule of the
# JSON road (auto_unbox, NA as "NA", a frame's columns) - the two formats
# carry the same values by construction
cbor_enc <- function(x) {
  if (is.null(x)) return(as.raw(0xf6))
  if (is.list(x)) {
    n <- length(x)
    nm <- names(x)
    if (!is.null(nm)) {
      parts <- vector("list", 2L * n)
      for (i in seq_len(n)) {
        parts[[2L * i - 1L]] <- cbor_enc(nm[[i]])
        parts[[2L * i]] <- cbor_enc(x[[i]])
      }
      return(c(cbor_head(5, n), unlist(parts, use.names = FALSE)))
    }
    # an array of numbers - a frame's column - in one writeBin
    if (n > 0L && all(vapply(x, function(v) is.numeric(v) && length(v) == 1L, logical(1)))) {
      bits <- matrix(writeBin(as.double(unlist(x, use.names = FALSE)), raw(), size = 8, endian = "big"), nrow = 8L)
      return(c(cbor_head(4, n), as.vector(rbind(as.raw(0xfb), bits))))
    }
    return(c(cbor_head(4, n), unlist(lapply(x, cbor_enc), use.names = FALSE)))
  }
  if (is.logical(x)) return(as.raw(if (isTRUE(x)) 0xf5 else 0xf4))
  if (is.character(x)) {
    b <- charToRaw(enc2utf8(x))
    return(c(cbor_head(3, length(b)), b))
  }
  if (is.integer(x)) return(if (x >= 0L) cbor_head(0, as.double(x)) else cbor_head(1, -1 - as.double(x)))
  if (is.double(x)) return(c(as.raw(0xfb), writeBin(x, raw(), size = 8, endian = "big")))
  stop(sprintf("a %s does not encode as CBOR", class(x)[1]))
}

# CBOR -> the tree jsonlite::fromJSON(simplifyVector = FALSE) would give
cbor_dec <- function(b) {
  pos <- 1L
  take <- function(n) {
    if (pos + n - 1L > length(b)) stop("a CBOR message ended early (cut short?)")
    out <- b[seq.int(pos, length.out = n)]
    pos <<- pos + n
    out
  }
  uint <- function(bytes) sum(as.numeric(bytes) * 256^((length(bytes) - 1L):0))
  arg <- function(info) {
    if (info < 24L) info
    else if (info == 24L) uint(take(1L))
    else if (info == 25L) uint(take(2L))
    else if (info == 26L) uint(take(4L))
    else if (info == 27L) uint(take(8L))
    else stop(sprintf("CBOR: an indefinite or reserved length (%d) is not in the wire's subset", info))
  }
  num <- function(v) if (abs(v) <= .Machine$integer.max) as.integer(v) else v
  item <- function() {
    ib <- as.integer(take(1L))
    major <- ib %/% 32L
    info <- ib %% 32L
    if (major == 0L) return(num(arg(info)))
    if (major == 1L) return(num(-1 - arg(info)))
    if (major == 3L) {
      n <- arg(info)
      if (n == 0) return("")
      s <- rawToChar(take(n))
      Encoding(s) <- "UTF-8"
      return(s)
    }
    if (major == 4L) {
      n <- arg(info)
      out <- vector("list", n)
      for (i in seq_len(n)) { v <- item(); if (!is.null(v)) out[[i]] <- v }
      return(out)
    }
    if (major == 5L) {
      n <- arg(info)
      out <- vector("list", n)
      keys <- character(n)
      for (i in seq_len(n)) {
        k <- item()
        if (!is.character(k)) stop("CBOR: a map key that is not text")
        keys[[i]] <- k
        v <- item()
        if (!is.null(v)) out[[i]] <- v
      }
      names(out) <- keys
      return(out)
    }
    if (major == 7L) {
      if (info == 20L) return(FALSE)
      if (info == 21L) return(TRUE)
      if (info == 22L || info == 23L) return(NULL)
      if (info == 25L) {
        h <- uint(take(2L))
        e <- (h %/% 1024) %% 32
        m <- h %% 1024
        v <- if (e == 0) m * 2^-24 else if (e == 31) (if (m == 0) Inf else NaN) else (m + 1024) * 2^(e - 25)
        return(if (h >= 32768) -v else v)
      }
      if (info == 26L) return(readBin(take(4L), "numeric", size = 4, endian = "big"))
      if (info == 27L) return(readBin(take(8L), "numeric", size = 8, endian = "big"))
      stop(sprintf("CBOR: simple value %d is not in the wire's subset", info))
    }
    stop(sprintf("CBOR: major type %d (byte strings, tags) is not in the wire's subset", major))
  }
  v <- item()
  if (pos <= length(b)) stop(sprintf("CBOR: %d bytes after the message", length(b) - pos + 1L))
  v
}

# digits = I(17), not NA: jsonlite's "max precision" is 15 significant
# digits, which rounded every double on its way to okay (sqrt(2) arrived as
# 1.4142135623731); 17 is what a double needs to come back as itself
# (wire-givens-r, found by the same-answers-on-every-wire suite)
okay_encode <- function(x) {
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = I(17))
  body <- if (.okay_wire$format == "cbor") cbor_enc(jsonlite::fromJSON(txt, simplifyVector = FALSE))
          else charToRaw(enc2utf8(as.character(txt)))
  if (.okay_wire$compress == "zlib") memCompress(body, "gzip") else body
}

okay_decode <- function(body) {
  if (.okay_wire$compress == "zlib") body <- memDecompress(body, "gzip")
  if (length(body) >= 4L && identical(body[1:4], as.raw(c(0xff, 0xff, 0xff, 0xff))))
    return(okay_arrow_request(body))
  if (.okay_wire$format == "cbor") cbor_dec(body)
  else { s <- rawToChar(body); Encoding(s) <- "UTF-8"; jsonlite::fromJSON(s, simplifyVector = FALSE) }
}

# ---- frames as Arrow (r-arrow, okay-py's `_arrow_request`/`reply_frame`
# twin) --------------------------------------------------------------------
#
# A frame request may arrive as ONE Arrow IPC stream: the frame is the
# table, and the request's header (id, op, fn, args) is the schema's
# metadata under "okay". Its answer goes back the same way where the
# answer's columns fit. `arrow` is imported only when a frame needs it.
#
# VERIFIED 2026-09-25 (r-arrow-verify) against R 4.6.1 + arrow 25.0.0 in a
# container (`TestRArrow`, 6/6): `t$schema$metadata` reads the request's
# header off a Table `read_ipc_stream` returned, `tab$metadata <-` sets it
# before a write, and `BufferOutputStream$create()`/`$finish()`/`as.raw()`
# round-trip the stream in memory — all three as the package documents.

okay_arrow_request <- function(body) {
  t <- arrow::read_ipc_stream(body, as_data_frame = FALSE)
  meta <- t$schema$metadata
  if (is.null(meta) || is.null(meta[["okay"]]))
    stop("an Arrow message without its okay header")
  req <- jsonlite::fromJSON(meta[["okay"]], simplifyVector = FALSE)
  req$`in` <- as.data.frame(t)
  req$.okay_arrow_in <- TRUE
  req
}

# the answer as Arrow, tagged `okay_arrow_bytes` for `say` to write
# untouched by JSON/CBOR; falls back to the ordinary encoded frame answer
# when the R side cannot make an Arrow table of it (rare: a data.frame's
# columns are almost always one of Arrow's types, unlike a Python dict's)
okay_arrow_reply <- function(rid, res) {
  bytes <- tryCatch({
    tab <- arrow::arrow_table(as.data.frame(res, stringsAsFactors = FALSE))
    tab$metadata <- list(okay = jsonlite::toJSON(list(id = rid, ok = list(t = "arrow")), auto_unbox = TRUE))
    sink <- arrow::BufferOutputStream$create()
    arrow::write_ipc_stream(tab, sink)
    as.raw(sink$finish())
  }, error = function(e) NULL)
  if (is.null(bytes)) list(id = rid, ok = enc(as.data.frame(res, stringsAsFactors = FALSE)))
  else structure(list(bytes = bytes), class = "okay_arrow_bytes")
}

.okay_out <- file("/dev/stdout", open = "wb", raw = TRUE)

say <- function(x) {
  if (inherits(x, "okay_arrow_bytes")) {
    body <- if (.okay_wire$compress == "zlib") memCompress(x$bytes, "gzip") else x$bytes
    n <- length(body)
    writeBin(c(as.raw((n %/% 256^(3:0)) %% 256), body), .okay_out)
    flush(.okay_out)
    return(invisible(NULL))
  }
  if (okay_framed()) {
    body <- okay_encode(x)
    n <- length(body)
    writeBin(c(as.raw((n %/% 256^(3:0)) %% 256), body), .okay_out)
  } else {
    txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = I(17))
    writeBin(charToRaw(paste0(enc2utf8(as.character(txt)), "\n")), .okay_out)
  }
  flush(.okay_out)
  # a configure takes effect AFTER its own answer
  sw <- .okay_wire$switch_to
  if (!is.null(sw)) {
    .okay_wire$format <- sw$format
    .okay_wire$compress <- sw$compress
    .okay_wire$frames <- sw$frames
    .okay_wire$switch_to <- NULL
  }
}

read_n <- function(n) {
  b <- raw(0)
  while (length(b) < n) {
    r <- readBin(con, "raw", n - length(b))
    if (length(r) == 0L) return(NULL)
    b <- c(b, r)
  }
  b
}

# the next request, or NULL when the host is gone
read_msg <- function() {
  repeat {
    if (okay_framed()) {
      len <- read_n(4L)
      if (is.null(len)) return(NULL)
      n <- sum(as.numeric(len) * 256^(3:0))
      body <- if (n == 0) raw(0) else read_n(n)
      if (is.null(body)) return(NULL)
      return(okay_decode(body))
    }
    line <- readLines(con, n = 1L, warn = FALSE)
    if (length(line) == 0L) return(NULL)
    if (nzchar(trimws(line))) return(jsonlite::fromJSON(line, simplifyVector = FALSE))
  }
}

con <- file("stdin", open = "rb")

say(list(shim = SHIM, r = paste(R.version$major, R.version$minor, sep = "."),
         speaks = list(format = list("json", "cbor"), compress = list("zlib"),
                       frames = if (.okay_has_arrow) list("columnar", "arrow") else list("columnar"))))

# ---- the loop ---------------------------------------------------------

while (!is.null(req <- read_msg())) say(serve(req))
