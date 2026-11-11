# The codebook: every value the blinding replaces and its code, in a PRIVATE
# CSV the user can edit -- TYPE, ACTUAL_VALUE, BLINDED_VALUE, NOTE, CHECK.
#   TYPE: TREATMENT (-> TRT01, ...), ANALYTE (code or name -> ANL01, ...), TEXT (other private text -> TERM01),
#         or a grouping group of config codebook.category_groups (ARM, ARMCD, COHORT, COHORTCD, REGIMEN):
#         the whole value is replaced (ARM01, ARMCD01, ...).
#   BLINDED_VALUE: the code. A row whose ACTUAL_VALUE is not itself a treatment/analyte of the data is another
#         spelling; its BLINDED_VALUE names the treatment/analyte it belongs to. '?' = to be answered (the run
#         stops), '-' = leave the text as it is, blank = next free code of its type.
#   CHECK: filled only on rows a reviewer must decide on (filter on it in Excel), e.g. an analyte code without
#         a name (ADNCA PARAMCD with no ATEST) that was given the code of the analyte it seems to belong to.
# A code that is both a treatment and an analyte legitimately gets a TRT code and an ANL code.
# build_maps reads the codebook, appends rows for values without one (never changing the user's rows) and
# stops once when rows were added or rows need an answer.

.UB_CB_COLS <- c("TYPE", "ACTUAL_VALUE", "BLINDED_VALUE", "NOTE", "CHECK")
.UB_WORD <- "(?<![A-Za-z0-9])[A-Za-z][A-Za-z0-9-]*[A-Za-z0-9]"
.UB_CB_DEFAULTS <- list(treatment_template = "TRT{seq:02d}", analyte_template = "ANL{seq:02d}",
                        text_template = "TERM{seq:02d}", category_template = "{group}{seq:02d}",
                        category_in_text_min_length = 6L, category_groups = list())

.ub_cb_settings <- function(cfg) {
  s <- .UB_CB_DEFAULTS
  for (k in names(cfg$codebook)) if (!startsWith(k, "_")) s[[k]] <- cfg$codebook[[k]]
  s
}

# list(GROUP = list(vars, number_with, in_text)) in dependency order
.ub_cb_groups <- function(cfg) {
  raw <- .ub_cb_settings(cfg)$category_groups
  raw <- raw[!startsWith(names(raw), "_")]
  names(raw) <- toupper(names(raw))
  out <- list(); todo <- raw
  while (length(todo)) {
    progressed <- FALSE
    for (g in names(todo)) {
      v <- todo[[g]]
      nw <- if (is.null(v$number_with) || !nzchar(v$number_with)) NA_character_ else toupper(v$number_with)
      if (!is.na(nw) && !nw %in% names(raw)) ub_error("codebook.category_groups.", g, ": number_with '", nw, "' is not a group")
      if (is.na(nw) || nw %in% names(out)) {
        out[[g]] <- list(vars = toupper(unname(.chr(v$vars))), number_with = nw, in_text = isTRUE(v$in_text))
        todo[[g]] <- NULL; progressed <- TRUE
      }
    }
    if (!progressed) ub_error("codebook.category_groups: circular number_with among ", paste(names(todo), collapse = ", "))
  }
  out
}

# c(VARIABLE = GROUP) for every variable coded as a whole value
.ub_cb_category_vars <- function(cfg) {
  g <- .ub_cb_groups(cfg)
  unlist(lapply(names(g), function(x) setNames(rep(x, length(g[[x]]$vars)), g[[x]]$vars)))
}

.ub_read_codebook <- function(path) {
  e <- data.frame(TYPE = character(), ACTUAL_VALUE = character(), BLINDED_VALUE = character(), NOTE = character(),
                  CHECK = character(), stringsAsFactors = FALSE)
  if (is.null(path) || !file.exists(path)) return(e)
  t <- utils::read.csv(path, colClasses = "character", na.strings = character(), check.names = FALSE, fileEncoding = "UTF-8-BOM")
  names(t) <- toupper(trimws(names(t)))
  for (c in .UB_CB_COLS) if (is.null(t[[c]])) t[[c]] <- rep("", nrow(t))
  t <- t[, .UB_CB_COLS, drop = FALSE]
  for (c in .UB_CB_COLS) t[[c]] <- trimws(t[[c]])
  t$TYPE <- toupper(t$TYPE)
  t <- t[nzchar(t$ACTUAL_VALUE) | nzchar(t$TYPE), , drop = FALSE]
  rownames(t) <- NULL
  t
}

.ub_write_codebook <- function(path, t) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(t[, .UB_CB_COLS, drop = FALSE], path, row.names = FALSE, fileEncoding = "UTF-8")
}

.ub_nms <- function(x) if (is.null(names(x))) character() else names(x)
.ub_nonblank_u <- function(x) { x <- trimws(as.character(unique(x))); x[!is.na(x) & nzchar(x)] }
.ub_cb_num <- function(tok) {
  m <- regmatches(tok, regexec("(\\d+)\\D*$", tok))
  vapply(m, function(z) if (length(z) == 2) as.integer(z[2]) else NA_integer_, 1L)
}
.ub_cb_places <- function(p, n = 3) {
  if (is.null(p)) return("")
  paste0(paste(utils::head(p, n), collapse = ", "), if (length(p) > n) ", ..." else "")
}

.ub_cb_discover <- function(datasets, cfg) {
  treatments <- character(); analytes <- list(); codes <- character(); where <- list()
  # a code without a name next to it (e.g. ADNCA.PARAMCD without ATEST): the other analyte codes on its
  # records (partners: code, partner, place) and its PARAM labels (labels: code, place, text)
  noname <- character()
  partners <- data.frame(c = character(), p = character(), place = character(), stringsAsFactors = FALSE)
  labels <- data.frame(c = character(), place = character(), text = character(), stringsAsFactors = FALSE)
  code_vars <- unique(unlist(lapply(cfg$datasets, function(p) {
    ev <- p$exact_vars
    c(names(ev)[vapply(ev, function(r) identical(as.character(r), "analyte_code"), logical(1))],
      if (length(p$analyte_pair)) as.character(p$analyte_pair[[1]]))
  })))
  groups <- .ub_cb_groups(cfg)
  cats <- lapply(groups, function(g) list()); pairs <- cats; rpairs <- cats
  addw <- function(lst, key, place) { lst[[key]] <- union(lst[[key]], place); lst }
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; ds <- cfg$datasets[[nm]]
    for (var in names(ds$exact_vars)) {
      if (!var %in% names(df)) next
      role <- ds$exact_vars[[var]]
      for (v in .ub_nonblank_u(df[[var]])) {
        if (role == "treatment") { treatments <- union(treatments, v); where <- addw(where, paste0("TREATMENT\r", v), paste0(nm, ".", var)) }
        if (role == "analyte_code") { codes <- union(codes, v); where <- addw(where, paste0("ANALYTE\r", v), paste0(nm, ".", var)) }
        if (role == "analyte_name") where <- addw(where, paste0("ANALYTE\r", v), paste0(nm, ".", var))
      }
    }
    pair <- .chr(ds$analyte_pair)
    if (length(pair) && pair[1] %in% names(df) && (length(pair) < 2 || !pair[2] %in% names(df))) {
      noname <- union(noname, .ub_nonblank_u(df[[pair[1]]]))
      a <- trimws(as.character(df[[pair[1]]]))
      for (oc in sort(setdiff(code_vars, pair[1]), method = "radix")) {
        if (!oc %in% names(df)) next
        b <- trimws(as.character(df[[oc]]))
        ok <- !is.na(a) & !is.na(b) & nzchar(a) & nzchar(b) & a != b
        u <- unique(data.frame(c = a[ok], p = b[ok], stringsAsFactors = FALSE))
        u <- u[!paste(u$c, u$p, sep = "\r") %in% paste(partners$c, partners$p, sep = "\r"), , drop = FALSE]
        if (nrow(u)) partners <- rbind(partners, data.frame(c = u$c, p = u$p, place = paste0(nm, ".", oc), stringsAsFactors = FALSE))
      }
      if ("PARAM" %in% names(df)) {
        tx <- trimws(as.character(df[["PARAM"]]))
        ok <- !is.na(a) & !is.na(tx) & nzchar(a) & nzchar(tx)
        labels <- unique(rbind(labels, data.frame(c = a[ok], place = rep(paste0(nm, ".PARAM"), sum(ok)), text = tx[ok],
                                                  stringsAsFactors = FALSE)))
      }
    }
    if (length(pair) && pair[1] %in% names(df)) {
      cd <- trimws(as.character(df[[pair[1]]]))
      nmv <- if (pair[2] %in% names(df)) trimws(as.character(df[[pair[2]]])) else rep("", length(cd))
      k <- !duplicated(paste(cd, nmv, sep = "\r")) & nzchar(cd)
      for (i in which(k)) {
        l <- analytes[[cd[i]]]; if (is.null(l)) l <- character()
        if (nzchar(nmv[i]) && !nmv[i] %in% l && nmv[i] != cd[i]) l <- c(l, nmv[i])
        analytes[[cd[i]]] <- l
      }
    }
    cols <- setNames(names(df), toupper(names(df)))
    for (g in names(groups)) {
      sp <- groups[[g]]
      pvars <- if (is.na(sp$number_with)) character() else groups[[sp$number_with]]$vars
      for (i in seq_along(sp$vars)) {
        var <- sp$vars[i]
        if (!var %in% names(cols) || !is.character(df[[cols[[var]]]])) next
        col <- trimws(as.character(df[[cols[[var]]]]))
        for (v in .ub_nonblank_u(col)) cats[[g]] <- addw(cats[[g]], v, paste0(nm, ".", var))
        pv <- if (i <= length(pvars) && pvars[i] %in% names(cols)) pvars[i] else pvars[pvars %in% names(cols)][1]
        if (length(pv) && !is.na(pv)) {
          p <- trimws(as.character(df[[cols[[pv]]]]))
          ok <- !is.na(col) & nzchar(col) & !is.na(p) & nzchar(p)
          u <- unique(data.frame(v = col[ok], p = p[ok], stringsAsFactors = FALSE))
          for (j in seq_len(nrow(u))) {
            pairs[[g]][[u$v[j]]] <- union(pairs[[g]][[u$v[j]]], u$p[j])
            rpairs[[g]][[u$p[j]]] <- union(rpairs[[g]][[u$p[j]]], u$v[j])
          }
        }
      }
    }
  }
  list(treatments = treatments, analytes = analytes, codes = codes, where = where, categories = cats,
       pairs = pairs, rpairs = rpairs, noname = noname, partners = partners, labels = labels)
}

# c(treatment = analyte code) for a drug that is also an analyte (same name, any case)
.ub_cb_linked <- function(treatments, analytes) {
  out <- character()
  for (t in treatments) for (cd in names(analytes))
    if (toupper(t) %in% toupper(c(cd, analytes[[cd]]))) { out[[t]] <- cd; break }
  out
}

.ub_cb_known_analytes <- function(rows) {
  s <- rows[rows$TYPE == "ANALYTE" & !rows$BLINDED_VALUE %in% c("", "?", "-"), , drop = FALSE]
  if (!nrow(s)) return("-")
  toks <- sort(unique(s$BLINDED_VALUE), method = "radix")
  paste(vapply(toks, function(t) paste0(t, " = ", paste(s$ACTUAL_VALUE[s$BLINDED_VALUE == t], collapse = ", ")), ""), collapse = "; ")
}

.ub_cb_new_rows <- function(rows, d, cfg, study_identities, rng) {
  st <- .ub_cb_settings(cfg)
  new <- rows[0, , drop = FALSE]
  all_rows <- function() rbind(rows, new)
  used <- function(tp) { a <- all_rows(); n <- .ub_cb_num(a$BLINDED_VALUE[a$TYPE == tp]); n[!is.na(n)] }
  has <- function(tp, actual) {
    a <- all_rows()
    if (tp %in% c("TREATMENT", "ANALYTE")) any(a$TYPE == tp & a$ACTUAL_VALUE == actual)
    else any(a$TYPE == tp & toupper(a$ACTUAL_VALUE) == toupper(actual))
  }
  add <- function(tp, actual, blinded, note, check = "") {
    if (has(tp, actual)) return(invisible())
    new[nrow(new) + 1L, ] <<- c(tp, actual, blinded, note, check)
  }
  token_of <- function(tp, actual) { a <- all_rows(); x <- a$BLINDED_VALUE[a$TYPE == tp & a$ACTUAL_VALUE == actual]; if (length(x)) x[1] else NA_character_ }
  next_free <- function(tp, floor = 0L) max(c(floor, used(tp))) + 1L
  tpl <- function(t, seq, group = "") .fmt_tpl(gsub("{group}", group, t, fixed = TRUE), seq)
  treatments <- d$treatments; analytes <- d$analytes; where <- d$where
  link <- .ub_cb_linked(treatments, analytes)
  for (t in .shuffle(sort(treatments, method = "radix"), rng)) {
    if (has("TREATMENT", t)) next
    a <- all_rows()
    same <- a$BLINDED_VALUE[a$TYPE == "TREATMENT" & toupper(a$ACTUAL_VALUE) == toupper(t) & !a$BLINDED_VALUE %in% c("", "?", "-")]
    tok <- if (length(same)) same[1] else tpl(st$treatment_template, next_free("TREATMENT"))
    add("TREATMENT", t, tok, paste0("treatment (", .ub_cb_places(where[[paste0("TREATMENT\r", t)]]), ")"))
  }
  t_of <- setNames(names(link), unname(link))
  ok_tok <- function(x) !is.na(x) & !x %in% c("", "?", "-")
  order_a <- .shuffle(sort(.ub_nms(analytes), method = "radix"), rng)
  nameless <- order_a[vapply(order_a, function(cd) cd %in% d$noname && !length(analytes[[cd]]) &&
                               is.na(token_of("ANALYTE", cd)) && length(analytes) > 1, logical(1))]
  for (cd in c(setdiff(order_a, nameless), nameless)) {
    if (cd %in% nameless) {
      # e.g. ADNCA.PARAMCD 'PXYZ' with no ATEST: probably another code of an analyte named elsewhere
      ev <- character()                     # code -> why (first evidence per code)
      pt <- d$partners[d$partners$c == cd, , drop = FALSE]
      for (j in seq_len(nrow(pt))) {
        tk <- token_of("ANALYTE", pt$p[j])
        if (ok_tok(tk) && !tk %in% names(ev)) ev[[tk]] <- paste0(pt$p[j], " (on the same records in ", pt$place[j], ")")
      }
      if (!length(ev)) {
        a <- all_rows()
        nm_ok <- a$TYPE == "ANALYTE" & a$ACTUAL_VALUE != cd & ok_tok(a$BLINDED_VALUE)
        nms <- a$ACTUAL_VALUE[nm_ok]; tks <- a$BLINDED_VALUE[nm_ok]
        o <- order(-nchar(nms), method = "radix"); nms <- nms[o]; tks <- tks[o]
        lb <- d$labels[d$labels$c == cd, , drop = FALSE]
        lb <- lb[order(lb$place, lb$text, method = "radix"), , drop = FALSE]
        for (j in seq_len(nrow(lb))) for (i in seq_along(nms)) {
          if (nchar(nms[i]) >= 4 && grepl(toupper(nms[i]), toupper(lb$text[j]), fixed = TRUE)) {
            if (!tks[i] %in% names(ev)) ev[[tks[i]]] <- paste0(nms[i], " (named in ", lb$place[j], " '", lb$text[j], "')")
            break
          }
        }
      }
      place <- .ub_cb_places(where[[paste0("ANALYTE\r", cd)]])
      if (length(ev) == 1) {
        add("ANALYTE", cd, names(ev), paste0("analyte code without a name (", place, "); given the code of ", strsplit(ev[[1]], " ")[[1]][1]),
            paste0("same analyte as ", ev[[1]], "? Keep ", names(ev), " if so; otherwise give it a new code"))
      } else if (length(ev) > 1) {
        ev <- ev[order(names(ev), method = "radix")]
        add("ANALYTE", cd, "?", paste0("analyte code without a name (", place, ")"),
            paste0("which analyte is this? candidates: ", paste(names(ev), "=", ev, collapse = "; ")))
      } else {
        others <- .ub_cb_known_analytes(all_rows())
        add("ANALYTE", cd, tpl(st$analyte_template, next_free("ANALYTE", max(c(0L, used("TREATMENT"))))),
            paste0("analyte code without a name (", place, ")"),
            paste0("analyte code without a name: a separate analyte, or another code of one of ", others,
                   "? If it is the same, give it that code"))
      }
      next
    }
    tok <- token_of("ANALYTE", cd)
    if (is.na(tok)) {
      t <- if (cd %in% names(t_of)) t_of[[cd]] else NA_character_
      k <- if (!is.na(t)) .ub_cb_num(token_of("TREATMENT", t)) else NA_integer_
      if (is.na(k) || k %in% used("ANALYTE")) k <- next_free("ANALYTE", max(c(0L, used("TREATMENT"))))
      tok <- tpl(st$analyte_template, k)
      add("ANALYTE", cd, tok, paste0("analyte code (", .ub_cb_places(where[[paste0("ANALYTE\r", cd)]]), ")",
                                     if (!is.na(t)) paste0("; same number as treatment ", t) else ""))
    }
    for (nmv in analytes[[cd]]) add("ANALYTE", nmv, tok, paste0("analyte name of ", cd, " (", .ub_cb_places(where[[paste0("ANALYTE\r", nmv)]]), ")"))
  }
  or_ask <- function(x) if (is.na(x)) "?" else x
  for (t in names(study_identities$treatment_aliases)) for (al in .chr(study_identities$treatment_aliases[[t]]))
    add("TREATMENT", al, or_ask(token_of("TREATMENT", t)), paste0("other spelling of ", t, " (study settings)"))
  for (cd in names(study_identities$analyte_aliases)) for (al in .chr(study_identities$analyte_aliases[[cd]]))
    add("ANALYTE", al, or_ask(token_of("ANALYTE", cd)), paste0("other spelling of ", cd, " (study settings)"))
  for (al in names(study_identities$analyte_code_aliases)) {
    cd <- as.character(study_identities$analyte_code_aliases[[al]])
    add("ANALYTE", al, or_ask(token_of("ANALYTE", cd)), paste0("another code of ", cd, " (study settings)"))
  }
  for (tm in .chr(study_identities$private_terms)) add("TEXT", tm, tpl(st$text_template, next_free("TEXT")), "private text (study settings)")
  a <- all_rows(); known_a <- toupper(a$ACTUAL_VALUE[a$TYPE == "ANALYTE"])
  for (cd in sort(d$codes, method = "radix")) if (!toupper(cd) %in% known_a)
    add("ANALYTE", cd, "?", paste0("analyte code without a name (", .ub_cb_places(where[[paste0("ANALYTE\r", cd)]]),
                                   ") -- give the code of its analyte (", .ub_cb_known_analytes(all_rows()), ")"))
  groups <- .ub_cb_groups(cfg)
  for (g in names(groups)) {
    sp <- groups[[g]]
    vals <- .shuffle(sort(.ub_nms(d$categories[[g]]), method = "radix"), rng)
    partner <- integer()
    if (!is.na(sp$number_with)) {
      a <- all_rows(); s <- a[a$TYPE == sp$number_with, , drop = FALSE]
      n <- .ub_cb_num(s$BLINDED_VALUE); partner <- setNames(n[!is.na(n)], s$ACTUAL_VALUE[!is.na(n)])
    }
    match <- character()
    for (v in .ub_nms(d$categories[[g]])) {
      ps <- d$pairs[[g]][[v]]
      if (length(ps) == 1 && length(d$rpairs[[g]][[ps]]) == 1 && ps %in% names(partner)) match[[v]] <- ps
    }
    floor <- if (length(match)) max(partner) else 0L
    for (v in vals) {
      if (has(g, v)) next
      k <- NA_integer_; note <- ""
      if (v %in% names(match) && !partner[[match[[v]]]] %in% used(g)) {
        k <- partner[[match[[v]]]]; note <- paste0("; same number as ", sp$number_with, " '", match[[v]], "'")
      }
      if (is.na(k)) k <- next_free(g, floor)
      add(g, v, tpl(st$category_template, k, g), paste0(g, " value (", .ub_cb_places(d$categories[[g]][[v]]), ")", note))
    }
  }
  if (nrow(new)) {
    rank <- c(TREATMENT = 0, ANALYTE = 1, TEXT = 2, setNames(3 + seq_along(groups), names(groups)))
    n <- .ub_cb_num(new$BLINDED_VALUE); n[is.na(n)] <- 0L
    prim <- ifelse(startsWith(new$NOTE, "treatment") | startsWith(new$NOTE, "analyte code"), 0L, 1L)
    new <- new[order(rank[new$TYPE], n, new$BLINDED_VALUE, prim, new$ACTUAL_VALUE, method = "radix"), , drop = FALSE]
  }
  rownames(new) <- NULL
  new
}

.ub_cb_is_abbrev <- function(word, full) {
  w <- toupper(word); f <- toupper(full)
  if (nchar(w) < 3 || nchar(w) > 8 || w == f || nchar(f) < nchar(w) + 2 || substr(w, 1, 1) != substr(f, 1, 1)) return(FALSE)
  if (w %in% strsplit(f, "[^A-Z0-9]+")[[1]]) return(FALSE)
  pos <- 1L
  for (ch in strsplit(substring(w, 2), "")[[1]]) {
    p <- regexpr(ch, substring(f, pos + 1L), fixed = TRUE)
    if (p < 0) return(FALSE)
    pos <- pos + as.integer(p)
  }
  TRUE
}

.ub_cb_suggest <- function(rows, datasets, cfg, d) {
  known <- toupper(rows$ACTUAL_VALUE)
  ok <- !rows$BLINDED_VALUE %in% c("", "?", "-")
  tok <- setNames(rows$BLINDED_VALUE[ok], paste0(rows$TYPE[ok], "\r", toupper(rows$ACTUAL_VALUE[ok])))
  tok <- tok[!duplicated(names(tok))]
  analytes <- d$analytes; treatments <- d$treatments
  out <- rows[0, , drop = FALSE]
  add <- function(tp, actual, blinded, note) {
    if (toupper(actual) %in% c(known, toupper(out$ACTUAL_VALUE))) return(invisible())
    out[nrow(out) + 1L, ] <<- c(tp, actual, blinded, note, "")
  }
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data; pn <- cfg$datasets[[nm]]$param_numeric
    if (is.null(pn) || is.null(pn$analyte_var) || !pn$analyte_var %in% names(df) || !pn$param_var %in% names(df)) next
    cl <- ub_param_class(pn, df[[pn$param_var]], cfg)
    for (v in sort(.ub_nonblank_u(df[[pn$analyte_var]][cl$kind %in% c("scale", "inverse")]), method = "radix")) {
      if (toupper(v) %in% known) next
      u <- toupper(v); hits <- character()
      for (cd in names(analytes)) for (x in c(cd, analytes[[cd]]))
        if (grepl(u, toupper(x), fixed = TRUE) || grepl(toupper(x), u, fixed = TRUE)) hits <- union(hits, cd)
      b <- if (length(hits) == 1) unname(tok[paste0("ANALYTE\r", toupper(hits))]) else NA_character_
      add("ANALYTE", v, if (is.na(b)) "?" else b,
          paste0("other spelling: ", nm, ".", pn$analyte_var, " names an analyte by this value",
                 if (!is.na(b)) paste0("; it matches analyte ", hits, " only -- check")
                 else paste0(" and it matches no single analyte -- give the code of its analyte (", .ub_cb_known_analytes(rows), ")")))
    }
  }
  fulls <- data.frame(tp = "TREATMENT", id = treatments, full = treatments, stringsAsFactors = FALSE)
  for (cd in names(analytes)) fulls <- rbind(fulls, data.frame(tp = "ANALYTE", id = cd, full = c(cd, analytes[[cd]]), stringsAsFactors = FALSE))
  skip <- names(.ub_cb_category_vars(cfg))
  for (nm in names(datasets)) {
    df <- datasets[[nm]]$data
    for (v in names(cfg$datasets[[nm]]$text_vars)) {
      if (!v %in% names(df) || toupper(v) %in% skip || !is.character(df[[v]])) next
      for (text in .ub_nonblank_u(df[[v]])) {
        for (w in regmatches(text, gregexpr(.UB_WORD, text, perl = TRUE))[[1]]) {
          if (toupper(w) %in% c(known, toupper(out$ACTUAL_VALUE))) next
          h <- unique(fulls[vapply(fulls$full, function(f) .ub_cb_is_abbrev(w, f), logical(1)), c("tp", "id"), drop = FALSE])
          th <- h[h$tp == "TREATMENT", , drop = FALSE]
          tg <- if (nrow(th)) th else h
          tg <- tg[order(tg$tp, tg$id, method = "radix"), , drop = FALSE]
          if (nrow(tg) == 1) {
            b <- unname(tok[paste0(tg$tp, "\r", toupper(tg$id))])
            add(tg$tp, w, if (is.na(b)) "?" else b, paste0("other spelling: possible abbreviation of ", tg$id, ", e.g. ", nm, ".", v, " '", text,
                                                         "' -- keep the code, or set it to '-' if it is not an abbreviation"))
          } else if (nrow(tg) > 1) {
            add(tg$tp[1], w, "?", paste0("other spelling: possible abbreviation of ", paste(tg$id, collapse = " or "), ", e.g. ", nm, ".", v,
                                         " '", text, "' -- give the code, or '-' if it is not an abbreviation"))
          }
        }
      }
    }
  }
  out
}

.ub_cb_fill_blanks <- function(rows, cfg) {
  st <- .ub_cb_settings(cfg); n <- 0L
  for (i in which(!nzchar(rows$BLINDED_VALUE))) {
    tp <- rows$TYPE[i]
    u <- .ub_cb_num(rows$BLINDED_VALUE[rows$TYPE == tp])
    tmpl <- switch(tp, TREATMENT = st$treatment_template, ANALYTE = st$analyte_template, TEXT = st$text_template, st$category_template)
    if (tp == "ANALYTE") u <- c(u, .ub_cb_num(rows$BLINDED_VALUE[rows$TYPE == "TREATMENT"]))
    u <- u[!is.na(u)]
    rows$BLINDED_VALUE[i] <- .fmt_tpl(gsub("{group}", tp, tmpl, fixed = TRUE), max(c(0L, u)) + 1L)
    rows$NOTE[i] <- paste0(if (nzchar(rows$NOTE[i])) paste0(rows$NOTE[i], "; ") else "", "code filled in by the package")
    n <- n + 1L
  }
  list(rows = rows, n = n)
}

.ub_cb_to_terms <- function(rows, d, cfg) {
  groups <- .ub_cb_groups(cfg)
  treatments <- d$treatments; analytes <- d$analytes; codes_seen <- d$codes
  link <- .ub_cb_linked(treatments, analytes); t_of <- setNames(names(link), unname(link))
  problems <- character(); keep <- list()
  out <- .empty_tbl(UB_MAP_COLS$terms)
  term <- function(tp, role, key, orig, blind) {
    s <- .ub_cb_num(blind); if (is.na(s)) s <- 0L
    out[nrow(out) + 1L, ] <<- c(tp, role, key, as.character(s), orig, blind)
  }
  name_of <- unlist(lapply(names(analytes), function(cd) setNames(rep(cd, length(analytes[[cd]])), analytes[[cd]])))
  if (is.null(name_of)) name_of <- character()
  prim_t <- treatments; prim_a <- c(names(analytes), names(name_of))
  seen <- list()
  for (i in seq_len(nrow(rows))) {
    k <- paste0(rows$TYPE[i], "\r", rows$ACTUAL_VALUE[i])
    if (!is.null(seen[[k]]) && seen[[k]] != rows$BLINDED_VALUE[i])
      problems <- c(problems, paste0("line ", i + 1, ": ", rows$TYPE[i], " '", rows$ACTUAL_VALUE[i], "' has two codes (", seen[[k]], ", ", rows$BLINDED_VALUE[i], ")"))
    seen[[k]] <- rows$BLINDED_VALUE[i]
  }
  tok_t <- character(); tok_a <- character()
  for (i in seq_len(nrow(rows))) {
    tp <- rows$TYPE[i]; a <- rows$ACTUAL_VALUE[i]; b <- rows$BLINDED_VALUE[i]
    if ((tp == "TREATMENT" && a %in% prim_t) || (tp == "ANALYTE" && a %in% prim_a)) {
      if (b %in% c("?", "-", "")) problems <- c(problems, paste0("line ", i + 1, ": ", tp, " '", a, "' must have a code (it is replaced wherever it occurs)"))
      else if (tp == "TREATMENT") tok_t[[a]] <- b
      else if (a %in% names(analytes)) tok_a[[a]] <- b
    }
  }
  allowed <- c("TREATMENT", "ANALYTE", "TEXT", names(groups))
  idx <- list(TREATMENT = split(names(tok_t), toupper(unname(tok_t))), ANALYTE = split(names(tok_a), toupper(unname(tok_a))))
  for (t in names(tok_t)) term("TREATMENT", "NAME", t, t, tok_t[[t]])
  for (cd in names(tok_a)) term("ANALYTE", "CODE", cd, cd, tok_a[[cd]])
  for (i in seq_len(nrow(rows))) {
    tp <- rows$TYPE[i]; a <- rows$ACTUAL_VALUE[i]; b <- rows$BLINDED_VALUE[i]; ln <- paste0("line ", i + 1, ": ")
    if (!tp %in% allowed) { problems <- c(problems, paste0(ln, "TYPE '", tp, "' is not one of ", paste(sort(allowed, method = "radix"), collapse = ", "))); next }
    if (!nzchar(a)) { problems <- c(problems, paste0(ln, "ACTUAL_VALUE is empty")); next }
    if (b == "?") { problems <- c(problems, paste0(ln, tp, " '", a, "' -- give its code in BLINDED_VALUE, or '-' to leave the text as it is")); next }
    if (tp %in% names(groups)) {
      if (b == "-") keep[[tp]] <- c(keep[[tp]], a) else term("CATEGORY", tp, a, a, b)
      next
    }
    if (b == "-") next
    if (tp == "TEXT") { term("PRIVATE", "TERM", a, a, b); next }
    if ((tp == "TREATMENT" && a %in% names(tok_t)) || (tp == "ANALYTE" && a %in% names(tok_a))) next
    if (tp == "ANALYTE" && a %in% names(name_of)) { term("ANALYTE", "NAME", name_of[[a]], a, b); next }
    other <- if (tp == "TREATMENT") "ANALYTE" else "TREATMENT"
    hit <- if (toupper(b) %in% names(idx[[tp]])) tp else if (toupper(b) %in% names(idx[[other]])) other else NA_character_
    if (is.na(hit)) {
      problems <- c(problems, paste0(ln, tp, " '", a, "' -- '", b, "' is not the code of ",
                                     if (tp == "ANALYTE") "an analyte" else "a treatment", " in this codebook (treatments: ",
                                     paste(sort(unique(tok_t), method = "radix"), collapse = ", "), "; analytes: ",
                                     paste(sort(unique(tok_a), method = "radix"), collapse = ", "), ")",
                                     # a stale row: written for a value of an earlier delivery that is gone now
                                     if (grepl("^(treatment \\(|analyte code|analyte name of )", rows$NOTE[i]) && !a %in% codes_seen)
                                       paste0("; '", a, "' no longer occurs in the data (left over from an earlier delivery, e.g. a ",
                                              "placeholder analyte that Prepare PC replaced) -- delete this row from the codebook")))
      next
    }
    for (id in idx[[hit]][[toupper(b)]]) {
      if (hit == "TREATMENT") {
        term("TREATMENT", "ALIAS", id, a, tok_t[[id]])
        if (id %in% names(link)) term("ANALYTE", "ALIAS", link[[id]], a, if (link[[id]] %in% names(tok_a)) tok_a[[link[[id]]]] else tok_t[[id]])
      } else {
        role <- if (a %in% codes_seen && !a %in% names(analytes)) "CODE_ALIAS" else "ALIAS"
        term("ANALYTE", role, id, a, tok_a[[id]])
        if (id %in% names(t_of) && role == "ALIAS")
          term("TREATMENT", "ALIAS", t_of[[id]], a, if (t_of[[id]] %in% names(tok_t)) tok_t[[t_of[[id]]]] else tok_a[[id]])
      }
    }
  }
  out <- out[!duplicated(out[, c("TYPE", "ROLE", "IDENTITY_KEY", "ORIG_TEXT")]), , drop = FALSE]
  rownames(out) <- NULL
  reals <- toupper(out$ORIG_TEXT[out$TYPE != "CATEGORY"])
  bad <- sort(unique(out$BLIND_TEXT[toupper(out$BLIND_TEXT) %in% reals]), method = "radix")
  if (length(bad)) problems <- c(problems, paste0("code(s) ", paste(bad, collapse = ", "), " are themselves real values -- choose other codes"))
  for (cd in codes_seen)
    if (!cd %in% names(analytes) && !any(out$ROLE == "CODE_ALIAS" & out$ORIG_TEXT == cd) &&
        !any(rows$TYPE == "ANALYTE" & rows$ACTUAL_VALUE == cd))
      problems <- c(problems, paste0("analyte code '", cd, "' has no row -- run again to add it"))
  list(terms = out, keep = keep, problems = problems)
}

.ub_update_codebook <- function(path, datasets, cfg, study_identities, seed = NULL, log = NULL) {
  rows <- .ub_read_codebook(path)
  d <- .ub_cb_discover(datasets, cfg)
  rng <- .ub_rng(seed, 7000L)
  fb <- .ub_cb_fill_blanks(rows, cfg); rows <- fb$rows
  new <- .ub_cb_new_rows(rows, d, cfg, study_identities, rng)
  new <- rbind(new, .ub_cb_suggest(rbind(rows, new), datasets, cfg, d))
  # CHECK: the rows a reviewer must decide on (filter on it in Excel)
  nochk <- !nzchar(new$CHECK)
  ask <- nochk & new$BLINDED_VALUE == "?"
  new$CHECK[ask] <- "answer needed: give the code (see NOTE), or '-' to leave the text as it is"
  sp <- nochk & !ask & startsWith(new$NOTE, "other spelling")
  new$CHECK[sp] <- paste0("is this really a spelling of it? Keep ", new$BLINDED_VALUE[sp], ", or set '-'")
  all <- rbind(rows, new)
  if (nrow(new) || fb$n || !file.exists(path)) .ub_write_codebook(path, all)
  if (is.function(log)) log("codebook ", path, ": ", nrow(rows), " row(s) read, ", nrow(new), " new, ", fb$n, " code(s) filled in")
  tt <- .ub_cb_to_terms(all, d, cfg)
  msgs <- character()
  if (nrow(new) || fb$n) {
    shown <- utils::head(new, 40)
    msgs <- c(msgs, paste0(nrow(new), " new row(s) were added to ", path, if (fb$n) paste0(" and ", fb$n, " empty code(s) filled in") else "",
                           if (nrow(new)) paste0(": ", paste(paste(shown$TYPE, shown$ACTUAL_VALUE, "->", shown$BLINDED_VALUE), collapse = "; "),
                                                 if (nrow(new) > 40) " ..." else "") else ""))
  }
  if (length(tt$problems)) msgs <- c(msgs, paste0(path, " needs an answer -- ", paste(tt$problems, collapse = " | ")))
  if (length(msgs)) ub_error(paste(msgs, collapse = " "), ". Review the codebook (BLINDED_VALUE: the code; '?' = to be answered; ",
                             "'-' = leave the text as it is), then run again")
  if (is.function(log)) {
    tm <- tt$terms
    p <- tm[tm$TYPE %in% c("TREATMENT", "ANALYTE") & tm$ROLE %in% c("NAME", "CODE"), , drop = FALSE]
    for (tp in sort(unique(p$TYPE), method = "radix")) {
      s <- p[p$TYPE == tp, , drop = FALSE]
      n <- tapply(s$IDENTITY_KEY, toupper(s$BLIND_TEXT), function(x) length(unique(x)))
      for (tok in names(n)[n > 1])
        log("codebook: ", tp, " code ", tok, " is shared by ", n[[tok]], " ", tolower(tp), "s (merged on purpose?)")
    }
  }
  list(terms = tt$terms, keep = tt$keep)
}
