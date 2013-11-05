#code from ETAS package

ppx=function (data, domain = NULL, coord.type = NULL) 
{
  data <- as.hyperframe(data)
  suitable <- with(unclass(data), vtype == "dfcolumn" & (vclass == 
    "numeric" | vclass == "integer"))
  if (is.null(coord.type)) {
    ctype <- ifelse(suitable, "spatial", "mark")
  }
  else {
    stopifnot(is.character(coord.type))
    stopifnot(length(coord.type) == ncol(data))
    ctypeid <- pmatch(coord.type, ctype.table, duplicates.ok = TRUE)
    if (any(uhoh <- is.na(ctypeid))) 
      stop(paste("Unrecognised coordinate", ngettext(sum(uhoh), 
        "type", "types"), commasep(sQuote(coord.type[uhoh]))))
    if (any(uhoh <- (!suitable & ctype.real[ctypeid]))) {
      nuh <- sum(uhoh)
      stop(paste(ngettext(nuh, "Coordinate", "Coordinates"), 
        commasep(sQuote(names(data)[uhoh])), ngettext(nuh, 
          "does not", "do not"), "contain real numbers"))
    }
    ctype <- ctype.table[ctypeid]
  }
  ctype <- factor(ctype, levels = ctype.table)
  out <- list(data = data, ctype = ctype, domain = domain)
  class(out) <- "ppx"
  return(out)
}
