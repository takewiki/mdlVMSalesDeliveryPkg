# =====================================================================
# DeliveryLocation - Export tab: data access
#   The filter conditions built on the Export tab are appended to the
#   query, so the filtering happens in the database.
# =====================================================================

#' Field list used by the Export tab
#'   label      : result column name
#'   expr       : expression in the SELECT list
#'   where_expr : expression used by the WHERE clause (defaults to expr)
#'   type       : text / number / date
#' @export
DeliveryLocationExport_fields <- function() {
  data.frame(
    label = c("Delivery Location", "Sales OrderID", "Sales OrderQty", "PN",
              "Product Name", "Sales OrderDate", "Delivery Date", "Delivery Qty"),
    expr = c("[FDeliveryLocation]", "[FSalesOrderID]", "[FSalesOrderQty]", "[FPN]",
             "[FProductName]", "cast([FSalesOrderDate] as date)",
             "[FDeliveryDate]", "[FDeliveryQty]"),
    where_expr = c("[FDeliveryLocation]", "[FSalesOrderID]", "[FSalesOrderQty]", "[FPN]",
                   "[FProductName]", "cast([FSalesOrderDate] as date)",
                   "cast([FDeliveryDate] as date)", "[FDeliveryQty]"),
    type = c("text", "text", "number", "text",
             "text", "date", "date", "number"),
    stringsAsFactors = FALSE
  )
}

#' Quote a SQL Server string literal
#' @export
DeliveryLocationExport_sql_string <- function(x) {
  paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
}

#' Build the WHERE shared by the preview, the grid and the export
#' @export
DeliveryLocationExport_where <- function(FWhere = "", FDeliveryLocation = "") {
  conds <- character(0)
  loc <- if (is.null(FDeliveryLocation)) "" else trimws(as.character(FDeliveryLocation))
  if (nzchar(loc) && !identical(loc, "*")) {
    conds <- c(conds, paste0("[FDeliveryLocation] like '%",
                             gsub("'", "''", loc, fixed = TRUE), "%'"))
  }
  if (!is.null(FWhere) && nzchar(FWhere)) {
    conds <- c(conds, paste0("(", FWhere, ")"))
  }
  if (!length(conds)) "" else paste(conds, collapse = " and ")
}

#' Query used by the Export tab
#' @export
DeliveryLocationExport_select_sql <- function(FWhere = "", FDeliveryLocation = "",
                                              cols = NULL) {
  meta <- DeliveryLocationExport_fields()
  if (!is.null(cols) && length(cols)) {
    meta <- meta[meta$label %in% cols, , drop = FALSE]
    if (!nrow(meta)) meta <- DeliveryLocationExport_fields()
  }
  sel <- paste(paste0(meta$expr, " as [", meta$label, "]"), collapse = ",
       ")

  sql <- paste0("select ", sel, "
  from rds_erp_vw_rpt_PNBySalesOrder")
  w <- DeliveryLocationExport_where(FWhere = FWhere, FDeliveryLocation = FDeliveryLocation)
  if (nzchar(w)) sql <- paste0(sql, "
 where ", w)
  paste0(sql, "
 order by FSalesOrderDate desc, FPN asc")
}

#' Run the export query
#' @export
DeliveryLocationExport_select <- function(erp_token, FDeliveryLocation = "", FWhere = "",
                                          cols = NULL) {
  sql <- DeliveryLocationExport_select_sql(FWhere = FWhere,
                                           FDeliveryLocation = FDeliveryLocation,
                                           cols = cols)
  res <- tsda::sql_select2(token = erp_token, sql = sql)
  res
}
