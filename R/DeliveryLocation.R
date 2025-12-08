
#' 查询DeliveryLocation_select
#'
#' @param erp_token

#' @return 无返回值
#' @export
#'
#' @examples
#' DeliveryLocation_select()
DeliveryLocation_select<- function(erp_token,FDeliveryLocation) {

  sql=paste0("

exec rds_erp_proc_rpt_PNSerialNoTrace '",FDeliveryLocation,"'
             ")

  res=tsda::sql_select2(token = erp_token,sql = sql)
  return(res)
}
