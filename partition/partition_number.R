partition_number = function(df, feed, con , tail) {
  enq_feed = enquo(feed)
  enq_con  = enquo(con)
  enq_tail = enquo(tail)

  df %>% mutate(
    FT = !!enq_feed - !!enq_tail,
    PT = !!enq_con - !!enq_tail,
    FTPT = FT * PT,
    sum_FTPT = sum(FTPT),
    PTPT = PT * PT,
    sum_PTPT = sum(PTPT),
    yield = sum_FTPT / sum_PTPT,
    P = !!enq_con * yield,
    `T` = !!enq_tail * (1 - yield),
    `F` = P + `T`,
    PN = (P / `F`) * 100
  )
}
