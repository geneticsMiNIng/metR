
find_DMR_Wilcoxon <- function(data){
  print("Started: Finding DMR by Wilcoxon test")
  suppressWarnings(
  data %>% do(test_wilcoxon(data=.)) %>%
    filter(!is.nan(p.value)) %>% arrange(p.value) %>% ungroup()
  )}

find_DMR_Ttest <- function(data){
  print("Started: Finding DMR by t-test")
  suppressWarnings(
  data %>% do(test_t(data=.)) %>%
    filter(!is.nan(p.value)) %>% arrange(p.value) %>% ungroup()
  )}

find_DMR_KS <- function(data){
  print("Started: Finding DMR by KS test")
  suppressWarnings(
  data %>% do(test_ks(data=.)) %>%
    filter(!is.nan(p.value)) %>% arrange(p.value) %>% ungroup()
  )}


find_DMR_Reg_Log <- function(data, p.value.log.reg, beta.coef.max){
  print("Started: Finding DMR by Logistic Regression")
  suppressWarnings(
  if (!is.null(p.value.log.reg)){
    data %>% do(reg_log(data=.)) %>%
      filter(p.value < p.value.log.reg, !is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(-abs(beta.coef)) %>% ungroup()
  }else{
    data %>% do(reg_log(data=.)) %>%
      filter(!is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(p.value) %>% ungroup()}
)}


find_DMR_Reg_Mixed <- function(data, p.value.reg.mixed, beta.coef.max){
  print("Started: Finding DMR by Logistic Regression with Mixed Effects")

  suppressWarnings(
  if (!is.null(p.value.reg.mixed)){
    data %>% do(reg_mixed(data=.)) %>%
      filter(p.value < p.value.reg.mixed, !is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(-abs(beta.coef)) %>% ungroup()
  }else{
    data %>% do(reg_mixed(data=.)) %>%
      filter(!is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(p.value) %>% ungroup()}
  )}


find_DMR_Reg_Corr_Mixed <- function(data, p.value.reg.corr.mixed, beta.coef.max){
  print("Started: Finding DMR by Logistic Regression with Mixed Effects with Correlation Matrix")
  data('mean.acf.chr')
  acf <- mean.acf.chr[-1]
  suppressWarnings(
  if (!is.null(p.value.reg.corr.mixed)){
    data %>% do(reg_corr_mixed(data=., acf = acf)) %>%
      filter(p.value < p.value.reg.corr.mixed, !is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(-abs(beta.coef)) %>% ungroup()
  }else{
    data %>% do(reg_corr_mixed(data=., acf = acf)) %>%
      filter(!is.nan(p.value),
             abs(beta.coef) < beta.coef.max) %>% arrange(p.value) %>% ungroup()}
 )}

find_DMR_given_methods <- function(data, methods, p.value.log.reg,
                                 p.value.reg.mixed, p.value.reg.corr.mixed, beta.coef.max){
  result <- list()

  if ('Wilcoxon' %in% methods){
    result$Wilcoxon <- find_DMR_Wilcoxon(data)}

  if ('Ttest' %in% methods){
    result$Ttest <- find_DMR_Ttest(data)}

  if ('KS' %in% methods){
    result$KS <- find_DMR_KS(data)}

  if ('Reg.Log' %in% methods){
    result$Reg.Log <- find_DMR_Reg_Log(data, p.value.log.reg, beta.coef.max) }

  if ('Reg.Mixed' %in% methods){
    result$Reg.Mixed <- find_DMR_Reg_Mixed(data, p.value.reg.mixed, beta.coef.max)}

  if ('Reg.Corr.Mixed' %in% methods){
    result$Reg.Corr.Mixed <-
      find_DMR_Reg_Corr_Mixed(data, p.value.reg.corr.mixed, beta.coef.max)}

  return(result)
}

find_DMR_prep_data <- function(data, map){
  data %<>% mutate(poz.new = poz)

  setDT(data)
  setDT(map)

  data <- data[map, on=.(poz.new >= start, poz.new <= end, chr = chr), nomatch = 0,
               .(chr, poz, prob, no, meth, unmeth, meth.rate, start, end), allow.cartesian = T] %>%
    group_by(chr, start, end)
}
