# Check levels
checklevels.R <-
  function(data){levels(as.factor(data$Variable))}

# Convert data.frame to png
tablepng.R <- 
  function(data, file){
    tg = gridExtra::tableGrob(data, rows=NULL)
    h = grid::convertHeight(sum(tg$heights), "in", TRUE)
    w = grid::convertWidth(sum(tg$widths), "in", TRUE)
    ggplot2::ggsave(paste(file,".png",sep=""),
                    tg, width=w, height=h, dpi=300)}

tablepng2.R <- 
  function(data, file){
    tg = gridExtra::tableGrob(data, rows=NULL)
    h = grid::convertHeight(sum(tg$heights), "in", TRUE)
    w = grid::convertWidth(sum(tg$widths), "in", TRUE)
    ggplot2::ggsave(paste0(file),
                    tg, width=w, height=h, dpi=300)}

# Add star
addsd.R <-
function(Value){
  ifelse(Value < 0.001, paste("***"),
         ifelse(Value < 0.01,paste("**"),
                ifelse(Value < 0.05,paste("*"),
                       paste(""))))
  }

library("htmltools")
library("webshot") 
export_formattable <- function(f, file, width,height,
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay,
          zoom = 10)
}

# Test
fisher.R <- function(a,b,c,d){
  data <- matrix(c(a,b,c,d),ncol=2)
  c(p = fisher.test(data)$p.value,  # simulate.p.value=TRUE, B=1e4, workspace = 100000
    OR = fisher.test(data)$estimate,
    CI = fisher.test(data)$conf.int)}


# Pediatric eGFR
# age = 0.1
# ht = 0.8
# cre = 1
# sex = "M"

refcre <- function(ht, sex){
  ifelse(sex %in% c("M","Male","male"),
         -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
         -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)}

percre <- function(cre, ht, sex){
  (ifelse(sex %in% c("M","Male","male"),
         -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
         -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778))/cre*100}

pegfr <- function(cre, age, ht, sex){
  R = ifelse(age<(3/12), NA,
             ifelse(0.107*log(age*12)+0.656, 1))
  r_cre = ifelse(sex %in% c("M","Male","male"),
                  -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
                  -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)
  110.2*R*r_cre/cre+2.93 }

ntf <- function(x){mailR::send.mail(from = "shibataryohei@gmail.com",
                                    to = "shibataryohei@gmail.com",
                                    subject = "RStudio Notification",
                                    body = "Your R script completed",
                                    encoding = "utf-8",
                                    smtp = list(host.name = "smtp.gmail.com",
                                                port = 587,
                                                user.name = "shibataryohei@gmail.com",
                                                passwd = x,
                                                ssl = T),
                                    authenticate = TRUE,
                                    send = TRUE)}