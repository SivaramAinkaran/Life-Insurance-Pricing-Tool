
library("shinythemes")

lifetable<- read.table("lifetable.txt",dec=".", header=TRUE)

#----------------------------------------------------------------###DEFINING VARIABLES-----------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#define a_x due for server
annuity_due_x<-function(age,i){
  limit=max(lifetable$age_x)-min(lifetable$age_x)
  k=c(0:limit)
  v=1/(1+i)
  discounting=v^k
  survival=lifetable$lx[age-16+k]/lifetable$lx[age-16]
  
  a=sum(discounting*survival,na.rm = TRUE)
  return(a)
}


#defining adue_xy
annuity_due_xy<-function(agex,agey,i){
  limit=max(lifetable$age_x)-min(lifetable$age_x)
  k=c(0:limit)
  v=(1/(1+i))
  discounting=v^k
  survivalx=lifetable$lx[agex-16+k]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+k]/lifetable$ly[agey-16]
  a=sum(discounting*survivalx*survivaly,na.rm = TRUE)
  return(a)  
  
}

#A_x whole life assurance for server
assurance_x<-function(age,i){return(1-(i/(1+i))*annuity_due_x(age,i))}

#define a_y due for server
annuity_due_y<-function(age,i){
  limit=max(lifetable$age_x)-min(lifetable$age_x)
  k=c(0:limit)
  v=1/(1+i)
  discounting=v^k
  survival=lifetable$ly[age-16+k]/lifetable$ly[age-16]
  
  a=sum(discounting*survival,na.rm = TRUE)
  return(a)
}

#A_y whole life assurance for server
assurance_y<-function(age,i){return(1-(i/(1+i))*annuity_due_y(age,i)) }

#define discounting and survival as function of n
discount_factor<-function(i,n){return((1/(1+i))^n)}
survival_x<-function(age,n){return(lifetable$lx[age-16+n]/lifetable$lx[age-16])}
survival_y<-function(age,n){return(lifetable$ly[age-16+n]/lifetable$ly[age-16])}






#defining adue_xym for m=1
annuity_due_xym1<-function(agex,agey,i){return((annuity_due_xy(agex,agey,i)))}
#defining adue_xym for m=12
annuity_due_xym12<-function(agex,agey,i){return((annuity_due_xy(agex,agey,i))-(11/24))}



#defining aARREARS_xym for m=1
annuity_arrears_xym1<-function(agex,agey,i){return((annuity_due_xy(agex,agey,i)-1))}
#defining aARREARS_xym for m=12
annuity_arrears_xym12<-function(agex,agey,i){return((annuity_due_xy(agex,agey,i)-1)+(11/24))}


#defining adue_xplusn_yplusn
annuity_due_xplusn_yplusn<-function(agex,agey,i,n){
  limit=max(lifetable$age_x)-min(lifetable$age_x)
  k=c(0:limit)
  v=1/(1+i)
  discounting=v^k
  survivalx=lifetable$lx[agex+n-16+k]/lifetable$lx[agex+n-16]
  survivaly=lifetable$ly[agey+n-16+k]/lifetable$ly[agey+n-16]
  a=sum(discounting*survivalx*survivaly,na.rm = TRUE)
  return(a)  
  
}

#defining assurance_xy
assurance_xy<-function(agex,agey,i){return(1-(i/(1+i))*annuity_due_xy(agex,agey,i)) }

#defining adue_xy_n
annuity_due_xy_n<-function(agex,agey,i,n){
  limit=n
  k=c(0:limit)
  v=1/(1+i)
  discounting=v^k
  survivalx=lifetable$lx[agex-16+k]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+k]/lifetable$ly[agey-16]
  a=sum(discounting*survivalx*survivaly,na.rm = TRUE)
  return(a)  
  
}
#defining adue_xy(m)_n for m = 1
annuity_due_xym1_n<-function(agex,agey,i,n){
  
  v=1/(1+i)
  discounting=v^n
  survivalx=lifetable$lx[agex-16+n]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+n]/lifetable$ly[agey-16]
  a=print(annuity_due_xy(agex,agey,i)-discounting*survivalx*survivaly*(annuity_due_xplusn_yplusn(agex,agey,i,n)),na.rm=TRUE)
  return(a)  
  
}
#defining adue_xy(m)_n for m = 12
annuity_due_xym12_n<-function(agex,agey,i,n){
  
  v=1/(1+i)
  discounting=v^n
  survivalx=lifetable$lx[agex-16+n]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+n]/lifetable$ly[agey-16]
  a=print(annuity_due_xy(agex,agey,i)-(11/24)-discounting*survivalx*survivaly*(annuity_due_xplusn_yplusn(agex,agey,i,n)-(11/24)),na.rm=TRUE)
  return(a)  
  
}

#defining aARREARS_xym1_n for m=1
annuity_xym1_n<-function(agex,agey,i,n){
  v=1/(1+i)
  discounting=v^n
  survivalx=lifetable$lx[agex-16+n]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+n]/lifetable$ly[agey-16]
  a=print(annuity_arrears_xym1(agex,agey,i)-discounting*survivalx*survivaly*(annuity_due_xplusn_yplusn(agex,agey,i,n)-1), na.rm=TRUE)
  return(a)
  
}

#defining aARREARS_xym12_n for m=12
annuity_xym12_n<-function(agex,agey,i,n){
  v=1/(1+i)
  discounting=v^n
  survivalx=lifetable$lx[agex-16+n]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+n]/lifetable$ly[agey-16]
  a=print(annuity_arrears_xym1(agex,agey,i)+(11/24)-discounting*survivalx*survivaly*((annuity_due_xplusn_yplusn(agex,agey,i,n)-1)+(11/24)), na.rm=TRUE)
  return(a)
}


#defining assurance_xy1_n for joint term assurance
assurance_xy1_n<-function(agex,agey,i,n){
  
  v=1/(1+i)
  discounting=v^n
  d=i/(1+i)
  survivalx=lifetable$lx[agex-16+n]/lifetable$lx[agex-16]
  survivaly=lifetable$ly[agey-16+n]/lifetable$ly[agey-16]
  a=print(((1-d*annuity_due_xy(agex,agey,i)-discounting*survivalx*survivaly)),na.rm=TRUE)
  return(a)  
  
}

#annuity due x:(n)
annuity_due_xnt <- function(age, contract_term, i){
  a_x <- annuity_due_x(age, i)
  a_xn <- annuity_due_x((age+contract_term),i)
  v_n <- discount_factor(i, contract_term)
  npx <- survival_x(age, contract_term)
  a=a_x-v_n*npx*a_xn
  return(a)
}

#annuity due x:(n)
annuity_due_xnt <- function(age, contract_term, i){
  a_x <- annuity_due_x(age, i)
  a_xn <- annuity_due_x((age+contract_term),i)
  v_n <- discount_factor(i, contract_term)
  npx <- survival_x(age, contract_term)
  a=a_x-v_n*npx*a_xn
  return(a)
}

#annuity due y:(n)
annuity_due_ynt <- function(age, contract_term, i){
  a_x <- annuity_due_y(age, i)
  a_xn <- annuity_due_y((age+contract_term),i)
  v_n <- discount_factor(i, contract_term)
  npx <- survival_y(age, contract_term)
  a=a_x-v_n*npx*a_xn
  return(a)
}
  

# DEFINING FUNCTIONS FOR RESERVES -----------------------------------------

#EPV - Pure Endowment - X
reserve_x_pe <- function(age,contract_term,i){
  vn_pe <- discount_factor(i,contract_term)
  npmainx_pe <- survival_x(age, contract_term)
  a=vn_pe*npmainx_pe
  return(a)
  
}

#EPV - Pure Endowment - Y
reserve_y_pe <- function(age,contract_term,i){
  vn_pe <- discount_factor(i,contract_term)
  npmainy_pe <- survival_y(age, contract_term)
  a=vn_pe*npmainy_pe
  return(a)
  
}

#EPV - Term Assurance - X
reserve_x_ta <- function(age,contract_term,i){
  A_xn_ta <- assurance_x((age+contract_term), i)
  A_x <- assurance_x(age,i)
  vn_ta <- discount_factor(i,contract_term)
  npmainx_ta <- survival_x(age, contract_term)
  a=A_x-vn_ta*npmainx_ta*A_xn_ta
  return(a)
  
}

#EPV - Term Assurance - Y
reserve_y_ta <- function(age,contract_term,i){
  A_yn_ta <- assurance_y(age+contract_term, i)
  A_y <- assurance_y(age,i)
  vn_ta <- discount_factor(i,contract_term)
  npmainy_ta <- survival_y(age, contract_term)
  a=A_y-vn_ta*npmainy_ta*A_yn_ta
  return(a)
  
}

#EPV - Endowment Assurance - X;EOY
reserve_x_ea_eoy <- function(age, contract_term, i){
  A_xn_ea <- assurance_x(age+contract_term, i)
  A_x <- assurance_x(age,i)
  vn_ea <- discount_factor(i, contract_term)
  npmainx_ea <- survival_x(age, contract_term)
  a=A_x-vn_ea*npmainx_ea*A_xn_ea+vn_ea*npmainx_ea
  return(a)
}

#EPV - Endowment Assurance - X:IUD
reserve_x_ea_iud <- function(age, contract_term, i){
  A_xn_ea <- assurance_x(age+contract_term, i)
  A_x <- assurance_x(age,i)
  vn_ea <- discount_factor(i, contract_term)
  npmainx_ea <- survival_x(age, contract_term)
  a=(sqrt(1+i))*A_x-vn_ea*npmainx_ea*A_xn_ea+vn_ea*npmainx_ea
  return(a)
}

#EPV - Endowment Assurance - Y;EOY
reserve_y_ea_eoy <- function(age, contract_term, i){
  A_yn_ea <- assurance_x(age+contract_term, i)
  A_y <- assurance_y(age,i)
  vn_ea <- discount_factor(i, contract_term)
  npmainy_ea <- survival_x(age, contract_term)
  a=A_y-vn_ea*npmainy_ea*A_yn_ea+vn_ea*npmainy_ea
  return(a)
}

#EPV - Endowment Assurance - Y;IUD
reserve_y_ea_iud <- function(age, contract_term, i){
  A_yn_ea <- assurance_x(age+contract_term, i)
  A_y <- assurance_y(age,i)
  vn_ea <- discount_factor(i, contract_term)
  npmainy_ea <- survival_x(age, contract_term)
  a=(sqrt(1+i))*A_y-vn_ea*npmainy_ea*A_yn_ea+vn_ea*npmainy_ea
  return(a)
}

# Whole life annuity 
m_axm_r<-function(age,i,m){return(m*(annuity_due_x(age,i)-1+(m-1)/(2*m)))} #m*ax monthly 
m_aym_r<-function(age,i,m){return(m*(annuity_due_y(age,i)-1+(m-1)/(2*m)))} #m*ay monthly 
m_aduexm_r<-function(age,i,m){return(m*(annuity_due_x(age,i)-((m-1)/(2*m))))} #m*aduex monthly
m_adueym_r<-function(age,i,m){return(m*(annuity_due_y(age,i)-((m-1)/(2*m))))} #m*aduey monthly
m_aduexm_rnt<-function(age,contract_term,i,m){return(m*(annuity_due_xnt(age,contract_term,i)-((m-1)/(2*m))))} #m*aduex monthly
m_adueym_rnt<-function(age,contract_term,i,m){return(m*(annuity_due_ynt(age,contract_term,i)-((m-1)/(2*m))))} #m*aduey monthly

# Term annuity
ax_r<-function(age,i){return(annuity_due_x(age,i)-1)} #ax
ay_r<-function(age,i){return(annuity_due_y(age,i)-1)} #ay

m_ax_nbar_r<-function(age,i,contract_term,m){
  vn_tan_r<-discount_factor(i, contract_term)
  npmainx_tan_r<-survival_x(age, contract_term)
  return(m*(ax_r(age,i)-vn_tan_r*npmainx_tan_r*ax_r((age+contract_term),i)
            -((m-1)/(2*m))*(1-vn_tan_r*npmainx_tan_r)))} #m*ax:nbar monthly
m_ay_nbar_r<-function(age,i,contract_term,m){
  vn_tan_r<-discount_factor(i, contract_term)
  npmainy_tan_r<-survival_y(age, contract_term)
  return(m*(ay_r(age,i)-vn_tan_r*npmainy_tan_r*ay_r((age+contract_term),i)
            -((m-1)/(2*m))*(1-vn_tan_r*npmainy_tan_r)))} #m*ay:nbar monthly

m_aduex_nbar_r<-function(age,i,contract_term,m){
  vn_tan_r<-discount_factor(i, contract_term)
  npmainx_tan_r<-survival_x(age, contract_term)
  return(m*(annuity_due_x(age,i)-vn_tan_r*npmainx_tan_r*annuity_due_x((age+contract_term),i)
            -((m-1)/(2*m))*(1-vn_tan_r*npmainx_tan_r)))} #m*aduex:nbar monthly
m_aduey_nbar_r<-function(age,i,contract_term,m){
  vn_tan_r<-discount_factor(i, contract_term)
  npmainy_tan_r<-survival_y(age, contract_term)
  return(m*(annuity_due_y(age,i)-vn_tan_r*npmainy_tan_r*annuity_due_y((age+contract_term),i)
            -((m-1)/(2*m))*(1-vn_tan_r*npmainy_tan_r)))} #m*aduey:nbar monthly

# Guaranteed whole life annuity
m_ax_nbarbar_r<-function(age,i,contract_term,m){
  vn_gwlan_r<-discount_factor(i, contract_term)
  npmainx_gwlan_r<-survival_x(age, contract_term)
  return(m*((1/(m*((1+i)^(1/m)-1)))*((1-vn_gwlan_r)/i)+vn_gwlan_r*npmainx_gwlan_r*
              (ax_r((age+contract_term),i)+((m-1)/(2*m)))))} #m*ax:nbarbar monthly

#m*ay:nbarbar monthly
m_ay_nbarbar_r<-function(age,i,contract_term,m){
  vn_gwlan_r<-discount_factor(i, contract_term)
  npmainy_gwlan_r<-survival_y(age, contract_term)
  return(m*((1/(m*((1+i)^(1/m)-1)))*((1-vn_gwlan_r)/i)+vn_gwlan_r*npmainy_gwlan_r*
              (ay_r((age+contract_term),i)+((m-1)/(2*m)))))} 


#----------------------------------------------------------------------SHINY COMPONENT INPUTS-----------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(shiny)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme=shinytheme("superhero"),
  
  # App title ----
  titlePanel("Pricing Tool"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      #interest rate slider
      sliderInput("interest_rate","Interest Rate",min=0,max=0.50,value=0.05,step=0.01),
      
      #Input: selection if couple
      radioButtons("joint", "Policy Type",
                   choices = list("Single" = 1, "Couple" = 2), selected = character(0)),
      
      conditionalPanel(
        condition = "input.joint=='1'",
        #group for mortality
        radioButtons("group", "Policyholder's Mortality Group",
                     choices = list("x" = 1, "y" = 2), selected = character(0))),
      
      
      
      
      # Input: input area for age
      numericInput("main_age","Policyholder's Age",20,min=20,max=100),
      
      #show this if choice is couple
      conditionalPanel(
        condition = "input.joint=='2'",
        numericInput("partner_age","Age of Partner",20,min=20,max=100)
      ),
      
      
      #type of insurance they want (if single)
      #show options for single life
      conditionalPanel(
        condition = "input.joint=='1'",
        
        radioButtons("contract_type", "Contract Type",choices =
                       list("Assurances"=1,"Annuity"=2),selected = character(0))),
      #show if selected assurances    
      conditionalPanel(
        condition = "input.contract_type=='1'&&input.joint=='1'",
        selectInput("type_of_assurance","Type of Assurance",
                    choices = 
                      list("Pure Endowment" = "pure endowment", 
                           "Term Assurance" = "term assurance",
                           "Endowment Assurance"="endowment assurance",
                           "Whole Life Assurance"="whole life assurance"))),
      
      #---------------------------------------------------------------SINGLE LIFE ASSURANCES----------------------------------------------------------------------------------      
      
      
      #payment timing and term  for assurances
      #for pure endowment
      conditionalPanel(
        condition = "input.type_of_assurance=='pure endowment'&& input.contract_type=='1'&&input.joint=='1'  ",
        #term selection
        sliderInput("contract_term_pe", "Contract Term",1,80,40,1),
        #sum assured
        numericInput("survival_benefit_pe","Survival Benefit",10000)),
      
      #for term assurance
      conditionalPanel(
        condition = "input.type_of_assurance=='term assurance'&& input.contract_type=='1'&&input.joint=='1'",
        radioButtons("immediate_or_end_of_the_year_ta", "Payment Time",
                     choices = list("Immediately Upon Death" = 1, "End of Year of Death" = 2),selected = character(0)),
        #term selection
        sliderInput("contract_term_ta", "Contract Term",1,80,40,1),
        #sum assured
        numericInput("death_benefit_ta","Death Benefit",10000)),
      
      #for endowment assurance
      conditionalPanel(
        condition = "input.type_of_assurance=='endowment assurance'&& input.contract_type=='1'&&input.joint=='1'",
        radioButtons("immediate_or_end_of_the_year_ea", "Payment Time",
                     choices = list("Immediately Upon Death" = 1, "End of Year of Death" = 2),selected = character(0)),
        #term selection
        sliderInput("contract_term_ea", "Contract Term",1,80,40,1),
        #sum assured
        numericInput("death_benefit_ea","Death Benefit",10000),
        numericInput("survival_benefit_ea","Survival Benefit",10000,step=5000)),
      
      #for whole life assurance
      conditionalPanel(
        condition = "input.type_of_assurance=='whole life assurance'&& input.contract_type=='1'&&input.joint=='1'",
        radioButtons("immediate_or_end_of_the_year_wla", "Payment Time",
                     choices = list("Immediately Upon Death" = 1, "End of Year of Death" = 2),selected = character(0)),
        #sum assured
        numericInput("death_benefit_wla","Death Benefit",10000)),
      
      #--------------------------------------------------------------sINGLE LIFE ANNUITIES---------------------------------------------------------------------------------      
      #show if selected annuity
      conditionalPanel(
        condition = "input.contract_type=='2'&&input.joint=='1'", 
        selectInput("annuity","Type of Annuity",
                    choices = 
                      list("Whole Life Annuity Paid in Arrears"="whole life annuity paid in arrears",
                           "Term Annuity Paid in Arrears"="term annuity paid in arrears",
                           "Guaranteed Whole Life Annuity Paid in Arrears"="guaranteed whole life annuity paid in arrears"))),
      
      #payment frequency and term for annuities
      
      #for whole life annuity paid in arrears
      conditionalPanel(
        condition = "input.contract_type=='2'&&input.annuity=='whole life annuity paid in arrears'&&input.joint=='1'",
        
        radioButtons("frequency_of_annuity_payments_wlan", "Frequency of Payment",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0)),
        #annuity amount
        numericInput("annuity_payment_wlan","Payment Amount",1000)),
      
      #for term annuity paid in arrears
      conditionalPanel(
        condition = "input.contract_type=='2'&&input.annuity=='term annuity paid in arrears'&&input.joint=='1'",
        
        radioButtons("frequency_of_annuity_payments_tan", "Frequency of Payment",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0)),
        #term selection
        sliderInput("contract_term_tan", "Contract Term",1,80,40,1),
        #annuity ammount
        numericInput("annuity_payment_tan","Payment Amount",1000)),
      
      
      #for guaranteed whole life annuity paid in arrears
      conditionalPanel(
        condition = "input.contract_type=='2'&& input.annuity=='guaranteed whole life annuity paid in arrears' && input.joint=='1'",
        
        radioButtons("frequency_of_annuity_payments_gwlan", "Frequency of Payment",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0)),
        #annuity amount
        numericInput("annuity_payment_gwlan","Payment Amount",1000),
        
        
        #guarantee period for guarantee whole life annuity
        sliderInput("guarantee_payments_gwlan","Number of Guaranteed Payments (years)",0,100,50,1)),
      
      
      #------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
      
      
      
      
      #type of joint policy they want (if couple)
      
      conditionalPanel(
        condition = "input.joint=='2'",
        
        radioButtons("contract_type_joint", "Contract Type",choices =
                       list("Joint Assurance"=1,"Joint Annuity"=2),selected = character(0))),
      
      
      #--------------------------------------------------------------------JOINT LIFE ASSURANCES-----------------------------------------------------------------------------------      
      #for joint assurance  
      conditionalPanel(
        condition = "input.contract_type_joint=='1'&&input.joint=='2' ",
        selectInput("type_of_joint_assurance","Type of Joint Assurance",
                    choices = 
                      list("Joint Whole Life Assurance" = "joint whole life assurance", 
                           "Joint Term Life Assurance" = "joint term life assurance"))),
      
      #for joint annuity
      conditionalPanel(
        condition = "input.contract_type_joint=='2'&&input.joint=='2'", 
        selectInput("type_of_joint_annuity","Type of Joint Annuity",
                    choices = 
                      list("Joint Whole Life Annuity" = "joint whole life annuity", 
                           "Joint Term Life Annuity" = "joint term life annuity"))),
      
      #payment timing and term for joint assurances
      #for joint whole life assurance
      conditionalPanel(
        condition = "input.contract_type_joint=='1'&&input.joint=='2'&& input.type_of_joint_assurance=='joint whole life assurance'",
        radioButtons("immediate_or_end_of_the_year_jwla", "Payment Time",
                     choices = list("Immediately Upon First Death" = 1, "End of Year of First Death" = 2),selected = character(0)),
        #sum assured
        numericInput("death_benefit_jwla","Death Benefit",10000)),
      
      #for joint term life assurance
      conditionalPanel(
        condition = "input.contract_type_joint=='1'&&input.joint=='2'&& input.type_of_joint_assurance=='joint term life assurance'",
        radioButtons("immediate_or_end_of_the_year_jta", "Payment Time",
                     choices = list("Immediately Upon First Death" = 1, "End of Year of First Death" = 2),selected = character(0)),
        #term selection
        sliderInput("contract_term_jta", "Contract Term",1,80,40,1),
        #sum assured
        numericInput("death_benefit_jta","Death Benefit",10000)),
      
      #-------------------------------------------------------------JOINT LIFE ANNUITIES------------------------------------------------------------------------------------------      
      
      #payment frequency and term for joint annuities
      #for joint whole life annuity
      conditionalPanel(
        condition = "input.contract_type_joint=='2'&&input.joint=='2'&& input.type_of_joint_annuity=='joint whole life annuity'",
        
        radioButtons("frequency_of_annuity_payments_jwlan", "Frequency of Payment",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0)),
        #annuity amount
        numericInput("annuity_payment_jwlan","Payment Amount",1000)),
      
      #for joint term life annuity
      conditionalPanel(
        condition = "input.contract_type_joint=='2'&&input.joint=='2'&& input.type_of_joint_annuity=='joint term life annuity'",
        
        radioButtons("frequency_of_annuity_payments_jtan", "Frequency of Payment",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0)),
        #term selection
        sliderInput("contract_term_jtan", "Contract Term",min=1,max=100,value=50,step=1),
        #annuity amount
        numericInput("annuity_payment_jtan","Payment Amount",1000)),
      
      
      
      #-------------------------------------------------------------EXPENSES AND PREMIUM PAYMENT---------------------------------------------------------------------------------------------      
      
      #expenses
      #remember to divide by 100 for percentage when using in calculations
      
      
      numericInput("initial_expense", "Initial Expense (% of Gross Premium)", value=0,min=0,max=100),
      
      numericInput("premium_expense", "Premium Expense (% of Gross Premium, Excluding first premium)", value=0,min=0,max=100),  
      
      numericInput("claim_expense", "Claim Expense (% of Benefit Amount)", value=0,min=0,max=100),  
      
      #premium payment
      
      radioButtons("premium_structure", "Premium Structure",
                   choices = list("Single Premium" = 1, "Level Premiums" = 2),selected = character(0)),
      
      conditionalPanel(
        condition = "input.premium_structure=='2'",
        radioButtons("frequency_of_premium_payments", "Frequency of Premium Payments",
                     choices = list("Monthly" = 1, "Yearly" = 2),selected = character(0))),
      
      
    ),
    
    
    
    
    
    #----------------------------------------------------------------SHINY COMPONENT OUTPUTS----------------------------------------------------------------------------------------------------- 
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
    # Main panel for displaying outputs ----
    mainPanel(
      
      #if functions such that if certain things are selected, then display the output so that only one is displayed
      #textoutput for premium
      #plotoutput for graph of reserve
      
      #---------------------------------------------------------------SINGLE LIFE ASSURANCES----------------------------------------------------------------------------------------------------------
      #Single X Assurance - Single Premium
      conditionalPanel(
        condition = "input.joint=='1' && input.group=='1' && input.contract_type=='1' && input.premium_structure=='1'",
        
        #Single X Assurance - Pure Endowment
        conditionalPanel(
          condition = "input.type_of_assurance=='pure endowment'",
          plotOutput("pe_x_sp_plot")
        ),
        
        #Single X Assurance - Term Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance' && input.immediate_or_end_of_the_year_ta=='1'",
          plotOutput("ta_x_sp_iud_plot")
        ),
        
        #Single X Assurance - Term Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance' && input.immediate_or_end_of_the_year_ta=='2'",
          plotOutput("ta_x_sp_eoy_plot")
        ), 
        
        #Single X Assurance - Endowment Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance' && input.immediate_or_end_of_the_year_ea=='1'",
          plotOutput("ea_x_sp_iud_plot")
        ), 
        
        #Single X Assurance - Endowment Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance' && input.immediate_or_end_of_the_year_ea=='2'",
          plotOutput("ea_x_sp_eoy_plot")
        ), 
        
        #Single X Assurance - Whole Life Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance' && input.immediate_or_end_of_the_year_wla=='1'",
          plotOutput("wla_x_sp_iud_plot")
        ), 
        
        #Single X Assurance - Whole Life Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance' && input.immediate_or_end_of_the_year_wla=='2'",
          plotOutput("wla_x_sp_eoy_plot")
        ), 
      ),
      
      
      #Single Y Assurance - Single Premium
      conditionalPanel(
        condition = "input.joint=='1' && input.group=='2' && input.contract_type=='1' && input.premium_structure=='1'",
        
        #Single Y Assurance - Pure Endowment
        conditionalPanel(
          condition = "input.type_of_assurance=='pure endowment'",
          plotOutput("pe_y_sp_plot")
        ),
        
        #Single Y Assurance - Term Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance' && input.immediate_or_end_of_the_year_ta=='1'",
          plotOutput("ta_y_sp_iud_plot")
        ),
        
        #Single Y Assurance - Term Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance' && input.immediate_or_end_of_the_year_ta=='2'",
          plotOutput("ta_y_sp_eoy_plot"),
        ), 
        
        #Single Y Assurance - Endowment Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance' && input.immediate_or_end_of_the_year_ea=='1'",
          plotOutput("ea_y_sp_iud_plot")
        ), 
        
        #Single Y Assurance - Endowment Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance' && input.immediate_or_end_of_the_year_ea=='2'",
          plotOutput("ea_y_sp_eoy_plot")
        ), 
        
        #Single Y Assurance - Whole Life Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance' && input.immediate_or_end_of_the_year_wla=='1'",
          plotOutput("wla_y_sp_iud_plot")
        ), 
        
        #Single Y Assurance - Whole Life Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance' && input.immediate_or_end_of_the_year_wla=='2'",
          plotOutput("wla_y_sp_eoy_plot")
        ), 
      ),
      
      #Single X Assurance - Level Premium
      conditionalPanel(
        condition = "input.joint=='1' && input.group=='1' && input.contract_type=='1' && input.premium_structure=='2'",
        
        #Single X Assurance - Pure Endowment
        conditionalPanel(
          condition = "input.type_of_assurance=='pure endowment' && input.frequency_of_premium_payments >= '1'",
          plotOutput("pe_x_lp_plot")
        ),
        
        #Single X Assurance - Term Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ta=='1'",
          plotOutput("ta_x_lp_iud_plot")
          
        ),
        
        #Single X Assurance - Term Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ta=='2'",
          plotOutput("ta_x_lp_eoy_plot")
        ), 
        
        #Single X Assurance - Endowment Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ea=='1'",
          plotOutput("ea_x_lp_iud_plot")
        ), 
        
        #Single X Assurance - Endowment Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ea=='2'",
          plotOutput("ea_x_lp_eoy_plot")
        ), 
        
        #Single X Assurance - Whole Life Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_wla=='1'",
          plotOutput("wla_x_lp_iud_plot")
        ), 
        
        #Single X Assurance - Whole Life Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_wla=='2'",
          plotOutput("wla_x_lp_eoy_plot")
        ), 
      ),
      
      #Single Y Assurance - Level Premium
      conditionalPanel(
        condition = "input.joint=='1' && input.group=='2' && input.contract_type=='1' && input.premium_structure=='2'",
        
        #Single Y Assurance - Pure Endowment
        conditionalPanel(
          condition = "input.type_of_assurance=='pure endowment' && input.frequency_of_premium_payments >= '1'",
          plotOutput("pe_y_lp_plot")
        ),
        
        #Single Y Assurance - Term Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ta=='1'",
          plotOutput("ta_y_lp_iud_plot")
        ),
        
        #Single Y Assurance - Term Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='term assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ta=='2'",
          plotOutput("ta_y_lp_eoy_plot")
        ), 
        
        #Single Y Assurance - Endowment Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ea=='1'",
          plotOutput("ea_y_lp_iud_plot")
        ), 
        
        #Single Y Assurance - Endowment Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='endowment assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_ea=='2'",
          plotOutput("ea_y_lp_eoy_plot")
        ), 
        
        #Single Y Assurance - Whole Life Assurance; IUD
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_wla=='1'",
          plotOutput("wla_y_lp_iud_plot")
        ), 
        
        #Single Y Assurance - Whole Life Assurance; EOY
        conditionalPanel(
          condition = "input.type_of_assurance=='whole life assurance'  && input.frequency_of_premium_payments >= '1' && input.immediate_or_end_of_the_year_wla=='2'",
          plotOutput("wla_y_lp_eoy_plot")
        ), 
      ),
      
      #----------------------------------------------------------------SINGLE LIFE ANNUITIES----------------------------------------------------------------------------------------------------------- 
      
      
      # CONDITIONAL PANEL: WHOLE LIFE ANNUITY PAID IN ARREARS FOR A SINGLE PERSON
      conditionalPanel(
        condition = "input.joint=='1'&& input.contract_type=='2' && input.annuity=='whole life annuity paid in arrears'",
        
        # Single premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_wlan=='1'||input.frequency_of_annuity_payments_wlan=='2')", 
          
          plotOutput("wlan_x_sp_plot")
        ),
        
        # Single premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_wlan=='1'||input.frequency_of_annuity_payments_wlan=='2')", 
          
          plotOutput("wlan_y_sp_plot")
        ),
        
        # Level premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_wlan=='1'||input.frequency_of_annuity_payments_wlan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("wlan_x_lp_plot")
        ),
        
        # Level premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_wlan=='1'||input.frequency_of_annuity_payments_wlan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("wlan_y_lp_plot")
        ),
      ),
      
      # CONDITIONAL PANEL: TERM ANNUITY PAID IN ARREARS FOR A SINGLE PERSON
      conditionalPanel(
        condition = "input.joint=='1'&& input.contract_type=='2' && input.annuity=='term annuity paid in arrears'",
        
        # Single premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_tan=='1'||input.frequency_of_annuity_payments_tan=='2')", 
          
          plotOutput("tan_x_sp_plot")
        ),
        
        # Single premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_tan=='1'||input.frequency_of_annuity_payments_tan=='2')", 
          
          plotOutput("tan_y_sp_plot")
        ),
        
        # Level premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_tan=='1'||input.frequency_of_annuity_payments_tan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("tan_x_lp_plot")
        ),
        
        # Level premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_tan=='1'||input.frequency_of_annuity_payments_tan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("tan_y_lp_plot")
        ),
      ),
      
      # CONDITIONAL PANEL: GUARANTEED WHOLE LIFE ANNUITY PAID IN ARREARS FOR A SINGLE PERSON
      conditionalPanel(
        condition = "input.joint=='1'&& input.contract_type=='2' && input.annuity=='guaranteed whole life annuity paid in arrears'",
        
        # Single premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_gwlan=='1'||input.frequency_of_annuity_payments_gwlan=='2')", 
          
          plotOutput("gwlan_x_sp_plot")
        ),
        
        # Single premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='1' && (input.frequency_of_annuity_payments_gwlan=='1'||input.frequency_of_annuity_payments_gwlan=='2')", 
          
          plotOutput("gwlan_y_sp_plot")
        ),
        
        # Level premium: x
        conditionalPanel(
          condition = "input.group=='1' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_gwlan=='1'||input.frequency_of_annuity_payments_gwlan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("gwlan_x_lp_plot")
        ),
        # Level premium: y
        conditionalPanel(
          condition = "input.group=='2' && input.premium_structure=='2' && (input.frequency_of_annuity_payments_gwlan=='1'||input.frequency_of_annuity_payments_gwlan=='2') && (input.frequency_of_premium_payments=='1'||input.frequency_of_premium_payments=='2')", 
          
          plotOutput("gwlan_y_lp_plot")
        ),
      ),
      
      
      
      #--------------------------------------------------------------JOINT LIFE ASSURANCES---------------------------------------------------------------------------------------------------------------
      #conditional panel for single premium joint assurances   
      
      conditionalPanel(
        condition = "input.premium_structure=='1' && input.joint=='2'&& input.contract_type_joint=='1'",
        
        #joint whole life immediate 
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='1'",
          plotOutput("jwla_sp_i_plot")
        ),
        
        
        #joint whole life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='2'",
          plotOutput("jwla_sp_end_plot")
        ),
        
        
        #joint term life immediate 
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='1'",
          plotOutput("jta_sp_i_plot")
        ),
        
        
        #joint term life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='2'",
          plotOutput("jta_sp_end_plot")
        )
        
        
        
      ), 
      
      
      #conditional panel for level premium joint assurance paid annually, m=1
      
      conditionalPanel(
        condition = "input.premium_structure=='2'&&input.frequency_of_premium_payments=='2' && input.joint=='2'&& input.contract_type_joint=='1'",
        
        #joint whole life immediate 
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='1'",
          plotOutput("jwla_yp_i_plot")
          
        ),
        
        
        #joint whole life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='2'",
          plotOutput("jwla_yp_end_plot")
        ),
        
        
        #joint term life immediate 
        
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='1'",
          plotOutput("jta_yp_i_plot")
        ),
        
        
        #joint term life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='2'",
          plotOutput("jta_yp_end_plot")
        )
        
      ),
      
      #conditional panel for level premium joint assurance paid monthly, m=12
      
      
      conditionalPanel(
        condition = "input.premium_structure=='2'&&input.frequency_of_premium_payments=='1'&& input.joint=='2'&& input.contract_type_joint=='1'",
        
        #joint whole life immediate 
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='1'",
          plotOutput("jwla_mp_i_plot")
        ),
        
        
        #joint whole life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint whole life assurance' && input.immediate_or_end_of_the_year_jwla=='2'",
          plotOutput("jwla_mp_end_plot")
        ),
        
        
        #joint term life immediate 
        
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='1'",
          plotOutput("jta_mp_i_plot")
        ),
        
        
        #joint term life first death
        conditionalPanel(
          condition = "input.type_of_joint_assurance=='joint term life assurance' && input.immediate_or_end_of_the_year_jta=='2'",
          plotOutput("jta_mp_end_plot")
        )
        
        
        
      ),  
      
      #-----------------------------------------------------------------------JOINT LIFE ANNUITIES------------------------------------------------------------------------------------------------------    
      
      #conditional panel for single premium joint annuities  
      
      conditionalPanel(
        condition = "input.premium_structure=='1' && input.joint=='2'&& input.contract_type_joint=='2'",
        
        #joint whole life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='1'",
          plotOutput("jwlan_sp_m_plot")
        ),
        
        
        #joint whole life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='2'",
          plotOutput("jwlan_sp_y_plot")
        ),
        
        
        #joint term life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='1'",
          plotOutput("jtan_sp_m_plot")
        ),
        
        
        #joint term life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='2' ",
          plotOutput("jtan_sp_y_plot")
        )
        
        
      ), 
      
      
      #conditional panel for level premium joint annuities paid annually, m=1
      
      conditionalPanel(
        condition = "input.premium_structure=='2'&&input.frequency_of_premium_payments=='2' && input.joint=='2'&& input.contract_type_joint=='2'",
        
        #joint whole life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='1'",
          plotOutput("jwlan_yp_m_plot")
          
        ),
        
        
        #joint whole life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='2'",
          plotOutput("jwlan_yp_y_plot")
        ),
        
        
        #joint term life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='1'",
          plotOutput("jtan_yp_m_plot")
        ),
        
        
        #joint term life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='2'",
          plotOutput("jtan_yp_y_plot")
        )
        
      ),
      
      #conditional panel for level premium joint annuities paid monthly, m=12
      
      conditionalPanel(
        condition = "input.premium_structure=='2'&&input.frequency_of_premium_payments=='1'&& input.joint=='2'&& input.contract_type_joint=='2'",
        
        #joint whole life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='1'",
          plotOutput("jwlan_mp_m_plot")
        ),
        
        
        #joint whole life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint whole life annuity' && input.frequency_of_annuity_payments_jwlan=='2'",
          plotOutput("jwlan_mp_y_plot")
          
        ),
        
        
        #joint term life annuity payable monthly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='1'",
          plotOutput("jtan_mp_m_plot")
        ),
        
        
        #joint term life annuity payable yearly
        conditionalPanel(
          condition = "input.type_of_joint_annuity=='joint term life annuity' && input.frequency_of_annuity_payments_jtan=='2'",
          plotOutput("jtan_mp_y_plot")
        )
        
        
      ),    
      
      #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
      #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------         
      
    ) #this is the bracket for main panel end
    
    
  ) #sidebar layout end
  
) #fluid page end

#----------------------------------------------------------------------SHINY SERVER--------------------------------------------------------------------------------------------------------- 
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    


# Define server logic for calculating premium and reserves
server <- function(input, output) {
  
  #-------------------------------------------------------------------CONVENIENT VALUES---------------------------------------------------------------------------------------------------------------    
  
  
  adue_x<-reactive({annuity_due_x(input$main_age,input$interest_rate)}) #this is adue_x in mortality table
  A_x<-reactive({assurance_x(input$main_age,input$interest_rate)}) #this is A_x in mortality table
  
  
  adue_y<-reactive({annuity_due_y(input$main_age,input$interest_rate)}) #this is adue_y
  A_y<-reactive({assurance_y(input$main_age,input$interest_rate)}) #this is A_y
  
  
  
  #rendertext for premium
  #renderplot for reserve
  
  #example for pure endowment calculations
  #x is main, single premium
  vn<-reactive({discount_factor(input$interest_rate,input$contract_term_pe)})
  
  npmainx<-reactive({survival_x(input$main_age,input$contract_term_pe)}) #this is npx 
  
  output$pe_x_sp<-renderText({((as.numeric(input$survival_benefit_pe))*
                                 (vn())*
                                 (npmainx())*
                                 (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))
  })
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------PREMIUM CALCULATIONS---------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
  
  #----------------------------------------------------------------SINGLE LIFE ASSURANCES---------------------------------------------------------------------------------------------------------------
  #some convenient values 
  
  
  adue_x<-reactive({annuity_due_x(input$main_age,input$interest_rate)}) #this is adue_x in mortality table
  A_x<-reactive({assurance_x(input$main_age,input$interest_rate)}) #this is A_x in mortality table
  
  adue_y<-reactive({annuity_due_y(input$main_age,input$interest_rate)}) #this is adue_y
  A_y<-reactive({assurance_y(input$main_age,input$interest_rate)}) #this is A_y
  
  frequency_of_premium_payment <- reactive({input$frequency_of_premium_payments})
  frequency_of_premium_payments <- reactive({if (frequency_of_premium_payment() == "1"){12} else {1}})
  
  #rendertext for premium
  #renderplot for reserve
  
  
  #PURE ENDOWMENT 
  vn_pe<-reactive({discount_factor(input$interest_rate,input$contract_term_pe)})
  
  npmainx_pe<-reactive({survival_x(input$main_age,input$contract_term_pe)}) #this is npx of group x
  npmainy_pe<-reactive({survival_y(input$main_age,input$contract_term_pe)}) #this is npx of group y
  
  adue_xn_pe<-reactive({annuity_due_x((input$main_age+input$contract_term_pe),input$interest_rate)}) #this is adue_x:n in mortality table
  adue_yn_pe<-reactive({annuity_due_y((input$main_age+input$contract_term_pe),input$interest_rate)}) #this is adue_y:n in mortality table
  A_xn_pe <- reactive({(assurance_x((input$main_age+input$contract_term_pe),input$interest_rate))}) #this is A_(x+n)
  A_yn_pe <- reactive({(assurance_y((input$main_age+input$contract_term_pe),input$interest_rate))}) #this is A_(x+n)
  
  termadue_x_lp_pe <- reactive({(((adue_x()-vn_pe()*npmainx_pe()*adue_xn_pe())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_pe()*npmainx_pe())))})#this is [a(m)x:n] term annuity due with mthly payments of group x
  
  termadue_y_lp_pe <- reactive({(((adue_y()-vn_pe()*npmainy_pe()*adue_yn_pe())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_pe()*npmainy_pe())))})#this is [a(m)x:n] term annuity due with mthly payments of group y
  
  #PURE ENDOWMENT - X:SINGLE
  pe_x_sp<-reactive({((input$survival_benefit_pe)*
                        (vn_pe())*
                        (npmainx_pe())*
                        (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))})
  
  #PURE ENDOWMENT - Y:SINGLE
  pe_y_sp<-reactive({((input$survival_benefit_pe)*
                        (vn_pe())*
                        (npmainy_pe())*
                        (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))})
  
  #PURE ENDOWMENT - X:LEVEL
  pe_x_lp<-reactive({((input$survival_benefit_pe)*
                        (vn_pe())*
                        (npmainx_pe())*
                        (1+(input$claim_expense/100)))/
      (frequency_of_premium_payments()*termadue_x_lp_pe()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_x_lp_pe()-(1/frequency_of_premium_payments())))})
  
  #PURE ENDOWMENT - Y:LEVEL                    
  pe_y_lp<-reactive({((input$survival_benefit_pe)*
                        (vn_pe())*
                        (npmainy_pe())*
                        (1+(input$claim_expense/100)))/
      (frequency_of_premium_payments()*termadue_y_lp_pe()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_y_lp_pe()-(1/frequency_of_premium_payments())))})
  
  #TERM ASSURANCE
  vn_ta<-reactive({discount_factor(input$interest_rate,input$contract_term_ta)})
  
  npmainx_ta<-reactive({survival_x(input$main_age,input$contract_term_ta)}) #this is npx of group x
  npmainy_ta<-reactive({survival_y(input$main_age,input$contract_term_ta)}) #this is npx of group y
  
  adue_xn_ta<-reactive({annuity_due_x((input$main_age+input$contract_term_ta),input$interest_rate)}) #this is adue_x:n in mortality table
  adue_yn_ta<-reactive({annuity_due_y((input$main_age+input$contract_term_ta),input$interest_rate)}) #this is adue_y:n in mortality table
  A_xn_ta <- reactive({(assurance_x((input$main_age+input$contract_term_ta),input$interest_rate))}) #this is A_(x+n)
  A_yn_ta <- reactive({(assurance_y((input$main_age+input$contract_term_ta),input$interest_rate))}) #this is A_(x+n)
  
  termadue_x_lp_ta <- reactive({(((adue_x()-vn_ta()*npmainx_ta()*adue_xn_ta())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_ta()*npmainx_ta())))})#this is [a(m)x:n] term annuity due with mthly payments of group x
  
  termadue_y_lp_ta <- reactive({(((adue_y()-vn_ta()*npmainy_ta()*adue_yn_ta())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_ta()*npmainy_ta())))})#this is [a(m)x:n] term annuity due with mthly payments of group y
  
  A_xn_ta_eoy <- reactive({(A_x()-vn_ta()*npmainx_ta()*A_xn_ta())})
  A_yn_ta_eoy <- reactive({(A_y()-vn_ta()*npmainy_ta()*A_yn_ta())})
  
  #TERM ASSURANCE - X:SINGLE
  #Immediately Upon Death
  ta_x_sp_iud <- reactive({(input$death_benefit_ta*
                              (sqrt(1+input$interest_rate)*A_xn_ta_eoy())*
                              (1+input$claim_expense/100))/
      (1-input$initial_expense/100)})
  
  #End Of Year
  ta_x_sp_eoy<-reactive({(input$death_benefit_ta*
                            (A_xn_ta_eoy())*
                            (1+input$claim_expense/100))/
      (1-input$initial_expense/100)})
  
  #TERM ASSURANCE - Y:SINGLE
  #Immediately Upon Death
  ta_y_sp_iud<-reactive({(input$death_benefit_ta*
                            (sqrt(1+input$interest_rate)*A_yn_ta_eoy())*
                            (1+input$claim_expense/100))/
      (1-input$initial_expense/100)}) 
  
  #End Of Year
  ta_y_sp_eoy<-reactive({(input$death_benefit_ta*
                            (A_yn_ta_eoy())*
                            (1+input$claim_expense/100))/
      (1-input$initial_expense/100)}) 
  
  #TERM ASSURANCE - X:LEVEL
  #Immediately Upon Death
  ta_x_lp_iud<-reactive({((1+input$claim_expense/100)*input$death_benefit_ta*
                            (sqrt(1+input$interest_rate)*A_xn_ta_eoy()))/
      (frequency_of_premium_payments()*termadue_x_lp_ta()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_x_lp_ta()-(1/frequency_of_premium_payments())))})
  
  #End Of Year
  ta_x_lp_eoy<-reactive({((1+input$claim_expense/100)*input$death_benefit_ta*
                            (A_xn_ta_eoy()))/
      (frequency_of_premium_payments()*termadue_x_lp_ta()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_x_lp_ta()-(1/frequency_of_premium_payments())))})
  
  #TERM ASSURANCE - Y:LEVEL   
  #Immediately Upon Death
  ta_y_lp_iud<-reactive({((1+input$claim_expense/100)*input$death_benefit_ta*
                            (sqrt(1+input$interest_rate)*A_yn_ta_eoy()))/
      (frequency_of_premium_payments()*termadue_y_lp_ta()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_y_lp_ta()-(1/frequency_of_premium_payments())))})
  #End Of Year
  ta_y_lp_eoy<-reactive({((1+input$claim_expense/100)*input$death_benefit_ta*
                            (A_yn_ta_eoy()))/
      (frequency_of_premium_payments()*termadue_y_lp_ta()-
         input$initial_expense/100-
         input$premium_expense/100*
         (frequency_of_premium_payments()*termadue_y_lp_ta()-(1/frequency_of_premium_payments())))})
  
  #ENDOWMENT ASSURANCE
  vn_ea<-reactive({discount_factor(input$interest_rate,input$contract_term_ea)})
  
  npmainx_ea<-reactive({survival_x(input$main_age,input$contract_term_ea)}) #this is npx of group x
  npmainy_ea<-reactive({survival_y(input$main_age,input$contract_term_ea)}) #this is npx of group y
  
  adue_xn_ea<-reactive({annuity_due_x((input$main_age+input$contract_term_ea),input$interest_rate)}) #this is adue_x:n in mortality table
  adue_yn_ea<-reactive({annuity_due_y((input$main_age+input$contract_term_ea),input$interest_rate)}) #this is adue_y:n in mortality table
  A_xn_ea <- reactive({(assurance_x((input$main_age+input$contract_term_ea),input$interest_rate))}) #this is A_(x+n)
  A_yn_ea <- reactive({(assurance_y((input$main_age+input$contract_term_ea),input$interest_rate))}) #this is A_(x+n)
  
  termadue_x_lp_ea <- reactive({(((adue_x()-vn_ea()*npmainx_ea()*adue_xn_ea())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_ea()*npmainx_ea())))})#this is [a(m)x:n] term annuity due with mthly payments of group x
  
  termadue_y_lp_ea <- reactive({(((adue_y()-vn_ea()*npmainy_ea()*adue_yn_ea())-
                                    ((frequency_of_premium_payments()-1)/
                                       (2*frequency_of_premium_payments()))*(1-vn_ea()*npmainy_ea())))})#this is [a(m)x:n] term annuity due with mthly payments of group y
  
  A_x_n_ea <- reactive({((A_x()-vn_ea()*(npmainx_pe()*A_xn_ea()))+(vn_ea()*npmainx_ea()))}) #Endowment Assurance EPV for group x
  A_y_n_ea <- reactive({((A_y()-vn_ea()*(npmainy_pe()*A_yn_ea()))+(vn_ea()*npmainy_ea()))}) #Endowment Assurance EPV for group x
  A_x_n_ea_iud <- reactive({(sqrt(1+input$interest_rate)*(A_x()-vn_ea()*(npmainx_pe()*A_xn_ea()))+(vn_ea()*npmainx_ea()))}) #Endowment Assurance EPV for group x
  A_y_n_ea_iud <- reactive({(sqrt(1+input$interest_rate)*(A_y()-vn_ea()*(npmainy_pe()*A_yn_ea()))+(vn_ea()*npmainy_ea()))}) #Endowment Assurance EPV for group x
  
  
  #ENDOWMENT ASSURANCE - X:SINGLE
  #Immediately Upon Death
  ea_x_sp_iud<-reactive({((input$survival_benefit_ea*A_x_n_ea_iud()*(1+input$claim_expense/100))/
                            (1-input$initial_expense/100))})
  #End Of Year
  ea_x_sp_eoy<-reactive({((input$survival_benefit_ea*A_x_n_ea()*(1+input$claim_expense/100))/
                            (1-input$initial_expense/100))})
  
  #ENDOWMENT ASSURANCE - Y:SINGLE
  #Immediately Upon Death
  ea_y_sp_iud<-reactive({((input$survival_benefit_ea*A_y_n_ea_iud()*(1+input$claim_expense/100))/
                            (1-input$initial_expense/100))})                      
  #End Of Year
  ea_y_sp_eoy<-reactive({((input$survival_benefit_ea*A_y_n_ea()*(1+input$claim_expense/100))/
                            (1-input$initial_expense/100))})
  
  #ENDOWMENT ASSURANCE - X:LEVEL
  #Immediately Upon Death
  ea_x_lp_iud<-reactive({((input$survival_benefit_ea*A_x_n_ea_iud()*(1+input$claim_expense/100))/
                            (frequency_of_premium_payments()*termadue_x_lp_ea()-
                               input$initial_expense/100-
                               input$premium_expense/100*
                               (frequency_of_premium_payments()*termadue_x_lp_ea()-(1/frequency_of_premium_payments()))))})
  #End Of Year
  ea_x_lp_eoy<-reactive({((input$survival_benefit_ea*A_x_n_ea()*(1+input$claim_expense/100))/
                            (frequency_of_premium_payments()*termadue_x_lp_ea()-
                               input$initial_expense/100-
                               input$premium_expense/100*
                               (frequency_of_premium_payments()*termadue_x_lp_ea()-(1/frequency_of_premium_payments()))))})
  
  #ENDOWMENT ASSURANCE - Y:LEVEL     
  #Immediately Upon Death
  ea_y_lp_iud<-reactive({((input$survival_benefit_ea*A_y_n_ea_iud()*(1+input$claim_expense/100))/
                            (frequency_of_premium_payments()*termadue_y_lp_ea()-
                               input$initial_expense/100-
                               input$premium_expense/100*
                               (frequency_of_premium_payments()*termadue_y_lp_ea()-(1/frequency_of_premium_payments()))))})
  #End Of Year
  ea_y_lp_eoy<-reactive({((input$survival_benefit_ea*A_y_n_ea()*(1+input$claim_expense/100))/
                            (frequency_of_premium_payments()*termadue_y_lp_ea()-
                               input$initial_expense/100-
                               input$premium_expense/100*
                               (frequency_of_premium_payments()*termadue_y_lp_ea()-(1/frequency_of_premium_payments()))))})
  #WHOLE LIFE ASSURANCE
  adue_xm_wla <- reactive({(frequency_of_premium_payments()*(adue_x()-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))})
  adue_ym_wla <- reactive({(frequency_of_premium_payments()*(adue_y()-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))})
  
  #WHOLE LIFE ASSURANCE - X:SINGLE
  #Immediately Upon Death
  wla_x_sp_iud<-reactive({((input$death_benefit_wla*(sqrt(1+input$interest_rate))*A_x()*(1+input$claim_expense/100))/
                             (1-input$initial_expense/100))})
  #End Of Year
  wla_x_sp_eoy<-reactive({((input$death_benefit_wla*A_x()*(1+input$claim_expense/100))/
                             (1-input$initial_expense/100))})
  
  
  #WHOLE LIFE ASSURANCE - Y:SINGLE
  #Immediately Upon Death
  wla_y_sp_iud<-reactive({((input$death_benefit_wla*(sqrt(1+input$interest_rate))*A_y()*(1+input$claim_expense/100))/
                             (1-input$initial_expense/100))})
  #End Of Year
  wla_y_sp_eoy<-reactive({((input$death_benefit_wla*A_y()*(1+input$claim_expense/100))/
                             (1-input$initial_expense/100))})
  
  #WHOLE LIFE ASSURANCE - X:LEVEL
  #Immediately Upon Death
  wla_x_lp_iud<-reactive({((input$death_benefit_wla*(sqrt(1+input$interest_rate))*A_x()*(1+input$claim_expense/100))/
                             (adue_xm_wla()-
                                input$initial_expense/100-
                                input$premium_expense/100*
                                (adue_xm_wla()-(1/frequency_of_premium_payments()))))})
  #End Of Year
  wla_x_lp_eoy<-reactive({((input$death_benefit_wla*A_x()*(1+input$claim_expense/100))/
                             (adue_xm_wla()-
                                input$initial_expense/100-
                                input$premium_expense/100*
                                (adue_xm_wla()-(1/frequency_of_premium_payments()))))})
  
  #WHOLE LIFE ASSURANCE - Y:LEVEL
  #Immediately Upon Death
  wla_y_lp_iud<-reactive({((input$death_benefit_wla*(sqrt(1+input$interest_rate))*A_y()*(1+input$claim_expense/100))/
                             (adue_ym_wla()-
                                input$initial_expense/100-
                                input$premium_expense/100*
                                (adue_ym_wla()-(1/frequency_of_premium_payments()))))})
  #End Of Year
  wla_y_lp_eoy<-reactive({((input$death_benefit_wla*A_y()*(1+input$claim_expense/100))/
                             (adue_ym_wla()-
                                input$initial_expense/100-
                                input$premium_expense/100*
                                (adue_ym_wla()-(1/frequency_of_premium_payments()))))})
  
  
  
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------   
  #-----------------------------------------------------------------SINGLE LIFE ANNUITIES--------------------------------------------------------------------------------------------------------      
  #------------------------------------------------------------------USEFUL VALUES -----------------------------------------------------------
  frequency_of_premium_payment <- reactive({input$frequency_of_premium_payments})
  freq_pay <- reactive({if (frequency_of_premium_payment() == "1"){12} else {1}})
  
  #wlan
  frequency_of_annuity_payment_wlan <- reactive({input$frequency_of_annuity_payments_wlan})
  m_wlan <- reactive({if (frequency_of_annuity_payment_wlan() == "1"){12} else {1}})
  
  ax<-reactive({annuity_due_x(input$main_age,input$interest_rate)-1}) #ax
  ay<-reactive({annuity_due_y(input$main_age,input$interest_rate)-1}) #ay
  ax_m_wlan<-reactive({ax()+(m_wlan()-1)/(2*m_wlan())}) #ax monthly for wlan
  ay_m_wlan<-reactive({ay()+(m_wlan()-1)/(2*m_wlan())}) #ay monthly for wlan
  aduex_m<-reactive({annuity_due_x(input$main_age,input$interest_rate)-((freq_pay()-1)/(2*freq_pay()))}) #aduex monthly
  aduey_m<-reactive({annuity_due_y(input$main_age,input$interest_rate)-((freq_pay()-1)/(2*freq_pay()))}) #aduey monthly
  
  #tan
  frequency_of_annuity_payment_tan <- reactive({input$frequency_of_annuity_payments_tan})
  m_tan <- reactive({if (frequency_of_annuity_payment_tan() == "1"){12} else {1}})
  
  vn_tan<-reactive({discount_factor(input$interest_rate,input$contract_term_tan)}) #vn for tan
  npmainx_tan<-reactive({survival_x(input$main_age,input$contract_term_tan)}) #npx for tan
  npmainy_tan<-reactive({survival_y(input$main_age,input$contract_term_tan)}) #npy for tan
  a_x_n_tan<-reactive({annuity_due_x((input$main_age+input$contract_term_tan),input$interest_rate)-1}) #ax+n for tan
  a_y_n_tan<-reactive({annuity_due_y((input$main_age+input$contract_term_tan),input$interest_rate)-1}) #ay+n for tan
  a_x_nbar_tan<-reactive({ax()-vn_tan()*npmainx_tan()*a_x_n_tan()}) #ax:nbar for tan
  a_y_nbar_tan<-reactive({ay()-vn_tan()*npmainy_tan()*a_y_n_tan()}) #ay:nbar for tan
  a_x_nbar_m_tan<-reactive({a_x_nbar_tan()-((m_tan()-1)/(2*m_tan()))*(1-vn_tan()*npmainx_tan())}) #ax:nbar monthly for tan
  a_y_nbar_m_tan<-reactive({a_y_nbar_tan()-((m_tan()-1)/(2*m_tan()))*(1-vn_tan()*npmainy_tan())}) #ay:nbar monthly for tan
  adue_x_nbar_tan<-reactive({annuity_due_x(input$main_age,input$interest_rate)-vn_tan()*npmainx_tan()*annuity_due_x((input$main_age+input$contract_term_tan),input$interest_rate)})#adue_x:nbar for tan
  adue_y_nbar_tan<-reactive({annuity_due_y(input$main_age,input$interest_rate)-vn_tan()*npmainy_tan()*annuity_due_y((input$main_age+input$contract_term_tan),input$interest_rate)})#adue_y:nbar for tan
  adue_xnbar_m_tan<-reactive({adue_x_nbar_tan()-((freq_pay()-1)/(2*freq_pay()))*(1-vn_tan()*npmainx_tan())}) #adue_x:nbar monthly for tan
  adue_ynbar_m_tan<-reactive({adue_y_nbar_tan()-((freq_pay()-1)/(2*freq_pay()))*(1-vn_tan()*npmainy_tan())}) #adue_y:nbar monthly for tan
  
  # gwlan
  frequency_of_annuity_payment_gwlan <- reactive({input$frequency_of_annuity_payments_gwlan})
  m_gwlan<- reactive({if (frequency_of_annuity_payment_gwlan() == "1"){12} else {1}})
  vn_gwlan<-reactive({discount_factor(input$interest_rate,input$guarantee_payments_gwlan)}) #vn for gwlan
  npmainx_gwlan<-reactive({survival_x(input$main_age,input$guarantee_payments_gwlan)}) #npx for gwlan
  npmainy_gwlan<-reactive({survival_y(input$main_age,input$guarantee_payments_gwlan)}) #npy for gwlan
  i_m_gwlan<- reactive({m_gwlan()*((1+input$interest_rate)^(1/m_gwlan())-1)}) #i^m for gwlan
  a_nbar_gwlan<-reactive({(1-vn_gwlan())/input$interest_rate}) #a_nbar for gwlan
  a_nbar_m_gwlan<-reactive({(1/i_m_gwlan())*a_nbar_gwlan()}) #a_nbar monthly for gwlan
  a_x_n_gwlan<-reactive({annuity_due_x((input$main_age+input$guarantee_payments_gwlan),input$interest_rate)-1}) #ax+n for gwlan
  a_y_n_gwlan<-reactive({annuity_due_y((input$main_age+input$guarantee_payments_gwlan),input$interest_rate)-1}) #ay+n for gwlan
  a_x_n_m_gwlan<-reactive({a_x_n_gwlan()+(m_gwlan()-1)/(2*m_gwlan())}) #ax+n monthly for gwlan
  a_y_n_m_gwlan<-reactive({a_y_n_gwlan()+(m_gwlan()-1)/(2*m_gwlan())}) #ay+n monthly for gwlan
  a_xn_bar_bar_m_gwlan<-reactive({a_nbar_m_gwlan()+vn_gwlan()*npmainx_gwlan()*a_x_n_m_gwlan()}) #gwlan for x monthly
  a_yn_bar_bar_m_gwlan<-reactive({a_nbar_m_gwlan()+vn_gwlan()*npmainy_gwlan()*a_y_n_m_gwlan()}) #gwlan for x monthly
  aduex_m<-reactive({annuity_due_x(input$main_age,input$interest_rate)-((freq_pay()-1)/(2*freq_pay()))}) #aduex monthly
  aduey_m<-reactive({annuity_due_y(input$main_age,input$interest_rate)-((freq_pay()-1)/(2*freq_pay()))}) #aduey monthly
  
  # WHOLE LIFE ANNUITY PAID IN ARREARS (_wlan) ------------------------------
  
  
  # Single premiums
  wlan_x_sp<-reactive({((m_wlan()*input$annuity_payment_wlan*ax_m_wlan())* 
                          (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for x
  
  wlan_y_sp<-reactive({((m_wlan()*input$annuity_payment_wlan*ay_m_wlan())*
                          (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for y
  
  # Level premiums
  wlan_x_lp<-reactive({((m_wlan()*input$annuity_payment_wlan*ax_m_wlan())* 
                          (1+(input$claim_expense/100)))/
      (freq_pay()*aduex_m()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*aduex_m()-1/freq_pay()))}) #for x
  
  wlan_y_lp<-reactive({((m_wlan()*input$annuity_payment_wlan*ay_m_wlan())* 
                          (1+(input$claim_expense/100)))/
      (freq_pay()*aduey_m()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*aduey_m()-1/freq_pay()))}) #for y
  
  # TERM ANNUITY PAID IN ARREARS (_tan) -------------------------------------
  
  # Single premiums
  tan_x_sp<-reactive({((m_tan()*input$annuity_payment_tan*a_x_nbar_m_tan())*
                         (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for x
  
  tan_y_sp<-reactive({((m_tan()*input$annuity_payment_tan*a_y_nbar_m_tan())*
                         (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for y
  
  # Level premiums
  tan_x_lp<-reactive({((m_tan()*input$annuity_payment_tan*a_x_nbar_m_tan())*
                         (1+(input$claim_expense/100)))/
      (freq_pay()*adue_xnbar_m_tan()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*adue_xnbar_m_tan()-1/freq_pay()))}) #for x
  
  tan_y_lp<-reactive({((m_tan()*input$annuity_payment_tan*a_y_nbar_m_tan())*
                         (1+(input$claim_expense/100)))/
      (freq_pay()*adue_ynbar_m_tan()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*adue_ynbar_m_tan()-1/freq_pay()))}) #for y
  
  # GUARANTEED WHOLE LIFE ANNUITY PAID IN ARREARS (_gwlan) ------------------
  
  # Single premiums
  gwlan_x_sp<-reactive({((m_gwlan()*input$annuity_payment_gwlan*a_xn_bar_bar_m_gwlan())* 
                           (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for x
  
  gwlan_y_sp<-reactive({((m_gwlan()*input$annuity_payment_gwlan*a_yn_bar_bar_m_gwlan())* 
                           (1+(input$claim_expense/100)))/
      (1-(input$initial_expense/100))}) #for y
  
  # Level premiums
  gwlan_x_lp<-reactive({((m_gwlan()*input$annuity_payment_gwlan*a_xn_bar_bar_m_gwlan())* 
                           (1+(input$claim_expense/100)))/
      (freq_pay()*aduex_m()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*aduex_m()-1/freq_pay()))}) #for x
  
  gwlan_y_lp<-reactive({((m_gwlan()*input$annuity_payment_gwlan*a_yn_bar_bar_m_gwlan())* 
                           (1+(input$claim_expense/100)))/
      (freq_pay()*aduey_m()-(input$initial_expense/100)-(input$premium_expense/100)*(freq_pay()*aduey_m()-1/freq_pay()))}) #for y
  
  
  
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------      
  #---------------------------------------------------------------JOINT LIFE ASSURANCES----------------------------------------------------------------------------------------------------      
  #Defining variables
  
  A_xy<-reactive(assurance_xy(input$main_age, input$partner_age, input$interest_rate)) #this is A_xy
  
  adue_xy_m1<-reactive({annuity_due_xym1(input$main_age, input$partner_age, input$interest_rate)}) #this is adue_xy for m=1
  
  adue_xy_m12<-reactive({annuity_due_xym12(input$main_age, input$partner_age, input$interest_rate)}) #this is adue_xy for m=12
  
  
  
  #Single premium, payable at end of year of death
  
  jwla_sp_end<-reactive({(input$death_benefit_jwla*(A_xy())*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})
  
  #Single premium, payable immediately on death
  
  jwla_sp_i<-reactive({(input$death_benefit_jwla*(A_xy())*((1+input$interest_rate)^0.5)*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})
  
  #Yearly premium, payable at end of year of death
  
  jwla_yp_end<-reactive({(input$death_benefit_jwla*(A_xy())*(1+input$claim_expense/100))/
      
      (adue_xy_m1()-(input$initial_expense/100)-input$premium_expense*(adue_xy_m1()-1))})
  
  
  #Yearly premium, payable immediately on death
  
  jwla_yp_i<-reactive({(input$death_benefit_jwla*(A_xy())*(1+input$interest_rate)^0.5*(1+input$claim_expense/100))/
      
      (adue_xy_m1()-(input$initial_expense/100)-input$premium_expense*(adue_xy_m1()-1))})   
  
  
  #Monthly premium, payable at end of year of death
  
  jwla_mp_end<-reactive({(input$death_benefit_jwla*(A_xy())*(1+input$claim_expense/100))/
      
      (12*adue_xy_m12()-(input$initial_expense/100)-input$premium_expense*(12*adue_xy_m12()-(1/12)))})
  
  
  #Monthly premium, payable immediately on death
  
  jwla_mp_i<-reactive({(input$death_benefit_jwla*(A_xy())*(1+input$interest_rate)^0.5*(1+input$claim_expense/100))/
      
      (12*adue_xy_m12()-(input$initial_expense/100)-input$premium_expense*(12*adue_xy_m12()-(1/12)))})   
  
  
  
  ##JOINT TERM LIFE ASSURANCE----------------------------------------------------------------------------------------------
  
  
  
  #Defining variables
  
  A_xy1_n<-reactive(assurance_xy1_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jta)) #this is A_xy1_n
  
  adue_xym1_n_jta<-reactive(annuity_due_xym1_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jta)) #this is adue_xy_n for m=1
  
  adue_xym12_n_jta<-reactive(annuity_due_xym12_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jta)) #this is adue_xy_n for m=12
  
  
  
  #Single premium, payable at end of year of death
  
  jta_sp_end<-reactive({(input$death_benefit_jta*(A_xy1_n())*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})
  
  #Single premium, payable immediately on death
  
  jta_sp_i<-reactive({(input$death_benefit_jta*(A_xy1_n())*((1+input$interest_rate)^0.5)*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})
  
  #Yearly premium, payable at end of year of death
  
  jta_yp_end<-reactive({(input$death_benefit_jta*(A_xy1_n())*(1+input$claim_expense/100))/
      
      (adue_xym1_n_jta()-(input$initial_expense/100)-input$premium_expense*(adue_xym1_n_jta()-1))})
  
  
  #Yearly premium, payable immediately on death
  
  jta_yp_i<-reactive({(input$death_benefit_jta*(A_xy1_n())*(1+input$interest_rate)^0.5*(1+input$claim_expense/100))/
      
      (adue_xym1_n_jta()-(input$initial_expense/100)-input$premium_expense*(adue_xym1_n_jta()-1))})   
  
  
  #Monthly premium, payable at end of year of death
  
  jta_mp_end<-reactive({(input$death_benefit_jta*(A_xy1_n())*(1+input$claim_expense/100))/
      
      (12*adue_xym12_n_jta()-(input$initial_expense/100)-input$premium_expense*(12*adue_xym12_n_jta()-(1/12)))})
  
  
  #Monthly premium, payable immediately on death
  
  jta_mp_i<-reactive({(input$death_benefit_jta*(A_xy1_n())*(1+input$interest_rate)^0.5*(1+input$claim_expense/100))/
      
      (12*adue_xym12_n_jta()-(input$initial_expense/100)-input$premium_expense*(12*adue_xym12_n_jta()-(1/12)))})  
  
  
  
  ##JOINT WHOLE LIFE ANNUITY-----------------------------------------------------------------------------------------------
  
  
  
  #Defining variables
  
  a_xy_m1<-reactive({annuity_arrears_xym1(input$main_age, input$partner_age, input$interest_rate)}) #this is aarrears_xy for m=1
  
  a_xy_m12<-reactive({annuity_arrears_xym12(input$main_age, input$partner_age, input$interest_rate)}) #this is aarrears_xy for m=12
  
  
  
  #Single premium, annuity payable yearly in arrears
  
  jwlan_sp_y<-reactive({(input$annuity_payment_jwlan*a_xy_m1()*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))}) 
  
  #Single premium, annuity payable monthly in arrears
  
  jwlan_sp_m<-reactive({((input$annuity_payment_jwlan*12*a_xy_m12()*(1+input$claim_expense/100))/
                           
                           (1-(input$initial_expense/100)))})
  
  #Yearly premium, annuity payable yearly in arrears
  
  jwlan_yp_y<-reactive({(input$annuity_payment_jwlan*a_xy_m1()*(1+input$claim_expense/100))/
      
      (adue_xy_m1()-(input$initial_expense/100)-input$premium_expense*(adue_xy_m1()-1))})
  
  
  #Yearly premium, annuity payable monthly in arrears
  
  jwlan_yp_m<-reactive({(input$annuity_payment_jwlan*12*a_xy_m12()*(1+input$claim_expense/100))/
      
      (adue_xy_m1()-(input$initial_expense/100)-input$premium_expense*(adue_xy_m1()-1))})
  
  
  #Monthly premium, annuity payable yearly in arrears
  
  jwlan_mp_y<-reactive({(input$annuity_payment_jwlan*a_xy_m1()*(1+input$claim_expense/100))/
      
      (12*adue_xy_m12()-(input$initial_expense/100)-input$premium_expense*(12*adue_xy_m12()-(1/12)))})  
  
  
  #Monthly premium, annuity payable monthly in arrears
  
  jwlan_mp_m<-reactive({(input$annuity_payment_jwlan*a_xy_m12()*(1+input$claim_expense/100))/
      
      (12*adue_xy_m12()-(input$initial_expense/100)-input$premium_expense*(12*adue_xy_m12()-(1/12)))}) 
  
  
  ##JOINT TERM LIFE ANNUITY----------------------------------------------------------------------------------------------
  
  
  
  #Defining variables
  
  adue_xym1_n_jtan<-reactive(annuity_due_xym1_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jtan)) #this is adue_xy_n for m=1
  
  adue_xym12_n_jtan<-reactive(annuity_due_xym12_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jtan)) #this is adue_xy_n for m=12
  
  a_xym1_n<-reactive(annuity_xym1_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jtan)) #this is aarrears_xy_n for m=1
  
  a_xym12_n<-reactive(annuity_xym12_n(input$main_age, input$partner_age, input$interest_rate, input$contract_term_jtan)) #this is aarrears_xy_n for m=12
  
  
  
  #Single premium, annuity payable yearly in arrears
  
  jtan_sp_y<-reactive({(input$annuity_payment_jtan*a_xym1_n()*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})  
  
  #Single premium, annuity payable monthly in arrears
  
  jtan_sp_m<-reactive({(input$annuity_payment_jtan*12*a_xym12_n()*(1+input$claim_expense/100))/
      
      (1-(input$initial_expense/100))})
  
  #Yearly premium, annuity payable yearly in arrears
  
  jtan_yp_y<-reactive({(input$annuity_payment_jtan*a_xym1_n()*(1+input$claim_expense/100))/
      
      (adue_xym1_n_jtan()-(input$initial_expense/100)-input$premium_expense*(adue_xym1_n_jtan()-1))})
  
  
  #Yearly premium, annuity payable monthly in arrears
  
  jtan_yp_m<-reactive({(input$annuity_payment_jtan*12*a_xym12_n()*(1+input$claim_expense/100))/
      
      (adue_xym1_n_jtan()-(input$initial_expense/100)-input$premium_expense*(adue_xym1_n_jtan()-1))})
  
  
  #Monthly premium, annuity payable yearly in arrears
  
  jtan_mp_y<-reactive({(input$annuity_payment_jtan*a_xym1_n()*(1+input$claim_expense/100))/
      
      (12*adue_xym12_n_jtan()-(input$initial_expense/100)-input$premium_expense*(12*adue_xym12_n_jtan()-(1/12)))}) 
  
  
  #Monthly premium, annuity payable monthly in arrears
  
  jtan_mp_m<-reactive({(input$annuity_payment_jtan*12*a_xym12_n()*(1+input$claim_expense/100))/
      
      (12*adue_xym12_n_jtan()-(input$initial_expense/100)-input$premium_expense*(12*adue_xym12_n_jtan()-(1/12)))})
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------RESERVE CALCULATIONS---------------------------------------------------------------------------------------------------------
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------    
  
  #Pure Endowment - Single;X
  output$pe_x_sp_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_pe))
    for (t in c((input$main_age-input$main_age):(input$contract_term_pe))) {
      reserve[t+1]<-(input$survival_benefit_pe*reserve_x_pe(age=(input$main_age+t),contract_term=(input$contract_term_pe-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab=("Seniority (years)"), ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(pe_x_sp(),digits=4)))
    
    
  })
  
  #Pure Endowment - Single;Y
  output$pe_y_sp_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_pe))
    for (t in c((input$main_age-input$main_age):(input$contract_term_pe))) {
      reserve[t+1]<-(input$survival_benefit_pe*reserve_y_pe(age=(input$main_age+t),contract_term=(input$contract_term_pe-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =", round(pe_y_sp(),digits=4)))
  })
  
  #Pure Endowment - Level;X
  output$pe_x_lp_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_pe))
    for (t in c((input$main_age-input$main_age):(input$contract_term_pe))) {
      reserve[t+1]<-(input$survival_benefit_pe*reserve_x_pe(age=(input$main_age+t),contract_term=(input$contract_term_pe-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*pe_x_lp()*frequency_of_premium_payments()*(annuity_due_xnt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(pe_x_lp(),digits=4)))
  })
  
  #Pure Endowment - Level;Y
  output$pe_y_lp_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_pe))
    for (t in c((input$main_age-input$main_age):(input$contract_term_pe))) {
      reserve[t+1]<-(input$survival_benefit_pe*reserve_y_pe(age=(input$main_age+t),contract_term=(input$contract_term_pe-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*pe_y_lp()*frequency_of_premium_payments()*(annuity_due_ynt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(pe_y_lp(),digits=4)))
  })  
  
  #Term Assurance - Single; X - IUD
  output$ta_x_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*(sqrt(1+input$interest_rate))*reserve_x_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(ta_x_sp_iud(),digits=4)))
  })
  
  #Term Assurance - Single; X - EOY
  output$ta_x_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*reserve_x_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(ta_x_sp_eoy(),digits=4)))
  })
  
  #Term Assurance - Single; Y - IUD
  output$ta_y_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*(sqrt(1+input$interest_rate))*reserve_y_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(ta_y_sp_iud(),digits=4)))
  })
  
  #Term Assurance - Single; Y - EOY
  output$ta_y_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*reserve_y_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(ta_y_sp_eoy(),digits=4)))
  })
  
  #Term Assurance - Level; X - IUD
  output$ta_x_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*(sqrt(1+input$interest_rate))*reserve_x_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+(input$claim_expense/100))-
                       (1-input$premium_expense/100)*ta_x_lp_iud()*frequency_of_premium_payments()*(annuity_due_xnt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ta_x_lp_iud(),digits=4)))
  })
  
  #Term Assurance - Level; X - EOY
  output$ta_x_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*reserve_x_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ta_x_lp_iud()*frequency_of_premium_payments()*(annuity_due_xnt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ta_x_lp_eoy(),digits=4)))
  })
  
  #Term Assurance - Level; Y - IUD
  output$ta_y_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*(sqrt(1+input$interest_rate))*reserve_y_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ta_y_lp_iud()*frequency_of_premium_payments()*(annuity_due_ynt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ta_y_lp_iud(),digits=4)))
  })
  
  #Term Assurance - Level; Y - EOY
  output$ta_y_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ta))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ta))) {
      reserve[t+1]<-(input$death_benefit_ta*reserve_y_ta(age=(input$main_age+t),contract_term=(input$contract_term_ta-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ta_y_lp_eoy()*frequency_of_premium_payments()*(annuity_due_ynt((input$main_age+t),(input$contract_term_ta-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ta_y_lp_eoy(),digits=4)))
  })
  
  #Endowment Assurance - Single; X - IUD
  output$ea_x_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_x_ea_iud(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_x_sp_iud(),digits=4)))
  })
  
  #Endowment Assurance - Single; X - EOY
  output$ea_x_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_x_ea_eoy(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_x_sp_eoy(),digits=4)))
  })
  
  #Endowment Assurance - Single; Y - IUD
  output$ea_y_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_y_ea_iud(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_y_sp_iud(),digits=4)))
  })
  
  #Endowment Assurance - Single; Y - EOY
  output$ea_y_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_y_ea_eoy(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_y_sp_eoy(),digits=4)))
  })
  
  #Endowment Assurance - Level; X - IUD
  output$ea_x_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_x_ea_iud(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ea_x_lp_iud()*frequency_of_premium_payments()*(annuity_due_xnt((input$main_age+t),(input$contract_term_ea-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_x_lp_iud(),digits=4)))
  })
  
  #Endowment Assurance - Level; X - EOY
  output$ea_x_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_x_ea_eoy(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ea_x_lp_eoy()*frequency_of_premium_payments()*(annuity_due_xnt((input$main_age+t),(input$contract_term_ea-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_x_lp_eoy(),digits=4)))
  })
  
  #Endowment Assurance - Level; Y - IUD
  output$ea_y_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_y_ea_iud(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ea_y_lp_iud()*frequency_of_premium_payments()*(annuity_due_ynt((input$main_age+t),(input$contract_term_ea-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_y_lp_iud(),digits=4)))
  })
  
  #Endowment Assurance - Level; Y - EOY
  output$ea_y_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(input$contract_term_ea))
    for (t in c((input$main_age-input$main_age):(input$contract_term_ea))) {
      reserve[t+1]<-(input$death_benefit_ea*reserve_y_ea_eoy(age=(input$main_age+t),contract_term=(input$contract_term_ea-t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*ea_y_lp_eoy()*frequency_of_premium_payments()*(annuity_due_ynt((input$main_age+t),(input$contract_term_ea-t),input$interest_rate)-((frequency_of_premium_payments()-1)/(2*frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(ea_y_lp_eoy(),digits=4)))
  })
  
  #Whole Life Assurance - Single Premium - X;IUD
  output$wla_x_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-input$death_benefit_wla*(sqrt(1+input$interest_rate))*assurance_x(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_x_sp_iud(),digits=4)))
  })
  
  #Whole Life Assurance - Single Premium - X;EOY
  output$wla_x_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-input$death_benefit_wla*assurance_x(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_x_sp_eoy(),digits=4)))
  })
  
  #Whole Life Assurance - Single Premium - Y;IUD
  output$wla_y_sp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-input$death_benefit_wla*(sqrt(1+input$interest_rate))*assurance_y(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_y_sp_iud(),digits=4)))
  })
  
  #Whole Life Assurance - Single Premium - Y;EOY
  output$wla_y_sp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-input$death_benefit_wla*assurance_y(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_y_sp_eoy(),digits=4)))
  })
  
  #Whole Life Assurance - Level; X - IUD
  output$wla_x_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-(input$death_benefit_wla*(sqrt(1+input$interest_rate))*assurance_x(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*wla_x_lp_iud()*(m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=frequency_of_premium_payments())))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_x_lp_iud(),digits=4)))
  })
  
  #Whole Life Assurance - Level; X - EOY
  output$wla_x_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-(input$death_benefit_wla*assurance_x(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*wla_x_lp_eoy()*(m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=frequency_of_premium_payments())))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_x_lp_eoy(),digits=4)))
  })
  
  #Whole Life Assurance - Level; Y - IUD
  output$wla_y_lp_iud_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-(input$death_benefit_wla*(sqrt(1+input$interest_rate))*assurance_y(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*wla_y_lp_iud()*(m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=frequency_of_premium_payments())))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_y_lp_iud(),digits=4)))
  })
  
  #Whole Life Assurance - Level; Y - EOY
  output$wla_y_lp_eoy_plot<-renderPlot({
    
    reserve<-c()
    seniority<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve[t+1]<-(input$death_benefit_wla*assurance_y(age=(input$main_age+t),i=input$interest_rate)*(1+input$claim_expense/100)-
                       (1-input$premium_expense/100)*wla_y_lp_eoy()*((m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=frequency_of_premium_payments()))))
    }                
    reserve[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority,reserve,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3,xlab="Seniority (years)", ylab="Reserve ($)", 
         main=paste("Evolution of Reserve\nPremium =",round(wla_y_lp_eoy(),digits=4)))
  })
  
  # RESERVES - WHOLE LIFE ANNUITY PAID IN ARREARS (_wlan)  ------------------
  
  # Single premium: x
  output$wlan_x_sp_plot<-renderPlot({
    reserve_wlan_x_sp<-c()
    seniority_wlan_x_sp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_wlan_x_sp[t+1]<-(input$annuity_payment_wlan*
                                 m_axm_r(age=(input$main_age+t),i=input$interest_rate,m=m_wlan())*
                                 (1+input$claim_expense/100))
    }                
    reserve_wlan_x_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_wlan_x_sp,reserve_wlan_x_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",wlan_x_sp()))
  }) 
  
  # Single premium: y
  output$wlan_y_sp_plot<-renderPlot({
    reserve_wlan_y_sp<-c()
    seniority_wlan_y_sp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_wlan_y_sp[t+1]<-(input$annuity_payment_wlan*
                                 m_aym_r(age=(input$main_age+t),i=input$interest_rate,m=m_wlan())*
                                 (1+input$claim_expense/100))
    }                
    reserve_wlan_y_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_wlan_y_sp,reserve_wlan_y_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",wlan_y_sp()))
  })  
  
  # Level premium: x
  output$wlan_x_lp_plot<-renderPlot({
    reserve_wlan_x_lp<-c()
    seniority_wlan_x_lp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_wlan_x_lp[t+1]<-(input$annuity_payment_wlan*
                                 m_axm_r(age=(input$main_age+t),i=input$interest_rate,m=m_wlan())*
                                 (1+input$claim_expense/100)-(1-input$premium_expense/100)*wlan_x_lp()*
                                 m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=freq_pay()))
    }                
    reserve_wlan_x_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_wlan_x_lp,reserve_wlan_x_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",wlan_x_lp()))
  }) 
  
  # Level premium: y
  output$wlan_y_lp_plot<-renderPlot({
    reserve_wlan_y_lp<-c()
    seniority_wlan_y_lp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_wlan_y_lp[t+1]<-(input$annuity_payment_wlan*
                                 m_aym_r(age=(input$main_age+t),i=input$interest_rate,m=m_wlan())*
                                 (1+input$claim_expense/100)-(1-input$premium_expense/100)*wlan_y_lp()*
                                 m_adueym_r(age=(input$main_age+t),i=input$interest_rate,m=freq_pay()))
    }                
    reserve_wlan_y_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_wlan_y_lp,reserve_wlan_y_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",wlan_y_lp()))
  }) 
  
  # RESERVES - TERM ANNUITY PAID IN ARREARS (_tan)  -------------------------
  
  # Single premium: x
  output$tan_x_sp_plot<-renderPlot({
    reserve_tan_x_sp<-c()
    seniority_tan_x_sp<-c((input$main_age-input$main_age):(input$contract_term_tan))
    for (t in c((input$main_age-input$main_age):(input$contract_term_tan))) {
      reserve_tan_x_sp[t+1]<-(input$annuity_payment_tan*
                                m_ax_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                            contract_term=(input$contract_term_tan-t),m=m_tan())*
                                (1+input$claim_expense/100))
    }                
    reserve_tan_x_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_tan_x_sp,reserve_tan_x_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",tan_x_sp()))
  })
  
  # Single premium: y
  output$tan_y_sp_plot<-renderPlot({
    reserve_tan_y_sp<-c()
    seniority_tan_y_sp<-c((input$main_age-input$main_age):(input$contract_term_tan))
    for (t in c((input$main_age-input$main_age):(input$contract_term_tan))) {
      reserve_tan_y_sp[t+1]<-(input$annuity_payment_tan*
                                m_ay_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                            contract_term=(input$contract_term_tan-t),m=m_tan())*
                                (1+input$claim_expense/100))
    }                
    reserve_tan_y_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_tan_y_sp,reserve_tan_y_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",tan_y_sp()))
  })
  
  # Level premium: x
  output$tan_x_lp_plot<-renderPlot({
    reserve_tan_x_lp<-c()
    seniority_tan_x_lp<-c((input$main_age-input$main_age):(input$contract_term_tan))
    for (t in c((input$main_age-input$main_age):(input$contract_term_tan))) {
      reserve_tan_x_lp[t+1]<-(input$annuity_payment_tan*
                                m_ax_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                            contract_term=(input$contract_term_tan-t),m=m_tan())*
                                (1+input$claim_expense/100)-(1-input$premium_expense/100)*tan_x_lp()*
                                m_aduex_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                               contract_term=(input$contract_term_tan-t),m=freq_pay()))
    }                
    reserve_tan_x_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_tan_x_lp,reserve_tan_x_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",tan_x_lp()))
  })
  
  # Level premium: y
  output$tan_y_lp_plot<-renderPlot({
    reserve_tan_y_lp<-c()
    seniority_tan_y_lp<-c((input$main_age-input$main_age):(input$contract_term_tan))
    for (t in c((input$main_age-input$main_age):(input$contract_term_tan))) {
      reserve_tan_y_lp[t+1]<-(input$annuity_payment_tan*
                                m_ay_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                            contract_term=(input$contract_term_tan-t),m=m_tan())*
                                (1+input$claim_expense/100)-(1-input$premium_expense/100)*tan_y_lp()*
                                m_aduey_nbar_r(age=(input$main_age+t),i=input$interest_rate,
                                               contract_term=(input$contract_term_tan-t),m=freq_pay()))
    }                
    reserve_tan_y_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_tan_y_lp,reserve_tan_y_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",tan_y_lp()))
  })
  
  
  # RESERVES - GUARANTEED WHOLE LIFE ANNUITY PAID IN ARREARS (_gwlan) -------
  
  # Single premium: x
  output$gwlan_x_sp_plot<-renderPlot({
    reserve_gwlan_x_sp<-c()
    seniority_gwlan_x_sp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_gwlan_x_sp[t+1]<-(input$annuity_payment_gwlan*
                                  m_ax_nbarbar_r(age=(input$main_age+t),i=input$interest_rate,
                                                 contract_term=(input$guarantee_payments_gwlan-t),
                                                 m=m_gwlan())*(1+input$claim_expense/100))
    }                
    reserve_gwlan_x_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_gwlan_x_sp,reserve_gwlan_x_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",gwlan_x_sp()))
  }) 
  
  # Single premium: y
  output$gwlan_y_sp_plot<-renderPlot({
    reserve_gwlan_y_sp<-c()
    seniority_gwlan_y_sp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_gwlan_y_sp[t+1]<-(input$annuity_payment_gwlan*
                                  m_ay_nbarbar_r(age=(input$main_age+t),i=input$interest_rate,
                                                 contract_term=(input$guarantee_payments_gwlan-t),
                                                 m=m_gwlan())*(1+input$claim_expense/100))
    }                
    reserve_gwlan_y_sp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_gwlan_y_sp,reserve_gwlan_y_sp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",gwlan_y_sp()))
  })
  
  # Level premium: x
  output$gwlan_x_lp_plot<-renderPlot({
    reserve_gwlan_x_lp<-c()
    seniority_gwlan_x_lp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_gwlan_x_lp[t+1]<-(input$annuity_payment_gwlan*
                                  m_ax_nbarbar_r(age=(input$main_age+t),i=input$interest_rate,
                                                 contract_term=(input$guarantee_payments_gwlan-t),
                                                 m=m_gwlan())*(1+input$claim_expense/100)-
                                  (1-input$premium_expense/100)*gwlan_x_lp()*
                                  m_aduexm_r(age=(input$main_age+t),i=input$interest_rate,m=freq_pay()))
    }                
    reserve_gwlan_x_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_gwlan_x_lp,reserve_gwlan_x_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",gwlan_x_lp()))
  }) 
  
  # Level premium: y
  output$gwlan_y_lp_plot<-renderPlot({
    reserve_gwlan_y_lp<-c()
    seniority_gwlan_y_lp<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_gwlan_y_lp[t+1]<-(input$annuity_payment_gwlan*
                                  m_ay_nbarbar_r(age=(input$main_age+t),i=input$interest_rate,
                                                 contract_term=(input$guarantee_payments_gwlan-t),
                                                 m=m_gwlan())*(1+input$claim_expense/100)-
                                  (1-input$premium_expense/100)*gwlan_y_lp()*
                                  m_adueym_r(age=(input$main_age+t),i=input$interest_rate,m=freq_pay()))
    }                
    reserve_gwlan_y_lp[1]<-0               
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_gwlan_y_lp,reserve_gwlan_y_lp,type="l", col="sandybrown", col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)",main=paste("Evolution of Reserve\nPremium=",gwlan_y_lp()))
  }) 
  ##JOINT WHOLE LIFE ASSURANCES-------------------------------------------------------------------------------------------------------------------------------------      
  
  #Single premium, payable at end of year of death
  output$jwla_sp_end_plot<-renderPlot({
    reserve_jwla_sp_end <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_sp_end<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_sp_end[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)
      seniority_jwla_sp_end[t]<-t                
    }                
    reserve_jwla_sp_end[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_sp_end,reserve_jwla_sp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_sp_end(),digits=4)))
  })
  
  
  #Single premium, payable immediately on death 
  output$jwla_sp_i_plot<-renderPlot({
    reserve_jwla_sp_i <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_sp_i<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_sp_i[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*((1+input$interest_rate)^0.5)*(1+input$claim_expense/100)
      seniority_jwla_sp_i[t]<-t                
    }                
    reserve_jwla_sp_i[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_sp_i,reserve_jwla_sp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_sp_i(),digits=4)))
  })
  
  
  #Yearly premium, payable at end of year of death
  output$jwla_yp_end_plot<-renderPlot({
    reserve_jwla_yp_end <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_yp_end<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_yp_end[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwla_yp_end()*(1-input$premium_expense/100)*annuity_due_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwla_yp_end[t]<-t                
    }                
    reserve_jwla_yp_end[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_yp_end,reserve_jwla_yp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_yp_end(),digits=4)))
  })
  
  
  #Yearly premium, payable immediately on death
  output$jwla_yp_i_plot<-renderPlot({
    reserve_jwla_yp_i <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_yp_i<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_yp_i[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*((1+input$interest_rate)^0.5)*(1+input$claim_expense/100)-
        jwla_yp_i()*(1-input$premium_expense/100)*annuity_due_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwla_yp_i[t]<-t                
    }                
    reserve_jwla_yp_i[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_yp_i,reserve_jwla_yp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_yp_i(),digits=4)))
  })
  
  
  #Monthly premium, payable at end of year of death
  output$jwla_mp_end_plot<-renderPlot({
    reserve_jwla_mp_end <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_mp_end<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_mp_end[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwla_mp_end()*(1-input$premium_expense/100)*12*annuity_due_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwla_mp_end[t]<-t                
    }                
    reserve_jwla_mp_end[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_mp_end,reserve_jwla_mp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_mp_end(),digits=4)))
  })
  
  
  #Monthly premium, payable immediately on death
  output$jwla_mp_i_plot<-renderPlot({
    reserve_jwla_mp_i <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwla_mp_i<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwla_mp_i[t+1]<-input$death_benefit_jwla*assurance_xy(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*((1+input$interest_rate)^0.5)*(1+input$claim_expense/100)-
        jwla_mp_i()*(1-input$premium_expense/100)*12*annuity_due_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwla_mp_i[t]<-t                
    }                
    reserve_jwla_mp_i[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwla_mp_i,reserve_jwla_mp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwla_mp_i(),digits=4)))
  })
  
  
  ##JOINT TERM LIFE ASSURANCES------------------------------------------------------------------------------------------------------------------------------------- 
  #Single premium, payable at end of year of death
  output$jta_sp_end_plot<-renderPlot({
    reserve_jta_sp_end <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_sp_end<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_sp_end[t+1]<-input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jta-t)*(1+input$claim_expense/100)
      seniority_jta_sp_end[t]<-t                
    }                
    reserve_jta_sp_end[1]<-0   
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_sp_end,reserve_jta_sp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_sp_end(),digits=4)))
  })
  
  #Single premium, payable immediately on death
  output$jta_sp_i_plot<-renderPlot({
    reserve_jta_sp_i <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_sp_i<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_sp_i[t+1]<-input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jta-t)*(1+input$claim_expense/100)*((1+input$interest_rate)^0.5)
      seniority_jta_sp_i[t]<-t                
    }                
    reserve_jta_sp_i[1]<-0       
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_sp_i,reserve_jta_sp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_sp_i(),digits=4)))
  })
  
  #Yearly premium, payable at end of year of death
  output$jta_yp_end_plot<-renderPlot({
    reserve_jta_yp_end <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_yp_end<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_yp_end[t+1]<-input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t))*(1+input$claim_expense/100)-
        (jta_yp_end()*(1-input$premium_expense/100)*annuity_due_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t)))
      seniority_jta_yp_end[t]<-t                
    }                
    reserve_jta_yp_end[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_yp_end,reserve_jta_yp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_yp_end(),digits=4)))
  })
  
  #Yearly premium, payable immediately on death
  output$jta_yp_i_plot<-renderPlot({
    reserve_jta_yp_i <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_yp_i<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_yp_i[t+1]<-input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t))*(1+input$claim_expense/100)*((1+input$interest_rate)^0.5)-
        (jta_yp_i()*(1-input$premium_expense/100)*annuity_due_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t)))
      seniority_jta_yp_i[t]<-t                
    }                
    reserve_jta_yp_i[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_yp_i,reserve_jta_yp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_yp_i(),digits=4)))
  })
  
  #Monthly premium, payable at end of year of death
  output$jta_mp_end_plot<-renderPlot({
    reserve_jta_mp_end <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_mp_end<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_mp_end[t+1] <- input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t))*(1+input$claim_expense/100)-
        (jta_mp_end()*(1-input$premium_expense/100)*12*annuity_due_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t)))
      seniority_jta_mp_end[t]<-t                
    }                
    reserve_jta_mp_end[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_mp_end,reserve_jta_mp_end,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_mp_end(),digits=4)))
  })
  
  #Monthly premium, payable immediately on death
  output$jta_mp_i_plot<-renderPlot({
    reserve_jta_mp_i <-c((input$main_age-input$main_age):input$contract_term_jta)
    seniority_jta_mp_i<-c((input$main_age-input$main_age):input$contract_term_jta)
    for (t in c((input$main_age-input$main_age):input$contract_term_jta)) {
      reserve_jta_mp_i[t+1]<-input$death_benefit_jta*assurance_xy1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t))*(1+input$claim_expense/100)*((1+input$interest_rate)^0.5)-
        (jta_mp_i()*(1-input$premium_expense/100)*12*annuity_due_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=(input$contract_term_jta-t)))
      seniority_jta_mp_i[t]<-t                
    }                
    reserve_jta_mp_i[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jta_mp_i,reserve_jta_mp_i,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jta_mp_i(),digits=4)))
  })
  
  
  ##JOINT WHOLE LIFE ANNUITY-----------------------------------------------------------------------------------------------
  #Single premium, annuity payable yearly in arrears
  output$jwlan_sp_y_plot<-renderPlot({
    reserve_jwlan_sp_y <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_sp_y<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_sp_y[t+1]<-input$annuity_payment_jwlan*annuity_arrears_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)
      seniority_jwlan_sp_y[t]<-t                
    }                
    reserve_jwlan_sp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_sp_y,reserve_jwlan_sp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_sp_y(),digits=4)))
  })
  
  
  #Single premium, annuity payable monthly in arrears
  output$jwlan_sp_m_plot<-renderPlot({
    reserve_jwlan_sp_m <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_sp_m<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_sp_m[t+1]<-input$annuity_payment_jwlan*12*annuity_arrears_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)
      seniority_jwlan_sp_m[t]<-t                
    }                
    reserve_jwlan_sp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_sp_m,reserve_jwlan_sp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_sp_m(),digits=4)))
  })
  
  
  #Yearly premium, annuity payable yearly in arrears
  output$jwlan_yp_y_plot<-renderPlot({
    reserve_jwlan_yp_y <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_yp_y<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_yp_y[t+1]<-input$annuity_payment_jwlan*annuity_arrears_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwlan_yp_y()*(1-input$premium_expense/100)*annuity_due_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwlan_yp_y[t]<-t                
    }                
    reserve_jwlan_yp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_yp_y,reserve_jwlan_yp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_yp_y(),digits=4)))
  })
  
  
  #Yearly premium, annuity payable monthly in arrears
  output$jwlan_yp_m_plot<-renderPlot({
    reserve_jwlan_yp_m <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_yp_m<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_yp_m[t+1]<-input$annuity_payment_jwlan*12*annuity_arrears_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwlan_yp_m()*(1-input$premium_expense/100)*annuity_due_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwlan_yp_m[t]<-t                
    }                
    reserve_jwlan_yp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_yp_m,reserve_jwlan_yp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_yp_m(),digits=4)))
  })
  
  
  #Monthly premium, annuity payable yearly in arrears
  output$jwlan_mp_y_plot<-renderPlot({
    reserve_jwlan_mp_y <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_mp_y<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_mp_y[t+1]<-input$annuity_payment_jwlan*annuity_arrears_xym1(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwlan_mp_y()*(1-input$premium_expense/100)*12*annuity_due_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwlan_mp_y[t]<-t                
    }                
    reserve_jwlan_mp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_mp_y,reserve_jwlan_mp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_mp_y(),digits=4)))
  })
  
  
  #Monthly premium, annuity payable monthly in arrears
  output$jwlan_mp_m_plot<-renderPlot({
    reserve_jwlan_mp_m <-c((input$main_age-input$main_age):(100-input$main_age))
    seniority_jwlan_mp_m<-c((input$main_age-input$main_age):(100-input$main_age))
    for (t in c((input$main_age-input$main_age):(100-input$main_age))) {
      reserve_jwlan_mp_m[t+1]<-input$annuity_payment_jwlan*12*annuity_arrears_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)*(1+input$claim_expense/100)-
        jwlan_mp_m()*(1-input$premium_expense/100)*12*annuity_due_xym12(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate)
      seniority_jwlan_mp_m[t]<-t                
    }                
    reserve_jwlan_mp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jwlan_mp_m,reserve_jwlan_mp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jwlan_mp_m(),digits=4)))
  })
  
  
  ##JOINT TERM WHOLE LIFE ANNUITY----------------------------------------------------------------------------------------------
  #Single premium, annuity payable yearly in arrears
  output$jtan_sp_y_plot<-renderPlot({
    reserve_jtan_sp_y <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_sp_y<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_sp_y[t+1]<-input$annuity_payment_jtan*annuity_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100)
      seniority_jtan_sp_y[t]<-t                
    }                
    reserve_jtan_sp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_sp_y,reserve_jtan_sp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_sp_y(),digits=4)))
  })
  
  
  #Single premium, annuity payable monthly in arrears
  output$jtan_sp_m_plot<-renderPlot({
    reserve_jtan_sp_m <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_sp_m<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_sp_m[t+1]<-input$annuity_payment_jtan*12*annuity_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100)
      seniority_jtan_sp_m[t]<-t                
    }                
    reserve_jtan_sp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_sp_m,reserve_jtan_sp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_sp_m(),digits=4)))
  })
  
  
  #Yearly premium, annuity payable yearly in arrears
  output$jtan_yp_y_plot<-renderPlot({
    reserve_jtan_yp_y <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_yp_y<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_yp_y[t+1]<-input$annuity_payment_jtan*annuity_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100) -
        jtan_yp_y()*(1-input$premium_expense/100)*annuity_due_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)
      seniority_jtan_yp_y[t]<-t                
    }                
    reserve_jtan_yp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_yp_y,reserve_jtan_yp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_yp_y(),digits=4)))
  })
  
  
  #Yearly premium, annuity payable monthly in arrears
  output$jtan_yp_m_plot<-renderPlot({
    reserve_jtan_yp_m <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_yp_m<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_yp_m[t+1]<-input$annuity_payment_jtan*12*annuity_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100) -
        jtan_yp_m()*(1-input$premium_expense/100)*annuity_due_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)
      seniority_jtan_yp_m[t]<-t                
    }                
    reserve_jtan_yp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_yp_m,reserve_jtan_yp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_yp_m(),digits=4)))
  })
  
  
  #Monthly premium, annuity payable yearly in arrears
  output$jtan_mp_y_plot<-renderPlot({
    reserve_jtan_mp_y <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_mp_y<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_mp_y[t+1]<-input$annuity_payment_jtan*annuity_xym1_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100) -
        jtan_mp_y()*(1-input$premium_expense/100)*12*annuity_due_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)
      seniority_jtan_mp_y[t]<-t                
    }                
    reserve_jtan_mp_y[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_mp_y,reserve_jtan_mp_y,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_mp_y(),digits=4)))
  })
  
  
  #Monthly premium, annuity payable monthly in arrears
  output$jtan_mp_m_plot<-renderPlot({
    reserve_jtan_mp_m <-c((input$main_age-input$main_age):input$contract_term_jtan)
    seniority_jtan_mp_m<-c((input$main_age-input$main_age):input$contract_term_jtan)
    for (t in c((input$main_age-input$main_age):input$contract_term_jtan)) {
      reserve_jtan_mp_m[t+1]<-input$annuity_payment_jtan*12*annuity_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)*(1+input$claim_expense/100) -
        jtan_mp_m()*(1-input$premium_expense/100)*12*annuity_due_xym12_n(agex=(input$main_age+t), agey=(input$partner_age+t), i=input$interest_rate, n=input$contract_term_jtan-t)
      seniority_jtan_mp_m[t]<-t                
    }                
    reserve_jtan_mp_m[1]<-0
    par(bg='#4e5d6c',fg='white', font.lab=2,font.axis=2)
    plot(seniority_jtan_mp_m,reserve_jtan_mp_m,type="l",col="sandybrown",col.axis='#ebebeb', col.main='#ebebeb', col.lab="#ebebeb", lwd=3, xlab="Seniority (years)", ylab="Reserve ($)", main=paste("Evolution of Reserve\nPremium =",round(jtan_mp_m(),digits=4)))
  })
  
}




shinyApp(ui, server)