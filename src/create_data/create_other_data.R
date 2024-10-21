#population
trust_pop <- read.csv('const/raw_data/trust_pop.csv') |>
  rename('trust_code'=TrustCode) |>
  filter(CatchmentYear == 2020)

#workforce
workforce_data <- read.csv('const/raw_data/FINAL_workforce.csv') |>
  dplyr::mutate(date = zoo::as.yearqtr(date)) |>
  janitor::clean_names() |>
  dplyr::mutate(nurses = nurses_health_visitors,
                senior_doctors = consultant+associate_specialist+staff_grade+specialty_doctor,
                managers = managers + senior_managers,
                all_ftes = total,
                doctors = hchs_doctors- senior_doctors,
                ratio_doctors = senior_doctors / hchs_doctors) |>
  select(date,ratio_doctors,trust_code,doctors,senior_doctors,nurses,managers,all_ftes)

#Overnight beds
overnight_bed_data <- read.csv('const/raw_data/FINAL_overnight_beds.csv') |>
  mutate(date = zoo::as.yearqtr(date))

#sickness rates
sickness_rate_data <- read.csv('const/raw_data/FINAL_sickness.csv') |>
  mutate(date = zoo::as.yearqtr(date))

#los
adjusted_los_data <- read.csv('const/raw_data/los_age_complexity_adjusted.csv')  |> 
  rename('trust_code'=PROCODE) |>
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) |>
  select(trust_code,date,standard_los)

#los unadjusted
unadjusted_los_data <- read.csv('const/raw_data/los_unadjusted.csv') |>
  rename('trust_code'=PROCODE) |>
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) |>
  select(trust_code,date,los_unadjusted)

#emergency proportions
emergency_activity_data <- read.csv('const/raw_data/elective_em_proportions.csv') |>
  rename('trust_code'=PROCODE) |>
  mutate(quarter = substr(cal_quarter,1,2),
         year = substr(cal_quarter,nchar(cal_quarter)-4,nchar(cal_quarter)),
         date = zoo::as.yearqtr(paste0(year,' ',quarter))) |>
  select(trust_code,date,elective_proportion,emergency_proportion)
