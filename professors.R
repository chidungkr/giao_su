

# Nguồn  dữ liệu sử dụng từ Hội Đồng Chức Danh Giáo Sư Nhà  nước: 
# http://www.hdcdgsnn.gov.vn/index.php/ho-t-d-ng-xet-duy-t/thong-bao/780-danh-sa-ch-a-ng-via-n-a-a-t-tia-u-chua-n-cha-c-danh-gs-pgs-n-m-2017

library(tidyverse)
library(readxl)
library(magrittr)


prof1 <- read_excel("C:/Users/win10ls/Downloads/KETQUA_GS_PGS_dat_nam_2017_WEB.xls", sheet = 1)
prof2 <- read_excel("C:/Users/win10ls/Downloads/KETQUA_GS_PGS_dat_nam_2017_WEB.xls", sheet = 2)


ten <- c("tt",  "name1", "name2", "birth_date", "gender", "field",  "organization", "birth_place")
prof2 %<>% slice(-c(1:5))
names(prof2) <- ten
prof2 %>% head()

prof2$gender %>% table()
prof2$field %>% table()


# Phân bố giới tính: 
theme_set(theme_minimal())
prof2 %>% 
  group_by(gender) %>% 
  count() %>% 
  ggplot(aes(gender, n)) + 
  geom_col()


# Phân bố theo ngành: 
prof2 %>% 
  group_by(field) %>% 
  count() %>% 
  ggplot(aes(reorder(field, n), n)) + 
  geom_col() + 
  coord_flip() + 
  labs(x = NULL, 
       y = NULL,  
       title = "The Associate Professors by Field", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")

# Khảo sát tuổi: 
library(lubridate)

prof2 %<>% mutate(birth_date = dmy(birth_date), 
                  birth_year = year(birth_date), 
                  age = 2018 - birth_year)

prof2 %>% 
  arrange(birth_date) %>% 
  #filter(!is.na(birth_date)) %>% 
  tail(25)

# Tuổi trung bình của  các phó GS theo giới tính
# có thể  thấy các PGS nữ  trẻ hơn đồng nghiệp nam: 
prof2 %>% 
  group_by(gender) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), age)

# Tuổi trung bình của các PGS theo ngành: 
prof2 %>% 
  group_by(field) %>% 
  summarise_each(funs(mean(., na.rm = TRUE)), age) %>% 
  arrange(-age) -> tuoi

# 10 ngành có PGS tuổi cao nhất: 
tuoi %>% head(10)

# PGS có  tuổi cao nhất và thấp  nhất có tuổi tương 
# ứng là 33 và 67: 
prof2 %>% slice(c(which.min(age), which.max(age)))

# Phân bố các PGS theo giới tính và ngành
prof2 %>% 
  group_by(gender, field) %>% 
  count() %>% 
  ggplot(aes(field, n, fill = gender)) + 
  geom_col(position = "fill") + 
  coord_flip() +  
  geom_hline(yintercept = 0.5, color = "purple")



# Muốn vẽ đẹp: 
prof2 %>% 
  group_by(gender, field) %>% 
  count() -> u

u %>% 
  spread(gender, n) -> u_wide



names(u_wide) <- c("field", "Female", "Male")

convert_na <- function(x) {
  x[is.na(x)] <- 0
  return(x)
  }


u_wide %<>% mutate_at(.vars = c("Female", "Male"), 
                      .funs = convert_na)

u_wide %<>% mutate(total = Male + Female, 
                  m_rate = Male / total, 
                  f_rate = Female / total, 
                  label = paste0(100*round(m_rate, 2), "%"))

u_wide %<>% 
  ungroup() %>% 
  arrange(-m_rate) %>% 
  mutate(field = factor(field, levels = field))

u_wide %>% 
  ggplot() + 
  geom_segment(aes(x = 0, 
                   xend = m_rate, 
                   y = field, 
                   yend = field, 
                   color = "m_rate"), size = 4) + 
  geom_segment(aes(x = m_rate, 
                   xend = m_rate + f_rate, 
                   y = field, 
                   yend = field, 
                   color = "f_rate"), size = 4) + 
  geom_text(aes(x = 0, y = field, label = label), 
            color = "white", hjust = -0.2) + 
  labs(x = NULL, y = NULL, 
       title = "Distribution of Associate Professors by Field and Gender", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn") + 
  scale_color_manual(values = c('#e41a1c','#4daf4a'), 
                     name = "", 
                     labels = c("Female", "Male"))


# Phân bố các PGS theo quê quán: 
library(stringr)
province <- function(x) {
  u <- x %>% str_replace_all(".*,", "")
  return(u)
}

prof2 %<>% mutate(tinh = province(birth_place))

prof2 %>% 
  group_by(tinh) %>% 
  count() %>% 
  arrange(-n) %>%
  ungroup() %>% 
  slice(1:30) %>% 
  ggplot(aes(reorder(tinh, n), n)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = 1.2, color = "white") + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors by Birth Place", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")

# Top 30 trường có nhiều PGS được  phong nhất: 
prof2 %>% 
  group_by(organization) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  slice(1:30) %>% 
  ggplot(aes(reorder(organization, n), n)) + 
  geom_col() + 
  coord_flip() + 
  geom_text(aes(label = n), hjust = 1.2, color = "white") + 
  labs(x = NULL, y = NULL, 
       title = "The Number of of Associate Professors by Organization", 
       caption = "Data Source: http://www.hdcdgsnn.gov.vn")
  

