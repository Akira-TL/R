# Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)

# 初始化变量,设置文件路径和文件名
file_name <- "probly.csv"
filter_criteria <- c("Almost Certainly", "Very Good Chance", "We Believe", "Likely", "About Even", "Little Chance", "Chances Are Slight", "Almost No Chance")
file_type = ".png"

# 判断系统环境,一键调用
if (Sys.info()[["sysname"]] == "Windows") {
    file_path <- "C:/Users/Akira/desktop/"
    output_file <- paste(file = file_path, file_name, file_type, sep = "")
} else {
    file_path <- "~/project/R/data/"
    output_file <- paste(file = "~/project/R/plot/", file_name, file_type, sep = "")
}

data <- read.csv(paste(file_path, file_name, sep = ""), sep = ",")

# Data is at wide format, we need to make it 'tidy' or 'long'
data <- data %>%
    gather(key = "text", value = "value") %>% # 设置列名称
    mutate(text = gsub("\\.", " ", text)) %>% # 替换点(虽然这数据可能压根没这东西)
    mutate(value = round(as.numeric(value), 0)) %>% # 对数值进行处理
    filter(text %in% filter_criteria) # 筛选条件 "

# Plot
p <- data %>%
    mutate(text = fct_reorder(text, value)) %>% # Reorder data
    ggplot(aes(x = text, y = value, fill = text, color = text)) +
    geom_violin(width = 2.1, size = 0.2) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    theme_ipsum() + # 设置图形主题样式
    theme(
        legend.position = "none" # 表示不显示图例
    ) +
    coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Assigned Probability (%)")

p # 在r中显示
# output_file <- "your_file_name" # 有默认值,非必要不修改
ggsave(output_file, units = "in", dpi = 300, width = 10, height = 4)
