# iBiogeobears 中文快速开始

这份指南面向第一次使用 `iBiogeobears` 的普通用户。目标是先跑通一个
单个类群的 BioGeoBEARS 示例分析，再逐步替换成自己的数据。

`iBiogeobears` 不自带 BioGeoBEARS。真正运行模型前，需要另外安装
BioGeoBEARS。即使还没有安装 BioGeoBEARS，也可以先做输入检查、创建项目、
生成输出目录结构。

## 1. 安装 iBiogeobears

在 R 或 RStudio 里运行：

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS")
```

如果你想安装指定测试版本，可以运行：

```r
install.packages("remotes")
remotes::install_github("XuWeiEvo/iBioGeoBEARS@v0.2.1-alpha")
```

加载软件并检查当前电脑环境：

```r
library(iBiogeobears)
check_installation()
```

`check_installation()` 会告诉你哪些部分已经准备好，哪些还需要安装。
HTML 报告需要 Quarto。PDF 报告还需要 TinyTeX、MiKTeX 或 TeX Live。

## 2. 安装 BioGeoBEARS

先查看安装计划：

```r
biogeobears_install_plan()
```

确认后再安装 BioGeoBEARS 和相关依赖：

```r
install_biogeobears(execute = TRUE)
```

安装后重新检查：

```r
check_installation()
check_biogeobears(required = FALSE)
```

如果 BioGeoBEARS 检查通过，就可以运行真实模型。

## 3. 先跑内置示例

建议第一次不要直接用自己的数据。先运行内置示例，确认电脑环境没问题。

创建一个完整示例项目：

```r
project <- create_example_project("ibgb_example")
```

先做 dry run 检查。这个步骤不会真正运行 BioGeoBEARS：

```r
dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

如果 dry run 没有问题，并且 BioGeoBEARS 已安装，再运行真实六模型分析：

```r
result <- run_workflow(project$config, dry_run = FALSE)
```

生成 HTML 报告和两个压缩包：

```r
report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)
```

优先查看这些文件：

```text
reports/summary_report.html
tables/shiny_run_summary.csv
tables/model_comparison.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
```

## 4. 使用图形界面

如果你更习惯点按钮，可以使用 Shiny 图形界面。

先安装 Shiny：

```r
install.packages("shiny")
```

启动软件：

```r
launch_app()
```

推荐操作顺序：

1. 点击 `Create example project`，或者使用 `New project wizard`。
2. 点击 `Refresh setup checks`。
3. 第一次运行时保持 `Dry run` 勾选，然后点击 `Run workflow`。
4. 如果有输入检查问题，先根据提示修复。
5. 只有在 BioGeoBEARS 已安装时，才取消 `Dry run`。
6. 再次点击 `Run workflow`，运行真实分析。
7. 点击 `Render report` 生成报告。
8. 点击 `Create bundle if missing` 生成结果压缩包。
9. 如果需要别人帮你排查问题，点击 `Create diagnostic bundle`。

## 5. 使用自己的数据

最简单的方法是在图形界面里使用 `New project wizard`。

需要准备三个文件：

- Newick 格式的系统树文件。
- geography CSV：每个物种或样本一行，每个地理区域一列。
- regions CSV：区域编号和区域名称。

如果使用 R 命令行，可以这样创建项目：

```r
project <- create_analysis_project(
  path = "my_ibgb_project",
  project_name = "my_clade",
  tree_file = "my_tree.nwk",
  geography_file = "my_geography.csv",
  regions_file = "my_regions.csv"
)
```

自己的数据也必须先做 dry run：

```r
dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)
```

dry run 通过后，再运行真实分析：

```r
result <- run_workflow(project$config, dry_run = FALSE)
```

## 6. 如果运行失败

先查看这些文件：

```text
tables/input_validation.csv
tables/model_run_status.csv
logs/session_info.txt
```

如果已经得到了 `result` 对象，可以创建诊断压缩包：

```r
diagnostics_zip <- bundle_diagnostics(result, overwrite = TRUE)
```

把这个压缩包发给维护者，比单独截图更有用。

如果程序中途停止，没有返回 `result`，请尽量提供这些文件：

```text
analysis.yml
results/<project>/tables/input_validation.csv
results/<project>/tables/model_run_status.csv
results/<project>/logs/session_info.txt
results/<project>/logs/
```

常见问题：

- 提示 BioGeoBEARS 缺失：运行 `biogeobears_install_plan()`，然后运行
  `install_biogeobears(execute = TRUE)`。
- 提示 Shiny 缺失：运行 `install.packages("shiny")`。
- HTML 报告没有生成：安装 Quarto，然后运行 `check_report_environment()`。
- PDF 报告没有生成：安装 TinyTeX、MiKTeX 或 TeX Live。
- 输入检查失败：打开 `tables/input_validation.csv`，查看 `repair_step` 列。
- 某一个模型失败：打开 `tables/model_run_status.csv`，查看失败模型的 log 路径。
- 出现 `+J` caution：这不是运行错误。意思是最优或接近最优的统计模型包含
  founder-event jump dispersal，需要谨慎解释，不能直接把最低 AICc 当作最终生物学结论。

## 7. 最小测试流程

如果只是想确认软件能不能跑通，可以把下面这段完整复制到 R 里：

```r
library(iBiogeobears)

check_installation()

project <- create_example_project("ibgb_example")

dry <- run_workflow(
  project$config,
  dry_run = TRUE,
  require_biogeobears = FALSE
)

result <- run_workflow(project$config, dry_run = FALSE)

report <- render_report(result, format = "html")
results_zip <- bundle_results(result)
diagnostics_zip <- bundle_diagnostics(result)

report
results_zip
diagnostics_zip
```

如果这段代码失败，请把错误信息和 `diagnostics_zip` 发给维护者。

## 8. 结果怎么读

优先看 HTML 报告：

```text
reports/summary_report.html
```

然后再看几个关键表格：

```text
tables/model_comparison.csv
tables/model_sensitivity.csv
tables/model_run_status.csv
```

`model_comparison.csv` 用来看模型拟合结果。`model_sensitivity.csv` 用来看
`+J` 模型是否影响解释。`model_run_status.csv` 用来看每个模型是否成功完成、
是否有 warning、log 文件在哪里。

不要只因为某个模型 AICc 最低，就直接把它写成唯一的生物学结论。特别是
`DEC+J`、`DIVALIKE+J`、`BAYAREALIKE+J` 这类 `+J` 模型，需要结合
`model_sensitivity.csv` 和报告里的 caution 一起解释。

