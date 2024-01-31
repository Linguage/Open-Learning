
# 基本语法

参考教程：[使用$\LaTeX$的笔记](https://blog.csdn.net/qq_326324545/article/details/90232073)

## 基本文档结构

``` latex
\documentclass[UTF8]{ctexart}
\title{你好，world!}
\author{Dong}
\date{\today}
\begin{document}
\maketitle
\tableofcontents
\section{你好中国}
中国在East Asia.
\subsection{Hello Beijing}
北京是capital of China.
\subsubsection{Hello Dongcheng District}
\paragraph{Tian'anmen Square}
is in the center of Beijing
\subparagraph{Chairman Mao}
is in the center of 天安门广场。
\subsection{Hello 辽宁}
\paragraph{大连理工} is one of the best university in 辽宁。
\end{document}
```

## 字体定制

``` latex
% 中文字题
\documentclass{article}
\usepackage{xeCJK} %调用 xeCJK 宏包
\setCJKmainfont{SimSun} %设置 CJK 主字体为 SimSun （宋体）
\begin{document}
你好，world!
\end{document}
```

## 页面设计
``` latex
% 页面大小
\usepackage{geometry}
\geometry{papersize={20cm,15cm}}
\geometry{left=1cm,right=2cm,top=3cm,bottom=4cm}
% 页眉页脚
\usepackage{fancyhdr}
\pagestyle{fancy}
\lhead{\author}
\chead{\date}
\rhead{152xxxxxxxx}
\lfoot{}
\cfoot{\thepage}
\rfoot{}
\renewcommand{\headrulewidth}{0.4pt}
\renewcommand{\headwidth}{\textwidth}
\renewcommand{\footrulewidth}{0pt}
% 行间距
\usepackage{setspace}
\onehalfspacing
%段间距
\addtolength{\parskip}{.4em}

```


# 模板


- [x] 日记本
- [ ] 图书模板
- [ ] 论文模板
- [ ] beamer
- [ ] 竖排文字模板

