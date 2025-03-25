# 中文LaTeX文档编译指南

本指南介绍如何编译支持中文的LaTeX文档，并提供相关的使用技巧。

## 必要的软件

1. **TeX发行版**：推荐使用以下任一发行版
   - **Windows**: [MiKTeX](https://miktex.org/) 或 [TeX Live](https://tug.org/texlive/)
   - **macOS**: [MacTeX](https://tug.org/mactex/)
   - **Linux**: [TeX Live](https://tug.org/texlive/)

2. **编辑器**：推荐使用
   - [TeXstudio](https://www.texstudio.org/)
   - [Visual Studio Code](https://code.visualstudio.com/) 配合 [LaTeX Workshop 扩展](https://marketplace.visualstudio.com/items?itemName=James-Yu.latex-workshop)
   - [Overleaf](https://www.overleaf.com/) (在线LaTeX编辑平台)

## 编译步骤

1. **使用XeLaTeX编译器**

   要编译中文LaTeX文档，需要使用XeLaTeX编译器，这是因为它能够很好地支持Unicode字符和系统字体。

   命令行编译:
   ```bash
   xelatex ChineseDocument.tex
   biber ChineseDocument  # 处理参考文献
   xelatex ChineseDocument.tex  # 再次编译以更新引用
   xelatex ChineseDocument.tex  # 第三次编译确保所有引用都得到更新
   ```

2. **使用编辑器编译**

   如果使用TeXstudio或VS Code，您可以配置编译链以自动执行上述步骤。具体设置请参考各编辑器的文档。

## 文件说明

本项目包含以下文件：

- `ChineseDocument.tex` - 主LaTeX文档
- `references.bib` - BibTeX格式的参考文献数据库

## 常见问题解决

1. **中文字体问题**

   如果编译时出现找不到中文字体的错误，可以尝试以下方法：
   
   - 在文档类选项中指定字体：`\documentclass[fontset=<字体集>]{ctexart}`，其中`<字体集>`可以是`windows`, `mac`, `fandol`等。
   - 手动设置字体：
     ```latex
     \setCJKmainfont{SimSun} % 设置中文主字体为宋体
     \setCJKsansfont{SimHei} % 设置中文无衬线字体为黑体
     \setCJKmonofont{FangSong} % 设置中文等宽字体为仿宋
     ```

2. **参考文献编译问题**

   如果参考文献没有正确显示：
   
   - 确保执行了完整的编译链（xelatex → biber → xelatex → xelatex）
   - 检查bib文件的编码是否为UTF-8

3. **图片路径问题**

   如果图片无法显示，请检查：
   
   - 图片文件路径是否正确
   - 是否包含了graphicx包
   - 可以使用`\graphicspath{{./images/}}`指定图片文件夹

## 更多资源

- [CTEX宏包文档](https://www.ctan.org/pkg/ctex)
- [LaTeX中文排版指南](https://github.com/CTeX-org/lshort-zh-cn)
- [LaTeX学习资料](https://liam.page/2014/09/08/latex-introduction/)
