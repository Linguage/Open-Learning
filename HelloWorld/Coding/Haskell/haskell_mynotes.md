<!-- 学习 Haskell 语言 -->

# 基本知识

## 类型系统

Haskell 是一个静态类型语言，这意味着编译器在编译代码时会检查类型是否匹配。类型系统有助于避免运行时错误，并使代码更易于理解和维护。

Haskell 的类型系统由以下基本概念组成：

- 类型变量：类型变量是指未知的类型，它可以表示任何类型。类型变量以大写字母开头，如 `a`、`b`、`c`。
  
- 类型构造器：类型构造器是指由类型变量组成的类型，如 `(Int, String)`、`[a]`、`Maybe a`。类型构造器以大写字母开头，以小写字母开头的类型变量表示其参数。
- 类型类：类型类是指一组类型约束，它可以用来描述某些类型具有某些属性。类型类以大写字母开头，以 `class` 关键字定义。
- 类型别名：类型别名是指给已知类型定义一个新的名称，以便于使用。类型别名以 `type` 关键字定义。

## 函数

Haskell 中的函数是第一类值，这意味着它们可以赋值给变量，也可以作为参数传递给其他函数。函数可以有多个参数，参数可以有类型注解。

函数定义语法如下：

```haskell
functionName :: parameterType -> returnType
functionName parameterName = expression
```

函数调用语法如下：

```haskell
functionName argument
```

## 表达式

Haskell 中的表达式可以是值、变量、函数调用、运算符组合、条件表达式、列表构造器、模式匹配等。

## 语句

Haskell 中的语句可以是赋值语句、条件语句、循环语句、函数定义、类型定义等。

## 模块

Haskell 中的模块是组织代码的一种方式。模块可以包含类型定义、函数定义、数据类型定义、类型类定义等。模块可以导入其他模块，并使用模块中的函数、类型、数据类型等。

## 标准库

Haskell 标准库提供了许多常用的函数和数据类型，可以帮助开发者快速编写程序。

## 运行 Haskell 程序

Haskell 程序可以直接运行在命令行中，也可以编译成可执行文件，然后运行。

## 参考资料

- [Haskell 语言指南](https://haskell.org/onlinereport/)


# 学习指南
## 学习 Haskell 语言

Haskell 是一门基于函数式编程语言，具有强大的静态类型系统和惰性求值特性。它被设计为可用于构建可靠、高效、并发和并行的程序。

Haskell 语言的主要特性包括：

- 静态类型系统：Haskell 具有强大的静态类型系统，它可以检测到代码中的类型错误，并提供编译时类型检查。
- 高阶函数：Haskell 具有丰富的高阶函数，可以接受函数作为参数或返回函数。
- 惰性求值：Haskell 的惰性求值特性可以让程序运行更快，因为它不会立即执行函数，而是等待需要时才执行。
- 并发和并行：Haskell 提供了并发和并行编程的支持，可以利用多核 CPU 和分布式计算资源。


## 安装 Haskell 语言

Haskell 语言可以在多种平台上安装，包括 Windows、Mac OS X、Linux 等。

### 安装 Haskell Stack

Haskell Stack 是 Haskell 语言的包管理器，它可以帮助我们安装和管理 Haskell 语言的各种库。

- 安装 Stack：

  ```
  curl -sSL https://get.haskellstack.org/ | sh
  ```

- 配置 Stack：

  ```
  stack setup
  ```

- 安装 Haskell 语言的标准库：

  ```
  stack install ghc
  ```

- 安装其他 Haskell 语言库：

  ```
  stack install package-name
  ```

### 安装 Haskell IDE

Haskell IDE 是 Haskell 语言的集成开发环境（IDE），它可以提供语法高亮、代码自动完成、编译和运行等功能。

- 安装 Haskell IDE：

  - 安装 IntelliJ IDEA Ultimate：https://www.jetbrains.com/idea/download/
  - 安装 HaskForce：https://plugins.jetbrains.com/plugin/12395-haskforce

- 配置 Haskell IDE：

  - 打开 IntelliJ IDEA Ultimate，点击菜单栏的 File -> Settings -> Languages & Frameworks -> Haskell
  - 点击左侧的 Haskell 图标，在右侧的 Haskell SDK 选择安装的 GHC 版本
  - 点击右上角的加号，添加 Haskell 项目，选择项目的位置和名称
  - 点击右上角的运行图标，运行 Haskell 项目

## 学习 Haskell 语言

Haskell 语言的学习资料很多，包括书籍、视频、教程等。下面是一些学习 Haskell 语言的资源：

- 书籍：

  - 《Haskell趣学指南》：https://book.douban.com/subject/26979474/
  - 《Haskell编程从入门到实践》：https://book.douban.com/subject/26914460/
  - 《Haskell程序设计》：https://book.douban.com/subject/1782656/

- 视频：

  - 《Haskell Programming from First Principles》：https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_
  - 《Learn You a Haskell for Great Good》：https://www.youtube.com/playlist?list=PLtLdwf9H3j2C3y2qO9vR0yO7zG31yW7lW
  - 《Haskell Programming for Beginners》：https://www.youtube.com/playlist?list=PLyGKBDfnk-iD9a_y359z1_7v9b9y399e_

- 教程：

  - 《Haskell Programming》：https://www.haskell.org/haskellwiki/Haskell_Programming
  - 《Haskell Programming Language》：https://www.youtube.com/playlist?list=PLyGKBDfnk-iD9a_y359z1_7v9b9y399e_
  - 《Haskell Programming from First Principles》：https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_
  
  - 《Learn You a Haskell for Great Good》：https://learnyouahaskell.com/chapters
  - 《Real World Haskell》：http://book.realworldhaskell.org/read/
  - 《Haskell Programming from First Principles》：https://www.youtube.com/playlist?list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_



## 参考资料

- https://www.haskell.org/
- https://www.haskell.org/documentation/
- https://www.haskell.org/community/
- https://www.haskell.org/learn/


