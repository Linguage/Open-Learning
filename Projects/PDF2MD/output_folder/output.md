附录 A 设置 Fortran 开发环
境
在我们深⼊代码之前，让我们先了解⼀下编辑、编译和运⾏ Fortran 程序的
基础知识。我会推荐⼀些我喜欢的⽂本编辑器，并指导您设置完整的
Fortran 开发环境。如果您熟悉 Docker 并且想跳过所有繁琐的设置步骤，请
直接跳转到第 A.4 节，在那⾥我会介绍如何获取现代 Fortran Dockerfile，让
您⽴即上⼿。
A.1 编辑 Fortran 源⽂件
您将编写 Fortran 程序和模块作为纯⽂本⽂件。您可以在您喜爱的⽂本编辑
器中编辑它们。以下是⼀些流⾏的选择：
Vim（Vi IMproved，https://www.vim.org/）是轻量级且功能强⼤的
编辑器，对于初学者来说有⼀定的学习曲线。这是我个⼈的选择。我在
2006 年开始编程时开始使⽤它，从那时起就再也没有换过其他编辑
器。
Emacs（https://www.gnu.org/software/emacs）是⼀个功能强⼤且可
扩展的编辑器，也是仍然在主流使⽤中的最古⽼的应⽤程序之⼀。
Atom（https://atom.io）是由 GitHub 开发的现代、功能丰富的集成
开发环境。Atom 还具有⼀个内置的包管理器，可以向编辑器添加第三
⽅功能。
Visual Studio Code（https://code.visualstudio.com）是另⼀个现代
的、功能齐全的集成开发环境，类似于 Atom。在选择⽂本编辑器时，
⼀个重要的特性是它是否可以⽤不同颜⾊突出显⽰ Fortran 语法。我列
出的所有编辑器都可以做到，⽆论是开箱即⽤还是通过扩展。它们还都
是免费且开源的，因此如果您还没有喜欢的编辑器，我建议您尝试每⼀
个，并看看哪⼀个最舒适。
Fortran 源⽂件的扩展名
虽然 Fortran 标准对 Fortran 源⽂件的扩展名没有强加任何约
束，但编译器供应商已经采⽤了⼏乎通⽤的⼀套规则：
带有后缀 .f、.for 或 .ftn 的⽂件名被解释为固定格式
（FORTRAN 77 及更早版本）的代码。
带有后缀 .f90、.f95、.f03 或 .f08 的⽂件被解释为⾃由格
式。
带有⼤写后缀（例如 .F 或 .F90）的⽂件提⽰编译器对⽂件
进⾏预处理。
本书中编写的所有代码都是⾃由格式的。⾃由格式简单地指的是
在 Fortran 90 标准中引⼊的更⾃由的语法。由于我所知道的所有
编译器都⽀持 .f90 作为⾃由格式代码的通⽤后缀（例如，Intel
编译器默认不⽀持 .f95、.f03 或 .f08），因此我们将在整本书中
都使⽤这个扩展名。
A.2 设置 Fortran 编译器
有⼏个⾼质量的 Fortran 编译器可供选择。⼤多数由商业供应商如英特尔、
Cray 等开发和维护。如果你能使⽤其中之⼀，那就太好了！在阅读本书时，
可以随意使⽤它们。当然，对于任何特定于编译器的设置或使⽤说明，你需
要参考你编译器的⽂档。
另外，作为 GNU 编译器集合（GCC）的⼀部分，还提供了⼀个免费、开源
的  Fortran 编 译 器 ， 即  GNU Fortran 编 译 器 （ gfortran ，
https://gcc.gnu.org/fortran）。在本书的⽰例和练习中，我们将使⽤ GNU
Fortran 编译器。与其他编译器相⽐，gfortran 的优点包括：
免费下载、使⽤和修改
在⼤多数操作系统上易于安装
处于积极开发阶段
实现了⼤多数标准特性，包括⼀些来⾃最新的 Fortran 2018 标准
在编写本书时，出现了更多的开源编译器，并且仍在积极开发中。特别是，
可
以
留
意
 
LFotran
（
https://lfortran.org
）
和
 
Flang
（https://github.com/flang-compiler/flang）。
Linux
在⼤多数 Linux 系统上，可以通过系统软件包管理器轻松安装 gfortran，⽽
⽆需访问外部资源。在基于 DEB 的系统（如 Debian 或 Ubuntu）上，安装
gfortran 就像执⾏
⼀样简单。该命令会为您解决下载和安装步骤，⼀旦命令执⾏完成，
gfortran 就可⽤了。在基于 RPM 的系统（如 Fedora 或 CentOS）上，您可
以这样安装 gfortran：dnf install gcc-gfortran。或者，如果 gfortran 在系统
的
软
件
包
管
理
器
中
不
可
⽤
，
您
可
以
从
https://gcc.gnu.org/wiki/GFortranBinaries 下载⼆进制⽂件。
权限 您需要管理员（root）权限才能使⽤软件包管理器安装编译
器。如果您知道⾃⼰在做什么，并且您的⽤户名已经在sudoers列
表中，只需在我提供的Linux安装命令前加上sudo即可。
macOS
对于 macOS，我建议您使⽤ homebrew 包管理器（https://brew.sh）。⼀
旦在您的系统上设置了 homebrew，安装 Fortran 编译器就像执⾏
这个命令⼀样简单。该命令将安装基本的 GNU Compiler Collection 以及
Fortran 编译器。
Windows
在 Windows 中设置开发环境最简单的⽅法是通过 Windows ⼦系统来运⾏
Linux。这是⼀个在 Windows 10 操作系统中原⽣运⾏的 Ubuntu Linux 实
例。如果您的 Windows 10 已经更新到最新版本，您可以从 Windows 应⽤
商店获取 Ubuntu Linux 系统。⼀旦您安装好并运⾏起来，安装 Fortran 编
译器就很容易了：
apt install gfortran 
 brew install gcc
apt install gfortran
否 则 ， 有 些 ⼈ 成 功 地 在  Windows 上 使 ⽤  Cygwin
（https://www.cygwin.com）来开发 Fortran。
A.3 设置 MPI 库（消息传递接⼜）
在第 1 章中，我使⽤了⼀个处理器之间的数据复制⽰例来演⽰使⽤ MPI 进⾏
并⾏编程。虽然在本书中我们将专注于使⽤ Coarray Fortran（CAF）进⾏并
⾏算法，但我们仍然需要安装 MPI 库，因为它是 GNU 编译器在使⽤
coarrays 时的⼀个依赖项（请参阅“设置 OpenCoarrays”部分）。
我 推 荐 使 ⽤  OpenMPI （ https://www.open-mpi.org ） 或  MPICH
（https://www.mpich.org）作为流⾏、⾼质量且易于使⽤的 MPI 实现。它
们可以通过 Linux 包管理器安装。例如，如果您使⽤ Ubuntu 或其他基于
Debian 的发⾏版，您可以使⽤以下命令安装 OpenMPI：
在基于 RPM 的发⾏版（如 Fedora 或 CentOS）上，输⼊以下命令：
安装完成后，MPI 库会为编译器提供⼀个可执⾏包装器。如果安装正确，您
可以通过在命令提⽰符下输⼊ mpif90 来验证：
不 要 担 ⼼ 这 个 错 误 消 息 。 这 只 是 意 味 着  mpif90 在 幕 后 正 确 调 ⽤ 了
gfortran，并且我们没有向它传递任何源⽂件。
A.4 设置 OpenCoarrays
OpenCoarrays（http://www.opencoarrays.org）提供了 GNU Fortran 编译
器和底层并⾏实现（在我们的案例中是 MPI）之间的接⼜；你不需要知道更
多。把它想象成 gfortran 的⼀个扩展，它将允许你使⽤ coarrays 构建和运⾏
apt install openmpi-bin libopenmpi-dev 
dnf install openmpi openmpi-devel 
mpif90 
gfortran: fatal error: no input files compilation 
terminated. 
并⾏程序。如果你已经可以访问带有 Intel 或 Cray 编译器套件的计算平台，
你就不需要 OpenCoarrays，可以跳到下⼀节。
Linux
直接从其 GitHub 仓库获取 OpenCoarrays 发⾏版：
通过源代码构建 OpenCoarrays 是最简单快速的⽅式。考虑到我们已经构建
了 gfortran 和 OpenMPI，编译 OpenCoarrays 相对来说⽐较简单：
你还需要 CMake（https://cmake.org）来构建 OpenCoarrays，并且需要
root 权限在你的系统上执⾏ make install。截⾄本⽂撰写时，OpenCoarrays
的 最 新 版 本 是  2.9.0 。 然 ⽽ ， 保 持 关 注 他 们 的 发 布 页 ⾯
（http://mng.bz/eQBG），如果有更新版本的话，请下载最新版本。
macOS
在 macOS 上使⽤ brew 安装 OpenCoarrays ⾮常简单：
使⽤ OpenCoarrays
OpenCoarrays 提供两个可执⾏⽂件：
caf — ⽤于编译 Coarray Fortran 程序的包装脚本
cafrun — ⽤于运⾏ Coarray Fortran 程序的包装脚本
git clone --branch 2.9.0 
https://github.com/sourceryinstitute/OpenCoarrays
cd OpenCoarrays 
mkdir build 
cd build
FC=gfortran CC=gcc cmake .. 
make 
make install
brew install opencoarrays
编译 CAF 程序时，我们将使⽤ caf 作为我们的编译器的替代品；例如：
要运⾏ CAF 程序，您将使⽤ cafrun 脚本调⽤它：
此命令在两个并⾏进程上调⽤ array_copy_caf 程序。如果计算机上有两个
物理处理器，则两者都将被使⽤。否则，cafrun 将在同⼀处理器上⽣成两个
并⾏线程运⾏。这些细节不会影响程序的语义。
为什么我们需要 OpenCoarrays？
Gfortran 完全⽀持 2008 年标准的 Fortran Coarray 语法。然
⽽，单独使⽤ gfortran 还没有内置的机制来进⾏使⽤ coarrays 的
并⾏计算。这意味着，使⽤普通的 gfortran，您可以编译 coarray
程序，但只能以单个图像（串⾏）模式运⾏它们。虽然这对于早
期开发和测试可能很有⽤，但我们需要能够在多个图像上并⾏运
⾏我们的程序。这就是 OpenCoarrays 的作⽤所在。
请注意，只有当您使⽤ gfortran 时才需要 OpenCoarrays。如果
您使⽤的系统已经配置了 Intel 或 Cray 编译器套件，则可以使⽤
构建 Coarray Fortran 代码。
A.5 构建 Docker 镜像
如果您熟悉 Docker，并希望跳过所有这些繁琐的设置，直接开始操作，请从
http://mng.bz/pBZR 下载 Dockerfile。要构建现代 Fortran 镜像，请键⼊：
此步骤需要⼀段时间，因为 Docker 将拉取基本操作系统镜像，并使⽤本书
中的编译器、依赖项和 Fortran 代码设置镜像。⼀旦完成，如果构建成功，
您将能够看到新的镜像；例如：
caf array_copy_caf.f90 -o array_copy_caf
cafrun -n 2 array_copy_caf
docker build . -t modern-fortran:latest
要运⾏它，请键⼊：
然后您就可以开始了！
docker images
REPOSITORY           TAG       IMAGE ID       CREATED          
SIZE
modern-fortran       latest    0e5c745c8928   6 minutes ago    
546MB
docker run -it modern-fortran:latest /bin/bash
