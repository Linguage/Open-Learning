
```chatgpt
请整理如下latex公式，主要修改下标.

下标中的t s c p w 以及数字均有物理实体含义，应当使用\mathrm命令，而x y z表示方向，应为斜体，不需要添加\mathrm命令。

例如，M_c\ddot{Z}_c改为M_\mathrm{c}\ddot{Z}_\mathrm{c}, K_{\mathrm{pz}}应当修改为K_{\mathrm{p}z}，C_{sz}应当修改为C_{\mathrm{s}z}.

每一组公式取消换行，也取消公式编号（比如\quad(2.10）),但输出的时候需要每组都通过$$..$$的形势输出：
```

