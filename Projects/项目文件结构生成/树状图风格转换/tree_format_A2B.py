import sys
from pathlib import Path


class TreeNode:
    def __init__(self, name):
        self.name = name
        self.children = []

def parse_tree(lines):
    """解析缩进文本构建树结构"""
    root = TreeNode("")
    stack = [(root, -1)]
    
    
    for line in lines:
        if line.strip() == "": 
            continue
            
        # 处理缩进级别
        raw_line = line
        # 将制表符转换为空格
        line = line.expandtabs(2)
        
        # 计算实际缩进级别
        # 每个缩进单位为2个字符(' -' 或 '  ')
        indent_str = line[:len(line) - len(line.lstrip())]
        indent = len(indent_str) // 2
        
        # 提取节点名称
        name = raw_line.lstrip()  # 移除开头的空格
        if name.startswith('- '):  # 检查是否以'- '开始
            name = name[2:]       # 移除'- '前缀 (刚好2个字符)
        name = name.strip()       # 移除剩余的空格
        # 在name前增加一个空格
        name = " " + name
        
        # 项目路径特殊处理
        if "project_dir:" in name:
            root.name = name
            continue
            
        # 创建新节点
        node = TreeNode(name)
        
        # 调整堆栈找到父节点
        while stack and stack[-1][1] >= indent:
            stack.pop()
            
        # 添加到父节点
        stack[-1][0].children.append(node)
        stack.append((node, indent))
        
    return root



def print_tree(node, prefix="", result=None, is_last=False):
    """生成树状图输出"""
    if result is None:
        result = []
        
    if not node.name:
        result.append(node.name)
    else:
        if prefix:
            # 使用└──替代最后一个节点的├──
            branch = " └──" if is_last else " ├──"
            result.append(f"{prefix[:-4]}{branch}{node.name}")
        else:
            result.append(node.name)
            
    for i, child in enumerate(node.children):
        is_last_child = (i == len(node.children) - 1)
        # 如果当前节点是最后一个，则后续缩进使用空格
        if is_last:
            new_prefix = prefix + "    "
        else:
            new_prefix = prefix + ("    " if is_last_child else " │  ")
        print_tree(child, new_prefix, result, is_last_child)
    
    return result

def main():
    """转换缩进文本为树状图格式"""
    import argparse
    parser = argparse.ArgumentParser(description='将缩进文本转换为树状图格式')
    parser.add_argument('-i', '--input', default='项目文件结构.txt', help='输入文件路径')
    parser.add_argument('-o', '--output', default='tree_output.txt', help='输出文件路径')
    args = parser.parse_args()
    
    with open(args.input, "r", encoding="utf-8") as f:
        lines = f.readlines()
    
    root = parse_tree(lines)
    tree_lines = print_tree(root)
    
    Path(args.output).write_text("\n".join(tree_lines), encoding="utf-8")
    print(f"\n树状图已保存到: {args.output}")

if __name__ == "__main__":
    main()