def convert_line(line):
    # 移除树状图符号
    line = line.replace('├──', '-').replace('└──', '-').replace('│', '')
    # 计算缩进级别
    indent_level = (len(line) - len(line.lstrip())) // 4
    # 清理行
    clean_line = line.strip()
    # 生成新格式的缩进
    new_indent = "  " * indent_level
    return f"{new_indent}  {clean_line}"

def process_tree(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()

    converted_lines = []
    first_line = True

    for line in lines:
        if first_line:
            converted_lines.append(line)  # 保持第一行不变
            first_line = False
            continue
            
        if line.strip():  # 跳过空行
            converted_line = convert_line(line)
            converted_lines.append(converted_line + '\n')

    with open(output_file, 'w', encoding='utf-8') as f:
        f.writelines(converted_lines)

def main():
    """转换树状图格式为缩进文本格式"""
    import argparse
    parser = argparse.ArgumentParser(description='将树状图格式转换为缩进文本格式')
    parser.add_argument('-i', '--input', default='tree_output.txt', help='输入文件路径')
    parser.add_argument('-o', '--output', default='indent_output.txt', help='输出文件路径')
    args = parser.parse_args()
    
    process_tree(args.input, args.output)
    print(f"\n缩进文本已保存到: {args.output}")

if __name__ == "__main__":
    main()