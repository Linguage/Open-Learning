class TreeConverter:
    # B格式的符号映射
    B_SYMBOLS = {
        ' ├──': ' ┣',
        ' │  ': ' ┃',
        ' └──': ' ┗',
        '    ': '  '
    }
    
    # C格式的符号映射
    C_SYMBOLS = {
        ' ┣': ' ├──',
        ' ┃': ' │  ',
        ' ┗': ' └──',
        '  ': '    '
    }

    @staticmethod
    def convert_format(content, to_format='C'):
        lines = content.split('\n')
        result = []
        
        # 处理第一行
        if lines[0].startswith('project_dir:'):
            first_line = lines[0].split('/')[-1].strip()
            result.append(first_line)
        else:
            result.append(lines[0])
        
        # 处理其余行
        for line in lines[1:]:
            if to_format == 'C':
                for b_sym, c_sym in TreeConverter.B_SYMBOLS.items():
                    line = line.replace(b_sym, c_sym)
            else:  # to_format == 'B'
                for c_sym, b_sym in TreeConverter.C_SYMBOLS.items():
                    line = line.replace(c_sym, b_sym)
            result.append(line)
        
        return '\n'.join(result)

def main():
    """在B格式和C格式之间转换树状图"""
    import argparse
    parser = argparse.ArgumentParser(description='在B格式和C格式之间转换树状图')
    parser.add_argument('-i', '--input', required=True, help='输入文件路径')
    parser.add_argument('-o', '--output', required=True, help='输出文件路径')
    parser.add_argument('-t', '--to_format', choices=['B', 'C'], required=True, help='目标格式')
    args = parser.parse_args()
    
    with open(args.input, 'r', encoding='utf-8') as f:
        content = f.read()
    
    result = TreeConverter.convert_format(content, args.to_format)
    
    with open(args.output, 'w', encoding='utf-8') as f:
        f.write(result)
    print(f"\n转换完成，结果已保存到: {args.output}")

if __name__ == '__main__':
    main()