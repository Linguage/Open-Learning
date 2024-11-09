import json
from datetime import datetime

def format_message(msg):
    try:
        # 适应可能的不同JSON结构
        if 'role' in msg['message']:
            role = msg['message']['role']
        elif 'author' in msg['message']:
            role = msg['message']['author']['role']
        else:
            role = 'unknown'
            
        content = msg['message']['text']
        
        # 根据角色添加不同的markdown标记
        if role == 'user' or role == 'human':
            return f"## Human:\n{content}\n\n"
        elif role == 'assistant' or role == 'system':
            return f"## Assistant:\n{content}\n\n"
        return f"## {role.capitalize()}:\n{content}\n\n"
    except KeyError as e:
        print(f"警告: 消息格式不正确 - {str(e)}")
        return f"## Unknown:\n(消息格式错误)\n\n"

def json_to_markdown():
    # 读取JSON文件
    with open('chat.json', 'r', encoding='utf-8') as f:
        json_data = json.load(f)
        print("JSON结构:", json.dumps(json_data['requests'][0], indent=2)[:200])  # 打印部分JSON结构以供调试
    
    # 准备markdown内容
    markdown_content = "# Chat History\n\n"
    markdown_content += f"Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n\n"
    
    # 处理所有对话内容
    for request in json_data.get('requests', []):
        markdown_content += format_message(request)
    
    # 写入markdown文件
    with open('output.md', 'w', encoding='utf-8') as f:
        f.write(markdown_content)
    
    print("Markdown文件已生成：output.md")

if __name__ == "__main__":
    json_to_markdown()