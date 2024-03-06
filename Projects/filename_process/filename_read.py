import os
import re

def find_files_with_extension(folder_path, extension):
    files_with_extension = []
    pattern = re.compile(r'\.dict\.yaml$', re.IGNORECASE)
    for root, dirs, files in os.walk(folder_path):
        for file in files:
            if pattern.search(file):
                files_with_extension.append(os.path.join(root, file))
    return files_with_extension

# 测试代码
folder_path = 'C:\Users\peakh\Downloads\Dicts\rime'
extension = '.dict.yaml'
files = find_files_with_extension(folder_path, extension)
for file in files:
    print(file)
