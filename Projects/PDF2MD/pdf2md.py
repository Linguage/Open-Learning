import os
import PyPDF2
from pdfminer.high_level import extract_text
import markdown

# 1. 解析 PDF 文档并提取文本内容和图像信息
def parse_pdf(file_path, output_folder):
    # 使用 pdfminer 提取文本内容
    text = extract_text(file_path)

    # 使用 PyPDF2 提取图像信息
    pdf_file = open(file_path, 'rb')
    pdf_reader = PyPDF2.PdfReader(pdf_file)

    # 创建存储图像的文件夹
    image_folder = os.path.join(output_folder, 'images')
    os.makedirs(image_folder, exist_ok=True)

    images = []
    for page_num in range(len(pdf_reader.pages)):
        page = pdf_reader.pages[page_num]
        xObject = page['/Resources']['/XObject'].getObject()

        # 提取图像
        for obj in xObject:
            if xObject[obj]['/Subtype'] == '/Image':
                size = (xObject[obj]['/Width'], xObject[obj]['/Height'])
                data = xObject[obj].getData()
                images.append({'size': size, 'data': data})

                # 将图像保存到文件夹中
                image_path = os.path.join(image_folder, f'image_{page_num}_{obj[1:]}.jpg')
                with open(image_path, 'wb') as image_file:
                    image_file.write(data)

    pdf_file.close()
    return text, images

# 2. 将提取的内容转换成 Markdown 格式
def convert_to_markdown(text, output_file):
    # 将文本内容写入 Markdown 文件
    with open(output_file, 'w') as f:
        f.write(text)

# 主函数
def main():
    input_pdf = 'origin_files/Appendix_A.md.pdf'
    output_folder = 'output_folder'
    # 解析 PDF 文件并提取内容
    text, images = parse_pdf(input_pdf, output_folder)

    # 将提取的文本内容转换成 Markdown 格式并保存
    markdown_output_file = os.path.join(output_folder, 'output.md')
    convert_to_markdown(text, markdown_output_file)

if __name__ == "__main__":
    main()

