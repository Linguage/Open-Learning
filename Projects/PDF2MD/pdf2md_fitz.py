import os
import fitz
import markdown

# 1. 解析 PDF 文档并提取文本内容和图片信息
def parse_pdf(file_path, page_numbers, output_folder):
    text = ""
    images = []
    pdf_full = fitz.open(file_path)
    for page_number in page_numbers:
        # 获取指定页码的页面对象
        page = pdf_full.load_page(page_number)

        page_text = page.get_text()

        # 遍历页面中的所有图片对象
        for img_index, img_info in enumerate(page.get_images(full=True)):
            img_dict = pdf_full.extract_image(img_info[0])
            img_bytes = img_dict["image"]
            img_name = f'image_{page_number}_{img_index}.jpg'
            img_path = os.path.join(output_folder, 'images', img_name)

            # 保存图片到文件夹中
            with open(img_path, 'wb') as img_file:
                img_file.write(img_bytes)

            # 将图片信息添加到 images 列表中
            images.append({'name': img_name, 'path': img_path})

            # 在文本中标记图片的名称和链接，包含 images 文件夹
            img_marker = f"![Image]({os.path.join('images', img_name)})"
            img_position = img_info[1]
            page_text = page_text[:img_position] + img_marker + page_text[img_position:]

        # 将当前页面的文本添加到总文本中
        text += page_text

    pdf_full.close()

    return text, images

# 2. 将提取的内容转换成 Markdown 格式
def convert_to_markdown(text, images, output_file):
    with open(output_file, 'w') as f:
        # 将文本内容写入 Markdown 文件
        f.write(text)

        # 在文本末尾插入图片链接
        f.write('\n\n## Images\n\n')
        for img_data in images:
            img_name = img_data['name']
            img_link = f"![Image]({os.path.join('images', img_name)})"
            f.write(f"{img_link}\n")

# 主函数
def main():
    input_pdf = 'origin_files/Modern Fortran Building Efficient Parallel Applications.pdf'
    # 指定需要提取内容的起始页码和结束页码
    start_page = 52
    # [13  27 52 81 111 136 169 197 228 262 290 317 352]
    end_page = 79
    page_numbers = list(range(start_page-1, end_page + 1))
    output_folder = 'ModernFortran_2'


    # 创建输出文件夹和子文件夹
    os.makedirs(os.path.join(output_folder, 'images'), exist_ok=True)

    # 解析 PDF 文件并提取内容
    text, images = parse_pdf(input_pdf, page_numbers, output_folder)

    # 将提取的文本内容和图片转换成 Markdown 格式并保存
    markdown_output_file = os.path.join(output_folder, 'output.md')
    convert_to_markdown(text, images, markdown_output_file)

if __name__ == "__main__":
    main()
