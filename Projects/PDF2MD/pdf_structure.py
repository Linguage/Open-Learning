import PyPDF2

def print_pdf_structure(pdf_file):
    pdf_reader = PyPDF2.PdfReader(pdf_file)
    for page_num in range(len(pdf_reader.pages)):
        page = pdf_reader.pages[page_num]
        print(f"Page {page_num + 1} Resources: {page.get('/Resources')}")

pdf_file = open('origin_files/Appendix_A.md.pdf', 'rb')
print_pdf_structure(pdf_file)
pdf_file.close()
