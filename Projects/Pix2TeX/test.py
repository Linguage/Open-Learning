from PIL import Image
from pix2tex.cli import LatexOCR

img = Image.open('Images/test2.png')
model = LatexOCR()
print(model(img))