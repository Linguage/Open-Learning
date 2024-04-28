import cmath as cm
import numpy as np
import pygame as pg
import sys
from pygame.locals import *

NN = 20 #截断阶数
factor = 16 #放大系数
path = 'her.txt'

def readMat(path):
    with open(path,'r') as f:
        lines = f.readlines()
    data = []
    for line in lines:
        tmp = line.strip('\n').strip(' ').split('  ')
        data.append(complex(float(tmp[0]),float(tmp[1])))
        #data.append(tmp)
    return np.array(data)
        
##读取txt文件获取坐标第一列为x坐标，第二列为y坐标
data = readMat(path)
data  = data*factor
Npts = data.size
##假设周期为1，计算傅立叶级数的系数
t = np.linspace(1/Npts,1,num = Npts,endpoint = True)
Fn = np.zeros(2*NN+1)*1j
for i in range(2*NN+1):
    Fn[i] = np.sum(data*np.exp(-1j*(i-NN)*2*np.pi*t))/Npts
rho = np.abs(Fn)   #获取指数表达的模和相角
ang = np.angle(Fn)

Xn = np.zeros([Npts,2*NN+1]) #存储每一步的矢量端绝对坐标
Yn = np.zeros([Npts,2*NN+1])
Xn[:,0] = rho[NN]*np.cos(ang[NN])
Yn[:,0] = rho[NN]*np.sin(ang[NN])
for i in range(1,NN+1):
    Xn[:,2*i-1] = rho[NN+i]*np.cos(ang[NN+i]+i*2*np.pi*t)
    Xn[:,2*i] = rho[NN-i]*np.cos(ang[NN-i]-i*2*np.pi*t)
    Yn[:,2*i-1] = rho[NN+i]*np.sin(ang[NN+i]+i*2*np.pi*t)
    Yn[:,2*i] = rho[NN-i]*np.sin(ang[NN-i]-i*2*np.pi*t)
for i in range(1,2*NN+1):
    Xn[:,i] = Xn[:,i]+Xn[:,i-1]
    Yn[:,i] = Yn[:,i]+Yn[:,i-1]

## 预处理
order = 2*NN+1
biasx = 500
biasy = 400
Xn = Xn+biasx
Yn = -Yn+biasy
pts = []
for i in range(Npts):
    pts.append([Xn[i,order-1],Yn[i,order-1]])


## 定义画圆函数
def drawCircles(screen,X,Y,color_line,color_cir):
    lenx = len(X)
    pg.draw.circle(screen,color_cir,(biasx,biasy),np.sqrt((X[0]-biasx)**2+(Y[0]-biasy)**2),1)
    pg.draw.aaline(screen,color_line,(biasx,biasy),(X[0],Y[0]),1)
    for i in range(1,order):
        pg.draw.circle(screen,color_cir,(X[i-1],Y[i-1]),np.sqrt((X[i]-X[i-1])**2+(Y[i]-Y[i-1])**2),1)
        pg.draw.line(screen,color_line,(X[i-1],Y[i-1]),(X[i],Y[i]),1)

pg.init()
FPS = 50
fpsClock = pg.time.Clock()
RED = (255,0,0)
GREEN = (0,255,0)
BLUE = (0,0,255)
BLACK = (0,0,0)
WHITE = (255,255,255)
screen = pg.display.set_mode((2*biasx,2*biasy),flags = pg.RESIZABLE)
pg.display.set_caption('LOVE YOU')

i = 0
while True:
    screen.fill(BLACK)
    if i > 1:
        pg.draw.aalines(screen,RED,False,pts[0:i],4)
    drawCircles(screen,Xn[i,:],Yn[i,:],WHITE,BLUE)
    for event in pg.event.get():
        if event.type == QUIT:
            pg.quit()
            sys.exit()
    i = i+1
    if i == Npts:
        i = 0
    fpsClock.tick(FPS)
    pg.display.update()

# 将生成的动画图像输出为gif格式保存
pg.image.save(screen,'love.gif')
