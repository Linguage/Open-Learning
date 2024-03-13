from abaqus import *
from abaqusConstants import *

# 创建模型
myModel = mdb.Model(name='Model-1')

# 创建装配
myAssembly = myModel.rootAssembly

# 定义参数
startX = 0.0
endX = 36.0
incrementX = 0.1

# 循环创建参考点
for i in range(int((endX - startX) / incrementX) + 1):
    xCoord = startX + i * incrementX
    pointName = 'Point-{}'.format(i)
    # 创建参考点
    myAssembly.ReferencePoint(point=(xCoord, 0.0, 0.0))
    # 获取创建的参考点对象
    refPoint = myAssembly.referencePoints.findAt((xCoord, 0.0, 0.0))
    # 重命名参考点
    refPoint[0].rename(pointName)

print("参考点创建完成。")
