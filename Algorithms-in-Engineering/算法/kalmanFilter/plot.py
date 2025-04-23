import plotly as plotly
import plotly.express as px
import numpy as np

x = np.linspace(0, 10, 100)
y = np.sin(x)
fig = px.line(x=x, y=y, title='正弦波示例')
fig.update_layout(xaxis_title='时间 (秒)', yaxis_title='幅度')
fig.show()