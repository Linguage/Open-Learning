# -*- coding: utf-8 -*-
# @Author   : SunriseCai
# @Time     : 2021-04-27  17:12 
# @File     : subway_lines.py
# @software : PyCharm


from pyecharts.charts import BMap
from pyecharts import options as opts
from pyecharts.globals import ChartType

from datas import subway_data


def subwayLines(data: list):
    """、"""
    bmap_chart = (
        BMap(init_opts=opts.InitOpts(width="1900px", height="1060px"))
            .add_schema(
            baidu_ak="你的百度ak",  # 百度ak
            center=[113.280637, 23.125178],  # 当前视角的中心点，用经纬度表示（广州
            zoom=10,  # 当前视角缩放比例
            # is_roam=True,  # 开启鼠标缩放和平移漫游
            map_style={  # 地图样式配置项
                # "styleJson": [
                #     {
                #         "featureType": "water",
                #         "elementType": "all",
                #         "stylers": {"color": "#d1d1d1"},
                #     },
                #     {
                #         "featureType": "land",
                #         "elementType": "all",
                #         "stylers": {"color": "#f3f3f3"},
                #     },
                #     {
                #         "featureType": "railway",
                #         "elementType": "all",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "highway",
                #         "elementType": "all",
                #         "stylers": {"color": "#fdfdfd"},
                #     },
                #     {
                #         "featureType": "highway",
                #         "elementType": "labels",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "arterial",
                #         "elementType": "geometry",
                #         "stylers": {"color": "#fefefe"},
                #     },
                #     {
                #         "featureType": "arterial",
                #         "elementType": "geometry.fill",
                #         "stylers": {"color": "#fefefe"},
                #     },
                #     {
                #         "featureType": "poi",
                #         "elementType": "all",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "green",
                #         "elementType": "all",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "subway",
                #         "elementType": "all",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "manmade",
                #         "elementType": "all",
                #         "stylers": {"color": "#d1d1d1"},
                #     },
                #     {
                #         "featureType": "local",
                #         "elementType": "all",
                #         "stylers": {"color": "#d1d1d1"},
                #     },
                #     {
                #         "featureType": "arterial",
                #         "elementType": "labels",
                #         "stylers": {"visibility": "off"},
                #     },
                #     {
                #         "featureType": "boundary",
                #         "elementType": "all",
                #         "stylers": {"color": "#fefefe"},
                #     },
                #     {
                #         "featureType": "building",
                #         "elementType": "all",
                #         "stylers": {"color": "#d1d1d1"},
                #     },
                #     {
                #         "featureType": "label",
                #         "elementType": "labels.text.fill",
                #         "stylers": {"color": "#999999"},
                #     },
                # ]
            },
        )
            .add(
            series_name="",
            data_pair=data,  # [[[123,20],[123,20],[123,20]], [[123,20],[123,20],[123,20]]]
            type_=ChartType.LINES,  # Geo 图类型，有 scatter, effectScatter, heatmap, lines 4 种
            is_polyline=True,  # 是否是多段线，在画 lines 图情况下
            is_large=True,  # 是否启用大规模线图的优化，在数据图形特别多的时候（>=5k）可以开启
            # 线样式配置项
            linestyle_opts=opts.LineStyleOpts(
                opacity=0.4,  # 图形透明度。支持从 0 到 1 的数字，为 0 时不绘制该图形。
                width=3,  # 线宽
                # type_='dotted',  # 线的类型 'solid', 'dashed', 'dotted'
                color="blue",  # 颜色
            ),
            # 涟漪特效配置选项
            effect_opts=opts.EffectOpts(
                # brush_type='fill',  # 波纹的绘制方式，可选 'stroke' 和'fill'， 。
                period=3,  # 波纹跑一个周期需要的时间，默认4s
                color='red',  # 特效标记的颜色
                # 'circle', 'rect', 'roundRect', 'triangle', 'diamond', 'pin', 'arrow', 'none'
                # 可以通过 'image://url' 设置为图片，其中 URL 为图片的链接，或者 dataURI。
                symbol='circle',  # 特效标记的图形
                symbol_size=10,  # 特效标记的大小，[20, 10] 表示标记宽为 20，高为 10
                trail_length=0.4,  # 特效尾迹的长度，取0到1的值，数值越大尾迹越长
            )
        )
            .add_control_panel(
            # 切换地图类型的空间
            maptype_control_opts=opts.BMapTypeControlOpts(
                position=1,  # 显示位置 0 1 2 3 : 左上 右上 左下 右下
                # 显示位置 0 1 2 : 水平方式,默认 下拉式 图片方式展示类型控件,设置后无法指定 maptypes属性
                type_= 2
            ),
            # 比例尺控件
            scale_control_opts=opts.BMapScaleControlOpts(
                position=2,  # 显示位置 0 1 2 3 : 左上 右上 左下 右下
            ),
            # 缩略地图控件
            overview_map_opts=opts.BMapOverviewMapControlOpts(
                position=3,  # 显示位置 0 1 2 3 : 左上 右上 左下 右下
                # is_open=True,   # 缩略地图添加到地图后的开合状态，默认 False
            ),
            # 地图的平移缩放控件
            navigation_control_opts=opts.BMapNavigationControlOpts(
                # is_enable_geo_location=True,  # 集成定位功能
                # is_show_zoom_info=True,  # 显示级别提示信息,未发现作用
                position=2,  # 显示位置 0 1 2 3 : 左上 右上 左下 右下
                type_=1,  # 0 1 2 3 : 平移、缩放和滑块 平移和缩放 平移 缩放
                offset_height=30,
                offset_width=5
            ),
            # 版权控件
            copyright_control_opts=opts.BMapCopyrightTypeOpts(
                position=3,
                copyright_="<h2>是小菜一碟吖</h2>"  # 文本内容，可以放入HTML标签
            ),
            # 地图定位的控件，使用 HTML5 浏览器定位功能
            geo_location_control_opts=opts.BMapGeoLocationControlOpts(
                # is_enable_auto_location=True,  # 添加控件时是否进行定位,默认False
            )
        )
    )
    return bmap_chart


chart = subwayLines(data=subway_data)
chart.render('render.html')
