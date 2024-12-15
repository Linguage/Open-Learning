# -*- coding: utf-8 -*-
# @Author   : SunriseCai
# @Time     : 2021-04-27  17:22 
# @File     : data_processing.py
# @software : PyCharm

import json
import math

X_PI = 3.14159265358979324 * 3000.0 / 180.0


def gcj02_to_bd09(data: list):
    """
    火星坐标系(GCJ-02)转百度坐标系(BD-09)
    谷歌、高德——>百度
    :param data: 火星坐标经纬度
    :return:
    """
    lng, lat = data
    z = math.sqrt(lng * lng + lat * lat) + 0.00002 * math.sin(lat * X_PI)
    theta = math.atan2(lat, lng) + 0.000003 * math.cos(lng * X_PI)
    bd_lng = z * math.cos(theta) + 0.0065
    bd_lat = z * math.sin(theta) + 0.006
    return [bd_lng, bd_lat]


def process_data():
    """processed data、"""
    lng_lat_data = list()
    with open('unprocessed_data.json', encoding='utf-8') as f:
        data = json.loads(f.read())
    for item in data['l']:
        data = [gcj02_to_bd09([float(_) for _ in _['sl'].split(',')]) for _ in item['st']]
        lng_lat_data.append(data)
    print(lng_lat_data)
    return lng_lat_data


process_data()
