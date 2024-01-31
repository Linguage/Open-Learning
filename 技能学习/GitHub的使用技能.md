
## 网络访问

在安装号git之后，在shell中输入如下命令。

此处端口为1080，具体端口号以自己的电脑代理为准。

``` git
  git config --global http.proxy 'socks5://127.0.0.1:1080'
  git config --global https.proxy 'socks5://127.0.0.1:1080'
  
```

## 版本

### Git on Shell

直接通过命令行安装git，连同GitHub，对文件进行管理。



### GitHub Desktop

下载GitHub官方的桌面客户端，在可视化界面中对文件进行管理。

## 工作流程


### 基本命令

```git {:line-number}
git push
git pull
git add .
git commit -m "说明文字"
```

### 控制文本

- .gitignore: 在其中列举不用同步的文件夹、文件名或其后缀
- 