
## 网络访问

在国内，因为某些客观原因，需要自行想一些办法实现与GitHub的联通。比如，通过设置代理。但是某些代理默认GitHub不需要走代理端口，因此依然无法实现文件的pull与push。对此问题，可以先确认自己的代理端口号，然后按照如下的方法来设置。

1. 安装好git之后，打开git bash。
2. 加入端口为1080，在GitShell中输入如下命令。

``` git
  git config --global http.proxy 'socks5://127.0.0.1:1080'
  git config --global https.proxy 'socks5://127.0.0.1:1080'
```


## 版本

### Git

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