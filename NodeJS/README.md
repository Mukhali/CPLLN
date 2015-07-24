![NodeJs](https://github.com/Mukhali/MPLLN/blob/master/NodeJS/Node.js_logo.png)

## 0x00 tips
    
    入口模块，包管理
    process 全局变量
    BOM用于标记一个文本文件使用Unicode编码。
    前端开发用 Yeoman 真的很好。
    Nodejs给Javascript赋予了服务端应用的生命，Jquery让Javascript成为浏览中开发的利器。
    NodeJs 是我见过的第一种如此异步的语言。
    NodeJS里大量的API内部是用C/C++实现的。
    守护进程。


## WebServer

```node.js
var http = require('http');

http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Hello World\n');
}).listen(1337, '127.0.0.1');

console.log('Server running at http://127.0.0.1:1337/');
```

## 文件操作
异步操作是通过回调函数保证的，同步方法名后面有Sync

文件属性读写：fs.stat、fs.chmod、fs.chown等等。

文件内容读写：fs.readFile、fs.readdir、fs.writeFile、fs.mkdir等等。

底层文件操作：fs.open、fs.read、fs.write、fs.close等等。

```
var fs = require('fs');

function copy(src, dst) {
    fs.writeFileSync(dst, fs.readFileSync(src));
}

function main(argv) {
    copy(argv[0], argv[1]);
}

main(process.argv.slice(2));
```

## 网络操作

## 进程操作
cluster模块让每个核上运行一个工作进程，并统一通过主进程监听端口和分发请求。

process.stdin、process.stdout和process.stderr

使用process对象管理自身。

使用child_process模块创建和管理子进程

## Grunt（自动化项目构建工具）
npm install -g grunt-cli        // grunt-cli not grunt
npm install -g grunt            // 全局安装
package.json：是npm项目配置文件
Gruntfile.js：是专门用来配置grunt的配置文件

npm install grunt --save-dev   // --save-dev表示把grunt作为devDependencies写入的package.json中。

grunt-contrib-uglify：压缩js代码                       // grunt
grunt-contrib-concat：合并js文件                       // grunt concat
grunt-contrib-qunit： 单元测试                         // grunt qunit
grunt-contrib-jshint：js代码检查                       // grunt jshint
grunt-contrib-watch： 监控文件修改并重新执行注册的任务 // grunt watch


## Bower（包依赖管理工具）
npm install bower -g            // 全局安装
bower install jquery            // 项目安装 jquery
bower uninstall jquery          // 项目卸载 jquery
bower list                      // 查看依赖
bower update jquery             // 升级 jquery
bower cache list                // 查看缓存
bower info jquery               // 查看库 jquery 的信息
bower lookup jquery             // 查看 jquery 的URL 
bower home jquery               // 自动去 jquery 主页

bower init                      // 开发并发布自己的库
// 生成 bower.json 文件，然后去注册包名。

## Yeoman
Yeoman主要有三部分组成：yo（脚手架工具）、grunt（构建工具）、bower（包管理器）
yo 官方提供了很多 generator。和之前用过的一个应用（IFTTT）很像，提供各种……。

npm install -g yo
yo                              // 输入 yo 后就是交互式选择安装

## Express 

npm install -g express
express -e nodejs-demo
cd nodejs-demo && npm install
