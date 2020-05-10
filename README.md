weibo-export
============

[![Build Status](https://travis-ci.org/easoncxz/weibo-export.svg?branch=master)](https://travis-ci.org/easoncxz/weibo-export)

A CLI tool to help you download posts from your own [weibo.com][weibo] account.

一个帮助你下载你自己的微博的命令行工具。

Since most people interested in this are expected to be Chinese-language
speakers, the rest of this document is written in Chinese only. The
command-line help docs via `weibo-export --help` is in English and English
only.

# 用途

这是我主要写给自己用的。我希望能尽可能多地下载保留自己微博帐号上的信息，
以对可能的炸号做准备。

# 半自动工具

这个工具的工作原理是模仿 m.weibo.cn 的手机网页版 JavaScript 客户端。
使用过程需要桌面浏览器辅助，最好是 Firefox.

# 安装

### Mac OS

欢迎通过 [Homebrew][brew] 安装：

    brew install easoncxz/tap/weibo-export

或者可以遵以下 Linux 用户的指示，从源代码编译。

### Linux

抱歉，我尚未研究好如何自动发布预编译版本给 Linux 用户。烦请安装
Haskell 的 build tool 自行编译：

    git clone git@github.com:easoncxz/weibo-export.git
    cd weibo-export
    ./automation/install-stack.sh
    stack build
    stack exec weibo-export -- --help

可以安装到用户全局使用：

    stack install

以上命令将会放置一个可执行文件到 `~/.local/bin`. 如果你喜欢，可以将
这个目录加到你的 `PATH` 环境变量中以方便使用 `weibo-export`.

# 使用

    weibo-export

    Usage: weibo-export ((-v|--version) | (-o|--output-dir ARG)
                        (-f|--headers-file ARG) (-i|--container-id ARG)
                        [-p|--start-from-page ARG] [-w|--no-wait])
      Download Weibo statuses via the m.weibo.cn mobile web API

    Available options:
      -v,--version             Display the version of weibo-export
      -o,--output-dir ARG      Where to put downloaded files. This will be created
                               with `mkdir -p` if it doesn't exist.
      -f,--headers-file ARG    A JSON file containing all HTTP headers to use.
                               Format is Firefox's "Copy Request Headers".
      -i,--container-id ARG    The `containerid` GET query param, copied verbatim
                               from your browser.
      -p,--start-from-page ARG Start downloading from this page (default: 1)
      -w,--no-wait             Don't wait, be aggressive, and risk being
                               rate-limited
      -h,--help                Show this help text

因为我们要模仿 m.weibo.cn 的手机客户端，所以需要用桌面浏览器获取所需的信息如 cookie.

步骤：

- 最好用 Firefox, 因为这个工具能识别 Firefox 自动输出的 HTTP request header 格式。
- 打开浏览器，访问 <https://m.weibo.cn>, 登录。
- 打开调试台，如 `Cmd + Opt + I`, 转至 "Network" 标签。
- 点开你自己的微博页面，确保向下滚动能见到不断有更多微博自动加载。
- 在 Network 标签里找一个请求，关键字 `getIndex?containerid=` 云云。可以在
  "Filter URLs" 搜索框里进行筛选。
- 在下面 Params 标签复制 `containerid` 的值出来记好。我的 `containerid` 看起来
  是像 `2345678901234567_-_WEIBO_SECOND_PROFILE_WEIBO` 这样的。
- 还是在下面，Params 旁边的 Headers 标签里，右键点击 "Response Headers (0 B)"
  或者 "Request Headers (0 B)", 选「复制全部」(Copy all). 将剪贴板里的内容
  粘贴到一个文件里。在 Mac 上可以用 `pbpaste > weibo-headers.json` 这样的
  命令。`weibo-headers.json` 的内容应该看起来像这样：

`weibo-headers.json`, 有部分省略 (`// ...`):

    {
      "Response Headers (0 B)": {
        "headers": [
          {
            "name": "cache-control",
            "value": "no-cache"
          },

          // ... 

        ]
      },
      "Request Headers (0 B)": {
        "headers": [
          {
            "name": "Accept",
            "value": "application/json, text/plain, */*"
          },
          {
            "name": "Accept-Encoding",
            "value": "gzip, deflate, br"
          },

          // ...

        ]
      }
    }

准备好 `containerid` 以及 `weibo-headers.json` 之后就可以运行 `weibo-export` 工具了：

    weibo-export \
        -o weibo-downloaded \
        -i '2345678901234567_-_WEIBO_SECOND_PROFILE_WEIBO' \
        -f weibo-headers.json

我个人喜欢用 `tee` 命令在观看输出的同时，记录一份日志，好知道我下载了多少、下载
了那些微博。

    weibo-export \
        -o weibo-downloaded \
        -i '2345678901234567_-_WEIBO_SECOND_PROFILE_WEIBO' \
        -f weibo-headers.json | tee -a weibo-export.log

终端内的日志信息样例：（`0000000000000000` 是我手动打的码）

    Starting download for page 198 of statuses...
    Saved page 198 of statuses
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Starting download for page 199 of statuses...
    Saved page 199 of statuses
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json
    Saved Status to: weibo-downloaded/status-normal-0000000000000000.json

输出文件格式是新浪服务器 API 的一部分，暂时每条微博一个 JSON 文件来储存。
可以用 [`jq`][jq] 进行检阅、分析，和处理。

# 版权协议

BSD 3-clause 协议。详见 [LICENSE](./LICENSE)。

[weibo]: http://weibo.com
[brew]: https://brew.sh/
