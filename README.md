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

这是我主要写给自己用的。我希望能尽可能多地下载保留自己微博帐号上的信息， 以对可能的炸号做准备。

# 安装

### Mac OS

欢迎通过 [Homebrew][brew] 安装：

    brew install easoncxz/tap/weibo-export

或者可以遵以下 Linux 用户的指示，从源代码编译。

### Linux

抱歉，我尚未研究好如何自动发布预编译版本给 Linux 用户。烦请安装 Haskell 的 build tool 自行编译：

    git clone git@github.com:easoncxz/weibo-export.git
    cd weibo-export
    ./automation/install-stack.sh
    stack build
    stack exec weibo-export -- --help

可以安装到用户全局使用：

    stack install

以上命令将会放置一个可执行文件到 `~/.local/bin`. 如果你喜欢，可以将这个目录加到你的 `PATH` 环境变量中以方便使用 `weibo-export`.

# 使用

    weibo-export: 0.2.0.0

    Usage: weibo-export ((-v|--version) | (-u|--user ARG) [-p|--start-from-page ARG]
                        (-o|--output-dir ARG) [-w|--no-wait])
      Download Weibo statuses via the m.weibo.cn mobile web API

    Available options:
      -v,--version             Display the version of weibo-export
      -u,--user ARG            User ID on weibo, e.g. "3563717322"
      -p,--start-from-page ARG Start downloading from this page (default: 1)
      -o,--output-dir ARG      Where to put downloaded files. This will be created
                               with `mkdir -p` if it doesn't exist.
      -w,--no-wait             Don't wait, be aggressive, and risk being
                               rate-limited
      -h,--help                Show this help text

请上微博找到你的用户 ID. 这是一串大概 10 位长的数字。比如，`1537790411` 好象是一个叫「鹿晗」的人的 user ID. 一般 user ID 会出现在浏览器地址栏里，比如：

    https://m.weibo.cn/u/1537790411

复制了你的用户 ID 之后，可以开始使用本工具。

    weibo-export \
        -o weibo-downloaded \
        -u 1537790411

我个人喜欢用 `tee` 命令在观看输出的同时，记录一份日志，好知道我下载了多少、下载了哪些微博。

    weibo-export \
        -o weibo-downloaded \
        -u 1537790411 | tee -a weibo-export.log

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

可以随时用 Ctrl-C 来中断下载；已下载的文件不会丢失。也可以任意地重新下载；相应的输出文件会被覆盖。

输出文件格式是新浪服务器 API 的一部分，暂时每条微博一个 JSON 文件来储存。
可以用 [`jq`][jq] 进行检阅、分析，和处理。

# 版权协议

BSD 3-clause 协议。详见 [LICENSE](./LICENSE)。

[weibo]: http://weibo.com
[brew]: https://brew.sh/
