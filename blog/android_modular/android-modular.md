# 一、概述

目前有赞移动端的主要工作内容是在“有赞微商城”和“有赞零售”两条公司主要的业务线，随着有赞 Saas 业务的增长，客户端也不断迭代，支持越来越多的功能。
在这个业务快速增长的情况下，移动端技术的整体架构也是一直在不断调整，来保证开发效率和业务的快速迭代。

这篇文章，主要是介绍有赞微商城 Android组件化的一些思路和实现。

## 1.1 现状

客户端的架构，从一开始的“All IN ONE” 模式（即所有代码都在 App 中），逐渐演变到目前的一个单 Project 多 Module 结构：
![img](https://img.yzcdn.cn/public_files/2019/04/22/97e898b6900446c8c572069b69dbbcc8.jpg)


## 1.2 痛点

新的项目架构，也带了了新的问题：
- 日益复杂的 Common 模块，逻辑复杂，依赖不清晰，不敢随便改动 Common 代码，造成大量冗余代码和无法维护的业务逻辑
- 随着业务模块的增多，打包速度一发不可收拾；从倒杯水的时间到下楼吃个饭的时间，大大减慢了开发节奏
- 由于业务模块跟项目中的上层（App 壳）和下层（Common 模块）耦合
- 业务模块增多，由于业务模块没有自己的生命周期，无法实现模块之间的隔离，整体模块控制比较混乱


## 1.3 需要解决的问题

- Common模块轻量化，需要将 Common 层的业务向上抽离，通用的底层和基础组件、公用 UI 组件抽成单独的依赖
- 移动端业务服务化，解耦现有业务，抽象出业务接口，业务模块只向外暴露自己的接口，并实现跨模块之间的调用
- 能够配置单模块或者多模块打包，不用每次调试都全量打包，费时费力，又影响开发的节奏
- 业务模块的依赖和发布管理

-------------------------------------------------------------------------------

# 二、架构调整

我们之前虽然有在做整个工程模块化的开发，但是目前的模块化框架可以说是不够彻底的：

- 模块只是项目结构的概念（一个模块一个 Module），在逻辑层并没有模块这个概念
- 模块本身并没有生命周期控制
- 公用服务中心化，公用逻辑部分全部都在 Common 模块中
- 模块对外暴露的服务不可知，都是直接依赖模块内部的代码逻辑
- 模块无法单独打包，针对模块的代码改动，只能全量打包之后才能看到效果
    
为了解决以上的问题，我们需要对现有的架构进行调整。


## 2.1模块的抽象

将模块的功能抽象出一些基础类，组成了模块化支持组件，它提供的功能有：

- 抽象出模块本身作为某一类业务的容器，即所有业务模块需要实现自己的模块类，继承自我们的 BaseModule，并在 App 壳工程中进行注册
- 模块对象跟 Activity 一样，拥有生命周期的概念，需要在生命周期的不同阶段处理自己相应的逻辑（注册服务、初始化数据等）
- 模块可以注册的对外暴露的服务的实现，在注册模块的时候，模块携带的服务也会被注册到 App 的服务中心


## 2.2 公共业务去中心化

跟很多客户端的同学聊过，很多 APP 发展到一定阶段之后，必然会诞生一个所谓的 Common 模块。它就像一个大储物柜，每个人都把一些其他人可能用到的东西一股脑儿塞进去。
这么个塞法，会有两个问题：

1. 冗余：比如一些工具类，很多时候，当你找不到需要的工具类的时候，你可能会塞一个新的进去
2. 维护成本高：所有公用的业务逻辑的实现都在 Common 中，对一些公用业务逻辑的影响面无法掌控


### 2.2.1 Common 里面都有什么？

- 工具类
- 公用的 UI 组件
- 多个业务模块都公用的业务类
- 基础组件的封装类（图片库、网络库、Webview）
- 封装的一些基类（BaseActivity，BaseFragment 什么的）

### 2.2.2 解决的思路

- 将公用的业务模块向上抽离到业务模块中（所谓业务模块的服务化）
- 将基础组件抽象到一个独立的组件中
- 将一些基础类下沉到不包含业务逻辑的底层核心库中


## 2.3 业务模块服务化

“服务化”这个词，在服务端的开发中经常被提到，简单来说，就是根据业务划分为多个模块，模块之间的交互以互相提供服务的方式来完成。
而客户端随着业务模块的增多，也必然存在业务模块之间存在业务依赖的情况，而 Android 端常规的模块依赖的方式有：

1. A 模块直接依赖 B 模块，直接调用 B 模块的代码逻辑
2. 将 A 和 B 模块中的公用部分放到 Common 模块中，通过调用 Common 模块的代码实现依赖


### 2.3.1 业务模块服务依赖的实现

- 后端的服务化是借助于 Dubbo 来构建的 RPC 服务，依赖某个服务，只需要依赖其对外暴露的 API 模块（只包含接口和数据结构的 Maven 依赖），不需要依赖其具体实现，具体服务调用的实现由框架来实现
- 客服端的依赖也可以参考这样的方式来实现模块之间的依赖，例如商品模块，可以提供一个 API 层，用来对外暴露数据结构和服务
    ![img](https://img.yzcdn.cn/public_files/2019/04/22/e40be56245ed3c45a833fdee8c168a21.jpg)


### 2.3.2 API 层实现方式

对外暴露服务的方式有很多种：

- 协议的方式：例如"app://order/detail/get?id=100"，数据可以用 JSON 来进行传递，请求本地服务就像调用一个 Http 服务一样，根据请求协议来获取数据，然后解析数据进行操作
- 接口的方式：像后端使用 Dubbo 服务那样，订单模块对外提供一个独立的 Maven 依赖，里面包含了数据接口和对外提供的服务接口，适用方依赖之后直接调用


### 2.3.3 接口的方式实现 API

协议的方式的问题：如果服务提供的地方更改了之后，需要手动去查询所有调用到的地方，进行更改，而且没有版本管理，而且数据解析都需要手动进行转换，改动的成本比较高，也有一定稳定性风险。
接口的方式的问题：需要额外提供一个依赖（单独把 API 层打包成一个 aar 包），使用方需要添加 Mave 依赖，所以引入依赖和发布的成本比较高。

我们最终选择了接口的方式，这种方式的稳定性和版本控制做的更好，对于改动来说，编译过程自动会帮你校验改动的影响面，而引入依赖和发布成本高的问题，完全可以交给构建工具（Gradle Plugin）来解决。


### 2.3.4 业务实现层

业务实现层需要做的，就是实现自己模块本身的业务逻辑，并实现自己提供的 API 接口，暴露对外的服务。

## 2.4 基础组件抽象

### 2.4.1 现有的基础组件实现

项目中现在有很多的基础组件都是统一在 Common 里面进行封装的，例如：账号库、网络库、图片加载、Web 容器库等等，这也带来了一些问题：

1. Common 太重
2. 业务模块跟基础组件强耦合，在开发一些跨团队的组件过程中，如果碰到使用的基础库不同的时候，需要比较多的时间来做封装
3. 升级基础组件或替换依赖的成本比较高，一些 API 的更改需要改动每个调用的地方


### 2.4.2 实现思路

- 将常用的基础组件整理，抽象成单独的一个抽象层，里面定义了一系列基础组件接口（图片加载、Web 容器、JsBridge 调用、账号等等）
- 把统一实现的组件放到另一个依赖里面，可以在 App 中进行具体实现的注册，而业务模块本身，可以只依赖抽象


### 2.4.3 依赖结构

![img](https://img.yzcdn.cn/public_files/2019/04/22/9ce22339b64362d47a10043dc4bb37cb.jpg)


## 2.5 单/多模块打包

随着业务量和业务复杂度的增长，还有多个三方组件的引入，客户端工程代码量也变得越来越庞大，直接造成的一个问题是：打包慢！一个简单的场景：当你开发了一个商品模块内部的功能之后，你需要打整个 App 的包才能进行测试，而打一个包的时间可能是 5～10 分钟，如果一天打包 10 次，也是比较酸爽。我们的组件也需要支持单模块或者选定的某些进行打包，其中的思路也是通过自定义 Gradle Plugin 在编译阶段，动态去更改 Module 实际依赖的 Android Gradle 插件来实现的。
经测试，同一台电脑，完整打包（clean之后再安装）耗时 4 分钟，而单模块打包（同样也是 clean 之后安装）耗时 1 分钟，整体打包时间降低了 70% 以上。


## 2.6 架构图

上面的一些改进点，总结成一张图，就是这样的:
![img](https://img.yzcdn.cn/public_files/2019/04/22/37f332b316f22edf0239838f9d460729.png)

-------------------------------------------------------------------------------

# 三、实现方案

目前我们的方案提供 3 个基础组件依赖和 1 个 Gradle 插件：

- modular-core: 提供组件模块化生命周期和模块服务注册相关的模块化基础组件
- modular-support: 对项目中二方、三方包接口的抽象
- modular-support-impl:对项目中二方、三方包接口的默认抽象
- modular-plugin: 支持模块生成 API 层目录，生成 APP 运行环境，以及管理模块发布的 Gradle 插件


## 3.1 Modular-core


### 3.1.1 实现模块类

业务模块类需要继承 BaseModule：
``` java
public class ModuleA extends BaseModule {
    
        @Override
        public void onInstalled() {
            registerBusinessService(ModuleAService.class, new CachedServiceFetcher() {
                @Override
                public ModuleAService createService(@NotNull ModularManage manager) {
                    if (service == null) {
                        service = new ModuleAServiceImpl();
                    }
                    return service;
                }
            });
        }
    }
```

### 3.1.2 模块生命周期

模块有以下几个生命周期：

- onInstalled() -> 模块被注册的时候调用：Module 在 App 中被注册的时候
- onCreate()    -> 模块第一次启动的时候调用：Module 所属的某个 Activity 第一次启动的时候
- onStart()     -> 模块启动的时候调用：模块第一次启动之的时候
- onStop()      -> 模块停止的时候调用：Activity 栈里面没有模块所属 Activity 的时候


### 3.1.3 模块生命周期的实现

其实组件内关于生命周期捕获和监听，都是借助于 Google 的 Android Architecture Components 中的 Lifecycle 库来实现的。

- 模块生命周期的捕获：首先需要将 Activity 的类注册到 Module 中，然后全局监听 Activity 的 Lifecycle.Event 事件，就可以获取到模块内 Activity 的运行情况
- 模块生命周期的监听：BaseModule 本身继承了 LifecycleOwner 接口，可以对其添加观察者，来实现对模块生命周期的监听


## 3.2 Modular-plugin

这里需要依赖对于 Android 的构建工具 Gralde 的扩展，它支持的高度可扩展特性，帮助我们在组件化开发中更加高效，不需要关系一些额外的工作，只需要关注开发的内容即可，对现有的代码逻辑基本没有侵入。


### 3.2.1 Gralde 的生命周期

这里必须要提一些的就是 Gradle 的生命周期，因为我们的很多扩展功能，都是在对 Gradle 执行的生命周期的各个阶段做一些改动来实现的，大概的生命周期如图：
![img](https://img.yzcdn.cn/public_files/2019/04/22/cb8a244879420b1e4fab4eddb1806760.jpg)


### 3.2.2 单模块打包

Android 打包成 Apk 并运行的条件有：

- AndroidManifest.xml 的配置支持（application 标签的配置）
- 主 Activity 的配置

#### 实现原理

- 自动生成模块自己的 Application 类
- 自动读取 Module 的 AndroidManifest 文件并修改成可以打包成 App 的配置
- 在打包的时候动态更改 SourceSet，使打包的时候使用生成的文件进行打包
- 在打包的时候动态更改支持的 Plugin 类型（'com.android.application'或是'com.android.library'）

#### 修改模块 build.gradle 的配置
将以下配置添加到模块目录下的 build.gradle 文件中

``` groovy
modular {
        // 模块包名
        packageName = "com.youzan.ebizcore.plugin.demoa"
        
        app {
            // 单模块打包开关
                asApp = true
                // 运行的 App 的名称
                appName = "Module A"
                // 入口 Activity
                launchActivity = "com.youzan.ebizcore.plugin.demoa.ModuleAActivity"
        
                // 配置只在单模块打包时需要引入的依赖
                requires {
                    require "com.squareup.picasso:picasso:2.3.2"
                }
            }
        }
```

#### 生成单模块运行需要的环境

运行 modular 的 createApp Task，就会自动生成需要的类（以 module_a 为例）

``` groovy
    自动生成的文件目录结构：
    ./module_a
        --src
        ----main
        ------app # 自动生成 app 目录
        --------java # 自动生成 Application 类
        --------res # 自动生成资源
        --------AndroidManifest.xml # 自动生成 Manifest 文件
```
        
#### 执行单模块打包并安装的 Task

运行 modular 的 runAsApp Task，模块就会被单独达成一个 apk 包，并安装到你的手机上，如果模块有上下文依赖（比如登录）的话可以额外提供依赖，加到模块的 app 的 requires 中。
这里的打包执行是在 build 目录下生成了一个打包脚本，并调用 Gradle 的 API 执行脚本来实现打包安装的。


### 3.2.3 模块 API 管理

模块 API 层提供的接口和数据结构代码是可以直接在模块内部被引用到的，方便开发，但是在暴露给外部的模块时候的时候是需要打包成 aar 上传到 Maven 来提供的，Modular-Plugin 分别针对这两个步骤提供了两个 Task，方便开发者快速进行开发和发布。

#### 在 build.gralde 中添加相关配置
``` groovy
    modular {
            packageName = "com.youzan.ebizcore.plugin.demoa"
            // 模块 API 支持相关参数
            api {
                // 是否需要提供 API 支持的开关（会影响到是否可以运行自动生成代码的 Task）
                hasApi = true
                // 对外提供的 API Service 类名
                apiService = "ModuleAService"
                // API 层的依赖
                requires {
                    require "com.google.code.gson:gson:2.8.2"
                }
            }
        }
```

#### 生成 API 打包需要的文件
运行 modular 的 createApi Task，就会自动生成需要的类（以 module_b 为例）
``` groovy
    ./module_b
          --src
          ----main
          ------service # 自动生成 service 目录，用来存放对外接口和数据对象
          --------java # 自动生成 Application 类
          --------AndroidManifest.xml # 自动生成 Manifest 文件，为了单独打成 aar 包
```
    
        


### 3.2.4 模块发布

发布功能内部使用了 'maven-publish' 插件来进行依赖的上传，开发者只关心上报的配置就好

#### 在 build.gralde 中添加发布配置
``` groovy
    modular{
            // 模块发布需要的参数
            publish {
                // 是否打开模块发布
                active = true
                // 上报地址，支持本地路径和远程 Mave 仓库地址
                repo = "../release"
                groupId = "com.youzan.ebizmobile.demo"
                artifactId = "modular-a"
                // 上报的业务模块 aar 包的版本号
                moduleVersion = "0.1.4"
                // 上报的 API 层 aar 包的版本号
                apiVersion = "0.1.5"
                // Maven 登录名和密码，可以从 local.properties 中取
                userName = ""
                password = ""
            }
        }
```

#### 执行发布的 Task

运行 modular 的 uploadModule Task，Module-Plugin 会执行打包上传的任务，执行顺序是这样的：
    1. 首先打包并上传 Module 的 API 模块（SourceSet 只包含 API 的类）
    2. 将 Module API 的代码从模块的 SourceSet 中去除，并添加刚才上报的 API 模块的 Maven 依赖到 Module 的 dependencies 中

## 3.3 Modular-support


### 3.3.1 基础组件抽象

以图片组件为例，一般业务模块中使用到的图片相关的功能有：图片加载、图片选择等，可以把这些功能抽象成接口
``` kotlin
    interface IImageLoadSupport {
        fun <IMAGE : ImageView> loadImage(imageView: IMAGE?, imgUrl: String)
        fun <IMAGE : ImageView> loadImage(imageView: IMAGE?, @DrawableRes drawableId: Int)
        fun <IMAGE : ImageView> loadImage(imageView: IMAGE?, imgUrl: String, callback: ImageLoadCallback<IMAGE>)
        fun imagePicker(activity: Activity?, selectedImgUris: List<Uri>)
        fun onImagePickerResult(requestCode: Int, resultCode: Int, intent: Intent?): List<String>?
    }
```

### 3.3.2 基础组件的实现

基础组件的实现可以在 App 中进行注册，如果需要单模块组件中使用 Support 相关功能，可以提供一套默认实现，在但模块运行时引入，在全局有一个 Support 注册中心，以 Map 的形式维护运行中的 Support 对象：
``` kotlin
    fun <SUPPORT : Any, SUPPORTIMPL : SUPPORT> registerProvider(supportCls: Class<SUPPORT>, provider: SupportProvider<SUPPORTIMPL>) {
        synchronized(Lock) {
            supportsProviderMap[supportCls] = provider
            if (supportsMap.containsKey(supportCls)) {
                supportsMap.remove(supportCls)
            }
        }
    }
```

-------------------------------------------------------------------------------

# 四、规划

开发到现在，这边的三个组件已经能够基本完成我们对于组件化核心需求，但是，也是有一些方向可以进一步优化整套方案的使用：

- Modular-Support 组件引入依赖注入的方式实现 API 的调用，使用方可以不再需要关心实例对象的获取
- Modular-Support 组件可以提供给 Weex、RN、H5、Flutter 业务一些原生的功能
- Modular-Plugin 能够进一步压缩打包时间，并且让开发中的依赖配置更加灵活
- Modular-Plugin 继续优化管理和依赖打包的功能，提高效率

-------------------------------------------------------------------------------

# 五、总结

组件化的道路千万条，项目的架构也是在不断得调整优化，来达到提升团队开发效率，保证项目稳定性的目的。以上的这些想法和在实际项目中的一些方案，希望能给正在进行模块化探索的同学提供一些灵感。

