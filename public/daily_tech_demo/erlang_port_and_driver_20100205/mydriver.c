#include <stdio.h>
#include "/usr/local/lib/erlang/usr/include/erl_driver.h"

// 数据结构 & 函数
// <1> ErlDrvPort
// <2> ErlDrvData
// <3> ErlDrvEntry
//
// <1> driver_output()

// 注意:
// <1> erl_driver.h的存放路径是 
//     /usr/local/lib/erlang/usr/include/erl_driver.h
// <2> 该文件在linux平台上编译成动态链接库

// 需要调用的功能函数:
int twice(int x) {
  return 2 * x;
}
int sum(int x, int y) {
  return x + y;
}


// Port Driver逻辑
typedef struct {
  ErlDrvPort port;
} drv_data;

static ErlDrvData drv_start(ErlDrvPort port, char *buf) {
  drv_data *d = (drv_data*)driver_alloc(sizeof(drv_data));
  d->port = port;
  return (ErlDrvData)d;
}

static void drv_stop(ErlDrvData handle) {
  driver_free((char*)handle);
}

// 核心 - Erlang & C 数据交换方式:
// 该函数响应Erlang通过Port发送过来的数据，进行计算，并把结果发送给Erlang Port.
static void drv_output(ErlDrvData handle, char *buf, int buflen) {
  drv_data *d = (drv_data*)handle;
  char fn = buf[0], res;
	 
  // 实现协议:
  // <1> 假定C++和Erlang中所有的整形都是一个字节
  // <2> Erlang -> C 的第一个字节:
  //     1表示调用twice方法
  //     2表示调用sum方法
  // <3> C- > Erlang 将计算后的结果发送给Erlang Process
  //     只发送一个字节的计算结果

  if(fn == 1) {
    res = twice(buf[1]);
  } else if(fn == 2) {
    res = sum(buf[1], buf[2]);
  }

  // 输出结果
  driver_output(d->port, &res, 1);
}

// 注册一个结构体(名字和start/stop/output三个函数)
ErlDrvEntry my_driver_entry = {
  NULL,
  drv_start,
  drv_stop,
  drv_output,
  NULL,
  NULL,
  "libmydriver", // 和Erlang的open_port中使用的参数一致
  NULL,
  NULL,
  NULL,
  NULL
};

// 返回ErlDrvEntry
DRIVER_INIT(mydriver) { // 和driver_entry中的名字一致
  return &my_driver_entry;
}

