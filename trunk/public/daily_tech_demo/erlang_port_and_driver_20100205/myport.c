#include <stdio.h>

// 处理协议:
// <1> 所有数据包的开头都以2个字节作为数据长度
// <2> Erlang -> C 
//     1表示twice的调用, 2表示sum的调用
//     
//     如果要调用twice(5)
//     会发送下列数据(byte)到C程序: 0, 2, 1, 5
//     如果调用sum(7, 8)
//     会发送下列数据(byte)到C程序: 0, 3, 2, 7, 8
// <3> C- > Erlang 
//     将计算后的结果发送给Erlang Process只发送"一个字节"的计算结果

typedef unsigned char byte;
int read_cmd(byte* buf);
int write_cmd(byte* buf, int len);
int read_exact(byte* buf, int len);
int write_exact(byte* buf, int len);


// 供Erlang调用的"数学库函数"
int twice(int x);
int sum(int x, int y);

int main() {
  int fn, result;
  byte buf[100];

  // 读取数据, 进行计算
  while(read_cmd(buf) > 0) {
    fn = buf[0];
    if(fn == 1) {
      result = twice(buf[1]);
    } else if (fn == 2) {
      result = sum(buf[1], buf[2]);
    } 

    buf[0] = result;
    write_cmd(buf, 1);
  }
  return 0;
}

int read_cmd(byte *buf) {
  int len;
  if(read_exact(buf, 2) != 2)
    return -1;

  // 两个字节的数据长度, 先接收高位byte， 后接收低位byte.
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len) {
	byte li;

        // 两个字节的数据长度, 现写高位byte, 后写低位byte.
	li = (len >> 8) && 0xff;
	write_exact(&li, 1);

	li = len & 0xff;
	write_exact(&li, 1);

	return write_exact(buf, len);
}

// 核心 - Erlang & C 数据交换方式:
// 通过stdin和stdout来交换数据, 下面两个函数可以精确的读取数据.

// 从标准输入(stdin)读取@len的数据到buf
// 返回读取数据的长度
int read_exact(byte *buf, int len) {
  int i, got = 0;
  do {
    // 从标准IO读取数据
    // stdin
    if((i = read(0, buf + got, len - got)) <= 0)
      return i;
      got += i;
  } while(got < len);

  return len;
}

// 从buf中写@len长度的数据到标准输出(stdout)
// 返回写入数据的长度
int write_exact(byte *buf, int len) {
  int i, wrote = 0;

  do {
    // 向标准IO写入数据
    // stdout
    if((i = write(1, buf + wrote, len - wrote)) <= 0)
      return i;
      wrote += i;
    } while(wrote < len);

  return len;
}

int twice(int x) {
  return 2 * x;
}

int sum(int x, int y) {
  return x + y;
}
