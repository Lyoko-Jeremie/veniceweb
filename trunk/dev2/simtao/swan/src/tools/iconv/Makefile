EBIN=../../../ebin
ERL_USR_INCLUDE=/usr/local/lib/erlang/usr/include
CFLAGS=-I$(ERL_USR_INCLUDE)

ICONV_BEAM=../../../ebin/iconv.beam
ICONV_ERL=./src/iconv.erl

ICONV_SO=../../../ebin/libiconv_drv.so
ICONV_C=./c_src/iconv_drv.c

all: $(ICONV_BEAM) $(ICONV_SO)

$(ICONV_BEAM): $(ICONV_ERL)
	erlc -o $(EBIN) $(ICONV_ERL)

$(ICONV_SO): $(ICONV_C)
	gcc $(CFLAGS) -fpic -shared -o $(ICONV_SO) $(ICONV_C)