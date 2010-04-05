EBIN=../../../ebin

MYSQL_BEAM=../../../ebin/mysql.beam
MYSQL_ERL=./src/mysql.erl

MYSQL_AUTH_BEAM=../../../ebin/mysql_auth.beam
MYSQL_AUTH__ERL=./src/mysql_auth.erl

MYSQL_CONN_BEAM=../../../ebin/mysql_conn.beam
MYSQL_CONN_ERL=./src/mysql_conn.erl

MYSQL_RECV_BEAM=../../../ebin/mysql_recv.beam
MYSQL_RECV_ERL=./src/mysql_recv.erl

MYSQL_WRAP_BEAM=../../../ebin/mysql_wrap.beam
MYSQL_WRAP_ERL=./src/mysql_wrap.erl


all: $(MYSQL_BEAM) $(MYSQL_AUTH_BEAM) $(MYSQL_CONN_BEAM) $(MYSQL_RECV_BEAM) $(MYSQL_WRAP_BEAM)

$(MYSQL_BEAM): $(MYSQL_ERL)
	erlc -o $(EBIN) $(MYSQL_ERL)

$(MYSQL_AUTH_BEAM): $(MYSQL_AUTH_ERL)
	erlc -o $(EBIN) $(MYSQL_AUTH_ERL)

$(MYSQL_CONN_BEAM): $(MYSQL_CONN_ERL)
	erlc -o $(EBIN) $(MYSQL_CONN_ERL)

$(MYSQL_RECV_BEAM): $(MYSQL_RECV_ERL)
	erlc -o $(EBIN) $(MYSQL_RECV_ERL)

$(MYSQL_WRAP_BEAM): $(MYSQL_WRAP_ERL)
	erlc -o $(EBIN) $(MYSQL_WRAP_ERL)

