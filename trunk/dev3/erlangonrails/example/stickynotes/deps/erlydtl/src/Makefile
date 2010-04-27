include ../../../support/include.mk

ERLYDTL_PARSER := erlydtl_parser

all: $(ERLYDTL_PARSER).erl $(EBIN_FILES)

# erlc also support "Yecc source code", It generates an .erl file. 
$(ERLYDTL_PARSER).erl: $(ERLYDTL_PARSER).yrl
	$(ERLC) -o . $<

clean:
	rm -rfv $(EBIN_FILES)
	rm -rfv $(ERLYDTL_PARSER).erl