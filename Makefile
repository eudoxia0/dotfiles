.default: all

st/st:
	make -C st
	rm st/st.o
	rm st/x.o

all: st/st

clean:
	rm st/st
