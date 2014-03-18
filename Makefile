PROFILING = --enable-library-profiling --enable-executable-profiling
PROFILING =

all:
	make -C golog
	make -C plan-recog
	make -C torcs-agent
	make -C golog-examples
	make -C limited

clean:
	make -C golog clean
	make -C plan-recog clean
	make -C torcs-agent clean
	make -C golog-examples clean

