include $(GOROOT)/src/Make.inc

bin/lispin: obj/lispin.$(O)
	$(LD) -o $@ $<

bin/server: obj/server.$(O)
	$(LD) -o $@ $<

obj/lispin.$(O): src/lispin.go
	$(GC) -o $@ $<

obj/server.$(O): src/server.go
	$(GC) -o $@ $<
