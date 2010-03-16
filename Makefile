## Makefile for ditz-mode installation.

prefix = /usr/local
lispdir = $(prefix)/share/emacs/site-lisp

SRCS = ditz-mode.el

all: install

install: $(SRCS)
	install -c -m 664 $(SRCS) $(lispdir)
