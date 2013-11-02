TARGET       := $(HOME)/.emacs.d
EMACS        := emacs

.PHONY: all install clean

all: clean install

install: clean
				ln -sf $(CURDIR) $(TARGET)
				cd $(TARGET)
				$(TARGET)/install.sh

clean:
				rm -rf $(TARGET)
