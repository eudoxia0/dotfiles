all: eudoxia/inform7.el \
	eudoxia/xcompose-mode.el

eudoxia/inform7.el:
	curl -sL -o $@ "https://raw.githubusercontent.com/alexispurslane/inform7-mode/refs/heads/master/inform7.el"

eudoxia/xcompose-mode.el:
	curl -sL -o $@ "https://raw.githubusercontent.com/kragen/xcompose/refs/heads/master/xcompose-mode.el"

clean:
	rm -f eudoxia/inform7.el
	rm -f eudoxia/xcompose-mode.el
