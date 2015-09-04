all: test

test:
		@cask exec ecukes --reporter magnars

test-no-win:
		@cask exec ecukes --no-win

test-debug:
		@cask exec ecukes --debug

test-install:
		@cask

.PHONY: ecukes test test-no-win test-install all