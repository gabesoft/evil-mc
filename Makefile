all: test

test: test-no-win

test-no-win:
		@cask exec ecukes --no-win

test-win:
		@cask exec ecukes --win

test-debug:
		@cask exec ecukes --debug

test-install:
		@$(RM) -r -f ./.cask/*
		@cask install

.PHONY: ecukes test test-no-win test-install all
