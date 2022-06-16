.PHONY: dev_server


build:
	stack build --pedantic


dev_server:
	ghcid --test ":main"
