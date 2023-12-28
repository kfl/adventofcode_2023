.PHONY: all
all: run-sml

run-sml: day01
	time ./day01

day01: day01.sml
	mosmlc $< -o $@
