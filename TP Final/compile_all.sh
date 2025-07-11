#!/bin/bash
find . -name "*.erl" -exec erlc {} +
erl -noshell -eval "test:start(), init:stop()."
erl -noshell -eval "test5:start(), init:stop()."
erl -noshell -eval "test6:start(), init:stop()."
erl -noshell -eval "test7:start(), init:stop()."
erl -noshell -eval "test8:start(), init:stop()."
