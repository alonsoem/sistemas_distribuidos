#!/bin/bash
find . -name "*.erl" -exec erlc {} +
erl -noshell -eval "test:start(), init:stop()."