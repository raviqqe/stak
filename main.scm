#!/usr/bin/env gsi

(include "./compile.scm")

(write-target (encode (compile (expand (read-source)))))
