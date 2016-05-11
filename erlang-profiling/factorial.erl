-module(factorial).

-compile(export_all).

compute(0) -> 1;
compute(N) when N >= 1 -> N * compute(N-1).
