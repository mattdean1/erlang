# An implentation of Tarry's algorithm in Erlang

Compile: `erlc tarry.erl`

Run: `erl -run tarry -run init stop < input.txt`

## Tarry's Algorithm

### Assumption

An undirected network of processes.

### The Algorithm

Initially, an initiator sends out a token.

* Rule 1: A process never forwards the token through the same channel twice.

* Rule 2: A process only forwards the token to its parent when there is no other option.

The token travels through each channel both ways, and finally ends up at the
initiator.
