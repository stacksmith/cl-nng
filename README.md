# cl-nng

WORK IN PROGRESS!!!

Bindings for [NNG library](https://github.com/nanomsg/nng), the next generation of nanomsg, which in turn is designed to replace ZeroMQ.

NNG, like its predecessors nanomsg (and to some extent ZeroMQ), is a lightweight, broker-less library, offering a simple API to solve common recurring messaging problems, such as publish/subscribe, RPC-style request/reply, or service discovery. The API frees the programmer from worrying about details like connection management, retries, and other common considerations, so that they can focus on the application instead of the plumbing.

## License 

MIT license, as is the NNG library itself.

## Status

Early WIP; bare minimum for http example implemented.

## Lispy interface

### (WITH-SOCKET ((socket-name protocol) &body body))

Evaluate body in an environment containing an open socket.  Protocol is req0, rep0, etc.

### (RECV-ON ((socket &optional result buffer bufsize) &body body)

Evaluate body in an environment that allocates a receive buffer and cleans up on exit.  Socket must be an open socket handle; optional parameters are names, defaulting to result buffer and bufsize.

### (SEND-STRING (socket string))

Send a lisp string

## Implementation notes

Swig was used initially to generate nng-cffi raw bindings.  The file was heavily edited to fix conversion problems. 

* NNG-CFFI - raw cffi bindings; nng_xxx_yyy named as nng::%xxx-yyy
* NNG-HTTP - http extensions
* NNG-MAIN - a thin wrapper, stack-allocated temporaries for allocators; xxx-yyy


##Quickstart:


